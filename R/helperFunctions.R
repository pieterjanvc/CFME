#' Check if a prompt is structured correctly and returned a parsed version
#'
#' @param prompt String of text to check
#'
#' @returns list with
#' - success: TRUE / FALSE
#' - msg: message
#' - content: list containing parsed prompt if successful
#'
#' @import stringr
#'
#' @export
#'
parsePrompt <- function(prompt) {
  sections <- str_split(prompt, "(?m)^#[^#]")[[1]][-1]
  #Check if there are 3 major sections (Task, rubric, to return)
  if (length(sections) != 3) {
    return(list(
      success = F,
      msg = paste(
        "The prompt does not have the 3 expected sections (#):",
        " task, rubric, to return"
      ),
      content = NULL
    ))
  }

  task <- str_split(sections[1], "\\n", n = 2)[[1]][-1] |> str_trim()

  # Competencies
  rubric <- str_split(sections[2], "(?m)^##[^#]")[[1]][-1]

  competencies <- str_split(rubric[1], "(?m)^###\\s\\d.\\s")[[1]][-1] |>
    str_split("\n", n = 2)

  if (length(competencies) != 6) {
    return(list(
      success = F,
      msg = "Cannot find 6 competencies in the prompt",
      content = NULL
    ))
  }

  competencies <- lapply(competencies, function(competency) {
    list(
      name = competency[1] |> str_trim(),
      description = competency[2] |> str_trim()
    )
  }) |>
    setNames(1:6)

  # Competency Scoring
  compScore <- str_split(rubric[2], "(?m)^###[^#]")[[1]][-1] |>
    str_split("\\:\\s?", n = 2)

  compScore <- setNames(compScore, sapply(compScore, "[[", 1))

  compScore <- lapply(compScore, function(x) {
    x <- str_split(x[[2]], "\n\\-\\s?")[[1]] |> str_trim()
    list(desciption = x[1], options = x[-1])
  })

  # Overall Scoring
  overallScore <- str_split(rubric[3], "(?m)^###[^#]")[[1]][-1] |>
    str_split("\\:\\s?", n = 2)

  overallScore <- setNames(overallScore, sapply(overallScore, "[[", 1))

  overallScore <- lapply(overallScore, function(x) {
    x <- str_split(x[[2]], "\n\\-\\s?")[[1]] |> str_trim()
    list(desciption = x[1], options = x[-1])
  })

  retrunMsg <- str_split(sections[3], "\\n", n = 2)[[1]][-1] |> str_trim()

  return(list(
    success = T,
    msg = "Prompt data successfully parsed",
    content = list(
      task = task,
      competencies = competencies,
      compScore = compScore,
      overallScore = overallScore,
      retrunMsg = retrunMsg
    )
  ))
}

#' Provide missing values if variable does not exist
#'
#' @param var Variabe to check
#' @param useNull (Default = F). Return NA if FALSE else NULL
#' @param n (Default = 1) How may times to repeat NA
#'
#' @returns A vector of values, NAs or NULL depending on settings
#'
missingVal <- function(var, useNull = F, n = 1) {
  if (!missing(var)) {
    var
  } else if (useNull) {
    NULL
  } else {
    rep(NA, n)
  }
}


#' Get the set (function) arguments of the current environment
#'
#' This is useful at the start of a function to capture all passed arguments
#'
#' @returns A list with the set function arguments and their values
#'
getFunArgs <- function(exclude) {
  x <- as.list(parent.frame())
  x <- x[!names(x) %in% exclude]
  x <- x[sapply(x, function(x) typeof(x) != "symbol")]
  if (length(x) == 0) {
    NULL
  } else {
    x
  }
}


#' Delop Shiny App
#'
#' @param db Database to use
#' @param gitHubBranch CFME branch
#' @param dev Deploy to dev app
#'
#' @import shiny DT bslib
#' @importFrom tidyr pivot_wider
#'
#' @returns Nothing
#'
deployShinyApp <- function(db, gitHubBranch, dev = F) {
  root <- ifelse(dev, "deploy/CFME-dev", "deploy/CFME")
  # Copy files
  dir.create(root, showWarnings = F)
  file.copy("inst/review_app.R", file.path(root, "app.R"), overwrite = T)
  file.copy("renv.lock", file.path(root, "renv.lock"), overwrite = T)
  file.copy(db, file.path(root, "cfme.db"), overwrite = T)
  devtools::install_github(paste0("pieterjanvc/CFME@", gitHubBranch))
  # Add CFME to lock file
  renv::record(
    paste0("pieterjanvc/CFME@", gitHubBranch),
    lockfile = file.path(root, "renv.lock")
  )
}

#' Backup and replace the DB using pins
#'
#' @param password Admin password, set `adminPass` as an environment variable
#' @param dbPath Path to the DB
#' @param exportPin (Default = "cfme_db_export") Pin name for the export / backup DB
#' @param importPin (Default = "cfme_db_import") Pin name for the import DB
#' @param nBackups (Default = 3) N most recent exports to keep
#'
#' @import pins
#' @importFrom sqlife dbIsSQLite
#'
#' @returns list with success an msg
#' @export
#'
pinDB <- function(
  password,
  dbPath,
  exportPin = "cfme_db_export",
  importPin = "cfme_db_import",
  nBackups = 3
) {
  if (Sys.getenv("adminPass") == "" || Sys.getenv("adminPass") != password) {
    return(list(success = F, msg = "Password incorrect or not activated"))
  }

  if (!dbIsSQLite(dbPath)) {
    return(list(success = F, msg = "Database file not found"))
  }

  tryCatch(
    {
      board <- board_connect()
      browser()
      # Backup the existing DB (export it)
      pin_upload(board, dbPath, exportPin)
      pin_versions_prune(
        board,
        paste0(board$account, "/", exportPin),
        n = nBackups
      )
      # Import the latest upload and replace it locally
      new <- pin_download(board, paste0(board$account, "/", importPin))

      if (!dbIsSQLite(new)) {
        stop("Import file not a valid database")
      }

      file.copy(new, dbPath, overwrite = T)
    },
    error = function(e) {
      return(list(success = F, msg = e))
    }
  )

  # pin_versions(board, "cfme_db_import")
  # recentEdit <- file.info(dbPath)$mtime >
  #   pin_meta(board, paste0(board$account, "/cfme_db_export"))$created

  return(list(success = T, msg = "Database refreshed"))
}
