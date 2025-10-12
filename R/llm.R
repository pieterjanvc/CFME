#' Check if the LLM response is in the expected CSV format
#'
#' @param string To parse as CSV
#' @param expColnames (Optional) Named list of column names / type to expect.
#'  Defaults:
#' "cID" = "integer",
#' "text" = "character"
#' "spec" = "integer"
#' "utility" = "integer"
#' "sent" = "integer"
#'
#' @returns List with two elements
#'  - statusCode: 0 not valid CSV, 1 column names / number issue,
#'     2 column type error, 3 expected CSV format
#'  - data: Data frame with results for statusCode 3, NULL otherwise
#' @export
#'
llm_csv_response <- function(string, expColnames) {
  if (missing(expColnames)) {
    expColnames <- c(
      "cID" = "integer",
      "text" = "character",
      "spec" = "integer",
      "util" = "integer",
      "sent" = "integer"
    )
  }

  suppressWarnings({
    tryCatch(
      {
        # Try and load as CSV
        check <- read.csv(textConnection(string), fill = F)

        # Check number of columns and names
        if (!all(colnames(check) == names(expColnames))) {
          return(list(statusCode = 1, data = NULL))
        }

        # Check column type
        if (!all(sapply(check, class) == expColnames)) {
          return(list(statusCode = 2, data = NULL))
        }

        return(list(statusCode = 3, data = check))
      },
      error = function(e) {
        return(list(statusCode = 0, data = NULL))
      }
    )
  })
}

#' Call an Azure LLM
#'
#' Note that this function expects an environment variable HMS_AZURE_API
#' that contains the API key You can set this up using
#' `Sys.setenv(HMS_AZURE_API = "API token here")`
#'
#' @param user User prompt
#' @param system Default = "You are a helpful AI assistant". System prompt
#' @param model Default = gpt-4o-1120. LLM model to use
#' @param maxTokens Default = 500. Max Number of tokens to return
#' @param version Default = 2024-10-21.
#' @param endpoint Default = https://azure-ai.hms.edu. Azure endpoint
#' @param log If set, the token usage is kept track of in this CSV file
#'
#' @import httr2
#'
#' @returns LLM Response object (list).
#'
#' If a log file is et a line is added to a CSV file with columns
#' timestamp, promptTokens and responseTokens (headers not included)
#'
#' @export
#'
llm_call <- function(user, system, model, maxTokens, version, endpoint, log) {
  # API info https://learn.microsoft.com/en-us/azure/ai-foundry/openai/reference

  # Model info
  endpoint <- ifelse(missing(endpoint), "https://azure-ai.hms.edu", endpoint)
  version <- ifelse(missing(version), "2024-10-21", version)
  model <- ifelse(missing(model), "gpt-4o-1120", model)

  # Build the URL
  baseURL <- sprintf(
    "%s/openai/deployments/%s/chat/completions?api-version=%s",
    endpoint,
    model,
    version
  )

  # Build the body
  system <- ifelse(missing(system), "You are a helpful AI assistant", system)
  maxTokens <- ifelse(missing(maxTokens), 500, maxTokens)
  body <- list(
    messages = list(
      list(role = "system", content = system),
      list(role = "user", content = user)
    ),
    max_tokens = maxTokens
  )

  # Send the request
  req <- request(baseURL) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_perform()

  if (resp_status(req) != 200) {
    stop(req)
  }

  resp <- resp_body_json(req)

  # Write token usage to log file if set
  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$prompt_tokens,
        resp$usage$completion_tokens
      ),
      log,
      append = T
    )
  }

  return(resp)
}

#' Get the data from an LLM evaluation
#'
#' @param dbInfo Database info
#' @param review_prompt_id System prompt to use for LLM
#' @param evaluation_id Evlaution to use
#' @param log (Optional) Save token usage to extra file (will also be in database)
#' @param include_questions (Default = T) Include questions in prompt
#' @param redacted (Default = T) Use redacted data
#' @param maxTries (Default = 3) How many times to try in case the response is
#' not in a valid format
#'
#' @returns A list with 4 elements
#' - statusCode = final status code (3 = success)
#' - review_response_id: ID from review_response table in database
#' - data: If successful, data frame with response, otherwise NULL
#' - tries: Number of times tried for valid response
#' @export
#'
llm_review <- function(
  dbInfo,
  review_prompt_id,
  evaluation_id,
  model,
  log,
  include_questions = T,
  redacted = T,
  maxTries = 3
) {
  conn <- dbGetConn(dbInfo)
  prompt <- tbl(conn, "review_prompt") |>
    filter(id == review_prompt_id) |>
    pull(prompt)

  if (length(prompt) == 0) {
    stop(
      "There is no system prompt for review_prompt_id ",
      review_prompt_id,
      " in the database"
    )
  }

  evals <- dbGetEvals(
    evaluation_id,
    dbInfo = conn,
    includeQuestions = include_questions
  )

  if (nrow(evals) == 0) {
    stop(
      "No evaluations found with evaluation_id ",
      paste(evaluation_id, collapse = ",")
    )
  } else if (nrow(evals) == 0) {
    warning(
      "For now only a single review is evaluated per function call. ",
      "Others were ignored"
    )
  }

  for (i in 1:maxTries) {
    # Actual LLM call
    tryCatch(
      {
        duration <- Sys.time()
        result <- llm_call(
          user = evals$evaluation[1],
          system = prompt,
          model = model,
          log = log
        )
        duration <- difftime(Sys.time(), duration, units = "sec") |>
          as.numeric() |>
          round(2)
      },
      error = function(e) {
        if (is.character(dbInfo)) {
          dbFinish(conn, commit = F)
        }
        stop(e)
      }
    )

    # Check if the model is already in the database as a reviewer otherwise add it
    reviewer_id <- tbl(conn, "reviewer") |>
      filter(model == result$model) |>
      pull(id)

    if (length(reviewer_id) == 0) {
      reviewer <- data.frame(
        human = 0,
        model = result$model
      )
      reviewer_id <- tbl_insert(reviewer, dbInfo, "reviewer") |> pull(id)
    }

    check <- llm_csv_response(result$choices[[1]]$message$content)

    # Add the response metadata
    review_response <- data.frame(
      evaluation_id = evaluation_id,
      review_prompt_id = review_prompt_id,
      reviewer_id = reviewer_id,
      include_questions = include_questions,
      redacted = redacted,
      statusCode = check$statusCode,
      tokens_in = result$usage$prompt_tokens,
      tokens_out = result$usage$completion_tokens,
      duration = duration
    )

    review_response_id <- tbl_insert(
      review_response,
      conn,
      "review_response",
      commit = T
    ) |>
      pull(id)

    if (check$statusCode == 3) {
      break
    }
  }

  if (is.character(dbInfo)) {
    dbFinish(conn)
  }

  return(list(
    statusCode = check$statusCode,
    evaluation_id = evaluation_id,
    review_response_id = review_response_id,
    data = check$data,
    tries = i
  ))
}
