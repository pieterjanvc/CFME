#' Parse data from a combined format and insert it into the database
#'
#' @param combined_data Dataframe of the original data
#' @param dbInfo connection or path to an SQLite database
#'
#' @import dplyr
#' @import RSQLite
#' @import sqlife
#' @importFrom stringr str_replace_all
#'
#' @returns TRUE if success
#' @export
#'
dbAddEvaluations <- function(combined_data, dbInfo) {
  # Lowercase for all columnnames
  colnames(combined_data) <- str_replace_all(
    tolower(colnames(combined_data)),
    "\\s+",
    "_"
  )
  # Edit data types
  combined_data <- lapply(combined_data, function(x) {
    if (class(x)[1] == "numeric") {
      as.integer(x)
    } else {
      as.character(x)
    }
  }) |>
    as.data.frame() |>
    rename(original_evaluator_id = evaluator_id)

  data <- combined_data

  # Create / access the database
  schema <- system.file("extdata", "cfme.sql", package = "CFME")

  if (schema == "") {
    schema <- "inst/cfme.sql"
  }

  result <- dbSetup(dbInfo, schema, validateSchema = T)
  conn <- dbGetConn(dbInfo)

  # --- Insert student data
  student <- data |>
    select(
      learner_anon_id,
      pce_assign,
      society,
      acad_prog,
      acad_prog_trk,
      gender,
      urim_flg,
      age
    ) |>
    distinct()

  student <- tbl_insert(student, conn, "student", commit = F)

  # Add the new student ID to the data
  data <- data |>
    left_join(
      student |> select(learner_anon_id, student_id = id),
      by = "learner_anon_id"
    )

  # --- Insert evaluator data
  #  Given their title can change over time, the evaluator_id is NOT unique
  evaluator <- data |>
    select(
      original_evaluator_id,
      evaluator,
      acad_title
    ) |>
    distinct()

  evaluator <- tbl_insert(evaluator, conn, "evaluator", commit = F)

  # Add the new evaluator ID to the data
  data <- data |>
    left_join(
      evaluator |> select(original_evaluator_id, acad_title, evaluator_id = id),
      by = c("original_evaluator_id", "acad_title")
    )

  # --- Insert clerkship data
  clerkship <- data |>
    select(
      clerkship,
      location
    ) |>
    distinct()

  clerkship <- tbl_insert(clerkship, conn, "clerkship", commit = F)

  # Add the new clerkship ID to the data
  data <- data |>
    left_join(
      clerkship |> select(clerkship, clerkship_id = id),
      by = c("clerkship")
    )

  # --- Insert rotation data
  rotation <- data |>
    select(
      student_id,
      clerkship_id,
      rotation_date,
      first_nbme_score,
    ) |>
    distinct()

  check <- rotation |> group_by(student_id, clerkship_id) |> filter(n() > 1)
  if (nrow(check) > 0) {
    head(check)
    stop("Rotations are not unique")
  }

  rotation <- tbl_insert(rotation, conn, "rotation", commit = F)

  # Add the new rotation ID to the data
  data <- data |>
    left_join(
      rotation |> select(student_id, clerkship_id, rotation_id = id),
      by = c("student_id", "clerkship_id")
    )

  # --- Insert evaluation data
  evaluation <- data |>
    group_by(
      rotation_id,
      evaluator_id,
      summary_flg,
      acad_yr
    ) |>
    mutate(
      complete = case_when(
        summary_flg[1] == "Y" & n() > 3 ~ 1,
        summary_flg[1] == "N" & n() > 2 ~ 1,
        TRUE ~ 0
      )
    ) |>
    ungroup() |>
    select(rotation_id, evaluator_id, summary_flg, acad_yr, complete) |>
    distinct() |>
    mutate(summary_flg = ifelse(summary_flg == "Y", 1, 0))

  check <- evaluation |>
    group_by(rotation_id, evaluator_id, summary_flg) |>
    filter(n() > 1)
  if (nrow(check) > 0) {
    head(check)
    stop("Evaluations are not unique")
  }

  evaluation <- tbl_insert(evaluation, conn, "evaluation", commit = F)

  # Add the new evaluation ID to the data
  data <- data |>
    left_join(
      evaluation |>
        select(rotation_id, evaluator_id, evaluation_id = id, summary_flg) |>
        mutate(summary_flg = ifelse(summary_flg == 1, "Y", "N")),
      by = c("rotation_id", "evaluator_id", "summary_flg")
    )

  # --- Insert question data
  question <- data |>
    select(
      question
    ) |>
    distinct()

  question <- tbl_insert(question, conn, "question", commit = F)

  # Add the new question ID to the data
  data <- data |>
    left_join(
      question |> select(question, question_id = id),
      by = c("question")
    )

  # --- Insert answer data
  answer <- data |>
    select(
      question_id,
      evaluation_id,
      submission_date,
      answer_txt,
      answer_txt_redacted,
      rowid
    ) |>
    distinct()

  answer <- tbl_insert(answer, conn, "answer", commit = F)

  # --- SANITY CHECK

  # Rejoin all data
  check <- answer |>
    left_join(question, by = c("question_id" = "id")) |>
    left_join(evaluation, by = c("evaluation_id" = "id")) |>
    left_join(rotation, by = c("rotation_id" = "id")) |>
    left_join(evaluator, by = c("evaluator_id" = "id")) |>
    left_join(clerkship, by = c("clerkship_id" = "id")) |>
    left_join(student, by = c("student_id" = "id")) |>
    mutate(summary_flg = ifelse(summary_flg == 1, "Y", "N"))

  # Get the same columns as the original
  colIdx <- sapply(
    colnames(combined_data),
    function(x) {
      which(x == colnames(check))
    },
    USE.NAMES = F
  ) |>
    unlist()

  check <- check[, colIdx] |> arrange(rowid)

  # Check number of rows
  if (nrow(check) != nrow(combined_data)) {
    dbRollback(conn)
    if (is.character(dbInfo)) {
      dbFinish(conn)
    }
    stop(
      "Something went wrong and the processed data ",
      "does not have the same number of rows as the original"
    )
  }

  #Check if data matches
  if (!all(check == combined_data, na.rm = T)) {
    dbRollback(conn)
    if (is.character(dbInfo)) {
      dbFinish(conn)
    }
    stop(
      "Something went wrong and the processed data does not match the original"
    )
  }

  missingVals <- check[!complete.cases(check), ]

  if (nrow(missingVals) > 0) {
    warning(
      "The following rowid have missing values: ",
      paste(missingVals$rowid, collapse = ", ")
    )
  }

  if (is.character(dbInfo)) {
    dbFinish(conn)
  }

  return(T)
}

#' Get the evaluation text from the database
#'
#' @param ids A vector of evaluation IDs to retrieve text for
#' @param dbInfo A DB connection or path
#' @param redacted (Default = TRUE) Show redacted text
#' @param includeQuestions (Default = TRUE) Add the questions to the text
#' @param html (Default = FALSE) Output HTML instead of plain text
#'
#' @import dplyr
#' @importFrom stringr str_trim
#'
#' @returns A data frame with a text summary for each evaluation
#' @export
dbGetEvals <- function(
  ids,
  dbInfo,
  redacted = T,
  includeQuestions = T,
  html = F
) {
  conn <- dbGetConn(dbInfo)
  evals <- tbl(conn, "answer") |>
    inner_join(
      tbl(conn, "evaluation") |>
        filter(id %in% {{ ids }}) |>
        select(id, rotation_id, summary_flg, complete),
      by = c("evaluation_id" = "id")
    ) |>
    left_join(tbl(conn, "question"), by = c("question_id" = "id")) |>
    left_join(
      tbl(conn, "rotation") |> select(id, clerkship_id),
      by = c("rotation_id" = "id")
    ) |>
    left_join(tbl(conn, "clerkship"), by = c("clerkship_id" = "id")) |>
    collect() |>
    mutate(
      answer = if (redacted) {
        answer_txt_redacted
      } else {
        answer_txt
      },
      answer = if (html) {
        str_replace_all(str_trim(answer), "\n", "<br>")
      } else {
        str_trim(answer)
      }
    )

  evals <- evals |>
    group_by(evaluation_id) |>
    summarise(
      summary = summary_flg[1] == 1,
      complete = complete[1] == 1,
      clerkship = clerkship[1],
      evaluation = paste(
        if (includeQuestions) {
          paste(
            ifelse(html, "<h3>", "---"),
            question,
            ifelse(html, "</h3>", "\n")
          )
        },
        answer,
        sep = "",
        collapse = ifelse(html, "<br>", "\n\n")
      ),
      .groups = "drop"
    )

  if (is.character(dbInfo)) {
    dbFinish(conn)
  }

  return(evals)
}

#' Add a new prompt to the database
#'
#' @param prompt Single string of system prompt text
#' @param dbInfo DB connection info
#' @param note (Optional) Note about this prompt
#' @param showWarning (Default = TRUE) Show warning if prompt already exists
#'
#' @import dplyr
#' @importFrom rlang hash
#'
#' @returns Prompt ID
#' @export
#'
dbAddPrompt <- function(prompt, dbInfo, note, showWarning = T) {
  # Check if the prompt already exists
  prompt_hash <- hash(prompt)
  conn <- dbGetConn(dbInfo)
  promptID <- tbl(conn, "review_prompt") |>
    filter(hash == local(prompt_hash)) |>
    pull(id)

  # Add new prompt if needed
  if (length(promptID) == 0) {
    toInsert <- data.frame(
      hash = prompt_hash,
      prompt = prompt
    )

    if (!missing(note)) {
      toInsert$note = note
    }

    promptID <- tbl_insert(toInsert, conn, "review_prompt") |> pull(id)
  } else if (showWarning) {
    warning("The provided prompt already is in the database")
  }

  if (is.character(dbInfo)) {
    dbFinish(conn)
  }

  return(promptID)
}


#' Add an LLM response from llm_review() to the database
#'
#' @param dbInfo Database info
#' @param llm_review Output of the llm_review() function
#'
#' @import dplyr
#' @import sqlife
#'
#' @returns A data frame with the following columns:
#' - evaluation_id,
#' - review_response_id,
#' - review_score_id: the ID for the each detected competency review scores
#' @export
#'
dbAddLLMreview <- function(dbInfo, llm_review) {
  conn <- dbGetConn(dbInfo)

  # Check if not already in database
  check <- tbl(conn, "review_score") |>
    filter(review_response_id == llm_review$review_response_id) |>
    pull(review_response_id)

  if (length(check) > 0) {
    warning(
      "The llm evaluation with review_response_id ",
      check,
      "is already in the database"
    )

    if (class(dbInfo) == "character") {
      dbFinish(conn)
    }

    return(check)
  }

  # Add the evaluation data
  data <- llm_review$data
  data$review_response_id = llm_review$review_response_id
  data <- data |>
    rename(
      competency_id = cID,
      specificity = spec,
      utility = util,
      sentiment = sent,
      text_matches = text
    )

  review_score_id <- tbl_insert(data, conn, "review_score", commit = T) |>
    pull(id)

  # Finsh and return
  if (class(dbInfo) == "character") {
    dbFinish(conn)
  }

  return(data.frame(
    evaluation_id = llm_review$evaluation_id,
    review_response_id = llm_review$review_response_id,
    review_score_id = review_score_id
  ))
}
