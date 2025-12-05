#' Parse data from a combined format and insert it into the database
#'
#' @param combined_data Dataframe of the original data
#' @param dbPath Path to a (new) CFME database
#' @param redactedOnly (Default = FALSE) If TRUE, only redacted evlaluations are
#' put into the database the version with identifiers is omitted
#'
#' @import dplyr
#' @import RSQLite
#' @import sqlife
#' @importFrom stringr str_replace_all
#'
#' @returns TRUE if success
#' @export
#'
dbAddEvaluations <- function(combined_data, dbPath, redactedOnly = F) {
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

  result <- dbSetup(dbPath, schema, validateSchema = T)
  conn <- dbGetConn(dbPath)

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
      if (redactedOnly) {
        NULL
      } else {
        "answer_txt"
      },
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
    stop(
      "Something went wrong and the processed data ",
      "does not have the same number of rows as the original"
    )
  }

  #Check if data matches
  if (!all(check == combined_data, na.rm = T)) {
    stop(
      "Something went wrong and the processed data does not match the original"
    )
  }

  if (redactedOnly) {
    check <- check |> select(-answer_txt)
  }

  missingVals <- check[!complete.cases(check), ]

  if (nrow(missingVals) > 0) {
    warning(
      "The following rowid have missing values: ",
      paste(missingVals$rowid, collapse = ", ")
    )
  }

  dbFinish(conn)

  return(T)
}

#' Get the evaluation text from the database
#'
#' @param ids A vector of evaluation IDs to retrieve text for
#' @param conn CFME database connection
#' @param redacted (Default = TRUE) Show redacted text.
#' Can also be a vector of length ids
#' @param includeQuestions (Default = TRUE) Add the questions to the text.
#' Can also be a vector of length ids
#' @param html (Default = FALSE) Output HTML instead of plain text.
#' Can also be a vector of length ids
#' @param subtitleTag (Default = "h3") In case of HTML = T which tag to use for
#' questions (i.e. subtitle)
#'
#' @import dplyr
#' @importFrom stringr str_trim
#'
#' @returns A data frame with a text summary for each evaluation
#' @export
dbGetEvals <- function(
  ids,
  conn,
  redacted = T,
  includeQuestions = T,
  html = F,
  subtitleTag = "h3"
) {
  toFilter <- ids
  evals <- tbl(conn, "answer") |>
    inner_join(
      tbl(conn, "evaluation") |>
        filter(id %in% toFilter) |>
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
    left_join(
      data.frame(
        evaluation_id = ids,
        redacted = redacted,
        includeQuestions = includeQuestions,
        html = html,
        subtitleTag = subtitleTag
      ),
      by = "evaluation_id"
    ) |>
    mutate(
      # Choose redacted or full
      text = ifelse(redacted, answer_txt_redacted, answer_txt),
      # Clean up whitespace
      text = ifelse(
        html,
        str_replace_all(str_trim(text), "\n", "<br>"),
        str_trim(text)
      ),
      # Add questions if needed
      text = ifelse(
        includeQuestions,
        paste0(
          ifelse(html, sprintf("<%s>", subtitleTag), "---"),
          question,
          ifelse(html, sprintf("</%s><br>", subtitleTag), "\n"),
          text
        ),
        text
      )
    )

  evals <- evals |>
    group_by(evaluation_id) |>
    arrange(question_id) |>
    summarise(
      summary = summary_flg[1] == 1,
      complete = complete[1] == 1,
      clerkship = clerkship[1],
      evaluation = paste(
        text,
        sep = "",
        collapse = ifelse(html, "<br><br>", "\n\n")
      ),
      .groups = "drop"
    )
  return(evals)
}

#' Add a new prompt to the database
#'
#' @param prompt Single string of system prompt text
#' @param conn CFME database connection
#' @param note (Optional) Note about this prompt
#' @param commit (Default = TRUE) Commit the transaction
#' @param showWarning (Default = TRUE) Show warning if prompt already exists
#'
#' @import dplyr
#' @importFrom rlang hash
#'
#' @returns Prompt ID
#' @export
#'
dbAddPrompt <- function(prompt, conn, note, commit = T, showWarning = T) {
  # Check if the prompt already exists
  prompt_hash <- hash(prompt)
  promptID <- tbl(conn, "review_prompt") |>
    filter(hash == local(prompt_hash)) |>
    pull(id)

  # Add new prompt if needed
  if (length(promptID) == 0) {
    parsed <- parsePrompt(prompt)
    if (!parsed$success) {
      dbFinish(conn, error = parsed$msg)
    }

    toInsert <- data.frame(
      hash = prompt_hash,
      prompt = prompt
    )

    if (!missing(note)) {
      toInsert$note = note
    }

    promptID <- tbl_insert(toInsert, conn, "review_prompt", commit = commit) |>
      pull(id)
  } else if (showWarning) {
    warning("The provided prompt already is in the database")
  }

  return(promptID)
}

#' Insert or update into review score table
#'
#' @param conn CFME database connection
#' @param scores Data frame with scores
#' @param reviewStatus (Optional) Set the review status
#' @param commit (Default = TRUE) Commit the transaction
#'
#' @import sqlife dplyr
#'
#' @returns Data frame with changed reviews
#' @export
#'
dbReviewScore <- function(conn, scores, reviewStatus, commit = T) {
  cols <- colnames(scores)
  if ("id" %in% cols) {
    # Update
    result <- tbl_update(scores, conn, "review_score", commit = F)
  } else {
    result <- tbl_insert(scores, conn, "review_score", commit = F)
  }

  # Update the review statusCode if set
  if (!missing(reviewStatus)) {
    data <- scores |>
      select(id = review_assignment_id) |>
      distinct() |>
      mutate(statusCode = reviewStatus)
    . <- tbl_update(data, conn, "review_assignment", commit = F)
  }

  if (commit) {
    dbCommit(conn)
  }

  return(result)
}

#' Add an AI response from llm_review() to the database
#'
#' @param conn CFME database connection
#' @param llmReview Output of the llm_review() function
#' @param commit (Default = TRUE) Commit the transaction
#'
#' @import dplyr
#' @import sqlife
#'
#' @returns A data frame with the following columns:
#' - evaluation_id,
#' - review_assignment_id,
#' - review_score_id: the ID for the each detected competency review scores
#' @export
#'
dbAIreview <- function(conn, llmReview, commit = T) {
  # Check which ones were a success
  success <- sapply(llmReview, "[[", "statusCode") == 3

  # Xombine data to be inserterd into database
  overallScores <- do.call(
    rbind,
    lapply(llmReview[success], function(x) {
      x$data$overallScores |>
        mutate(
          tokens_in = x$tokens_in,
          tokens_out = x$tokens_out,
          duration = x$duration,
          note = ifelse(x$tries == 1, NA, paste(x$tries, "tries"))
        )
    })
  ) |>
    rename(
      utility = util,
      sentiment = sent
    )

  compScores <- do.call(
    rbind,
    lapply(llmReview[success], "[[", c("data", "compScores"))
  ) |>
    rename(
      review_assignment_id = id,
      competency_id = cID,
      specificity = spec
    )

  compText <- do.call(
    rbind,
    lapply(llmReview[success], "[[", c("data", "compText"))
  ) |>
    rename(
      review_assignment_id = id,
      competency_id = cID,
    )

  # Delete existing results
  #  competency_text has a cascading delete so will clean up automatically
  existing <- tbl(conn, "competency_score") |>
    filter(
      review_assignment_id %in% local(overallScores$id)
    ) |>
    select(id, review_assignment_id, competency_id) |>
    collect()

  tbl_delete(
    data.frame(id = existing$id),
    conn,
    "competency_score",
    commit = F,
    returnData = F
  )

  # Add new results
  compScores <- tbl_insert(compScores, conn, "competency_score", commit = F)
  compText <- compText |>
    left_join(
      compScores |>
        select(competency_score_id = id, review_assignment_id, competency_id),
      by = c("review_assignment_id", "competency_id")
    ) |>
    select(competency_score_id, text_match)
  compText <- tbl_insert(compText, conn, "competency_text", commit = F)

  # Update existing scores (and commit)
  result <- tbl_update(overallScores, conn, "review_assignment", commit = T)

  return(result)
}

#' Internal function to insert or update into reviewer table
#'
#' @param conn SQLite connection
#' @param data Data frame with table columns
#' @param commit (Default = TRUE) Commit the transaction
#'
#' @importFrom sqlife tbl_update tbl_insert
#'
#' @returns Inserted / Updated data frame
dbReviewer <- function(conn, data, commit = T) {
  if ("id" %in% colnames(data)) {
    # Update existing
    return(tbl_update(data, conn, "reviewer", commit = commit))
  } else {
    # Create new
    return(tbl_insert(data, conn, "reviewer", commit = commit))
  }
}

#' Insert or Update human reviewer info into the database
#'
#' @param conn CFME database connection
#' @param id (Optional) Reviewer id. If provided this means updating existing.
#' If not, a new reviewer will be created
#' @param username Username. Required if new reviewer
#' @param first (Optional) first name
#' @param last (Optional) last name
#' @param note (Optional) note
#' @param commit (Default = T) Commit the changes to the database
#'
#' @import sqlife dplyr
#'
#' @returns Data frame with inserted / updated reviewer info
#'
#' @export
dbReviewerHuman <- function(
  conn,
  id,
  username,
  first,
  last,
  note,
  commit = T
) {
  if (!missing(id)) {
    check <- id
    id <- tbl(conn, "reviewer") |>
      filter(id %in% {{ id }}, human == 1) |>
      pull(id)
    # Check if exists
    if (length(id) == 0) {
      stop(
        "No human reviewer exists with id ",
        check,
        ". Omit id to create new reviewer"
      )
    }
  } else if (missing(username)) {
    stop("A new human reviewer needs at least a username")
  } else {
    check <- tbl(conn, "reviewer") |>
      filter(username %in% {{ username }}) |>
      pull(username)
    if (length(check) > 0) {
      stop(sprintf(
        "Reviewers with username %s already exist",
        paste(check, collapse = ", ")
      ))
    }
  }

  # Create the data frame needed for insertion into reviewer table
  reviewer <- data.frame(
    id = missingVal(id),
    human = T,
    username = missingVal(username),
    first_name = missingVal(first),
    last_name = missingVal(last),
    note = missingVal(note)
  )
  # Only keep columns with any new info
  reviewer <- reviewer[, apply(reviewer, 2, function(x) !all(is.na(x)))]

  result <- dbReviewer(conn, reviewer, commit = commit)
  return(result)
}

#' Insert or Update AI reviewer info into the database
#'
#' @param conn CFME database connection
#' @param id (Optional) Reviewer id. If provided this means updating existing.
#' If not, a new reviewer will be created
#' @param model AI model name. Required if new reviewer
#' @param note (Optional) Text note
#' @param commit (Default = T) Commit the changes to the database
#'
#' @import sqlife dplyr
#'
#' @returns Data frame with inserted / updated reviewer info
#'
#' @export
dbReviewerAI <- function(
  conn,
  id,
  model,
  note,
  commit = T
) {
  if (!missing(id)) {
    check <- id
    id <- tbl(conn, "reviewer") |>
      filter(id %in% {{ id }}, human == 0) |>
      pull(id)
    # Check if exists
    if (length(id) == 0) {
      stop(
        "No AI reviewer exists with id ",
        check,
        ". Omit id to create new AI reviewer"
      )
    }
  } else if (missing(model)) {
    stop("A new AI reviewer needs model name")
  } else {
    x <- model
    check <- tbl(conn, "reviewer") |>
      filter(model == x) |>
      pull(id)
    if (length(check) > 0) {
      stop(sprintf("A reviewer with model name %s already exists", model))
    }
  }

  # Create the data frame needed for insertion into reviewer table
  reviewer <- data.frame(
    id = missingVal(id),
    human = F,
    model = missingVal(model),
    note = missingVal(note)
  )
  # Only keep columns with any new info
  reviewer <- reviewer[, apply(reviewer, 2, function(x) !all(is.na(x)))]

  result <- dbReviewer(conn, reviewer, commit = commit)

  return(result)
}

#' Insert or update a review assignment
#'
#' @param conn CFME database connection
#' @param id (Optional) Review assignment ID. If not set, new entry is created
#' @param reviewer_id (Required if id not set)
#' @param evaluation_id (Required if id not set)
#' @param review_prompt_id (Required if id not set)
#' @param include_questions (Optional value)
#' @param redacted (Optional value)
#' @param duration (Optional value)
#' @param statusCode (Optional value)
#' @param tokens_in (Optional value)
#' @param tokens_out (Optional value)
#' @param note (Optional value)
#' @param timestamp (Optional value)
#' @param commit (Default = T)
#'
#' @returns A data frame with inserted / updated database records in review_assignment table
#' @export
dbReviewAssignment <- function(
  conn,
  id,
  reviewer_id,
  evaluation_id,
  review_prompt_id,
  include_questions,
  redacted,
  duration,
  statusCode,
  tokens_in,
  tokens_out,
  note,
  timestamp,
  commit = T
) {
  data <- getFunArgs(c("conn", "commit")) |> as.data.frame()

  if (missing(id)) {
    # New
    data$statusCode = 0
    if (missing(redacted)) {
      data$redacted = T
    } else {
      redactedOnly <- tbl(conn, "answer") |>
        slice_sample(n = 5) |>
        pull(answer_txt) |>
        is.na() |>
        sum() ==
        5
      if (redactedOnly & redacted == F) {
        stop("This database only contains redacted evaluations")
      }
    }

    # Check prompt
    if (missing(review_prompt_id)) {
      review_prompt_id <- tbl(conn, "review_prompt") |>
        filter(id == max(id)) |>
        pull(id)
      if (length(review_prompt_id) == 0) {
        stop("You need to add at least one prompt before assigning reviews")
      }
    } else {
      review_prompt_id <- tbl(conn, "review_prompt") |>
        filter(id == local(review_prompt_id)) |>
        pull(id)

      if (length(review_prompt_id) == 0) {
        stop("The provided review_prompt_id does not exist")
      }
    }

    data$review_prompt_id = review_prompt_id

    result <- tbl_insert(data, conn, "review_assignment", commit = commit)
  } else {
    # Existing
    result <- tbl_update(data, conn, "review_assignment", commit = commit)
  }

  return(result)
}
