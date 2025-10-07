#' Parse data from a combined format and insert it into the database
#'
#' @param combined_data Dataframe of the original data
#' @param dbInfo connection or path to an SQLite database
#'
#' @import dplyr
#' @import RSQLite
#' @import sqlife
#'
#' @returns TRUE if success
#' @export
#'
addDataToDB <- function(combined_data, dbInfo) {
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
    as.data.frame()

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

  # --- Insert reviewer data
  #  Given their title can change over time, the evaluator_id is NOT unique
  reviewer <- data |>
    select(
      evaluator_id,
      evaluator,
      acad_title
    ) |>
    distinct()

  reviewer <- tbl_insert(reviewer, conn, "reviewer", commit = F)

  # Add the new reviewer ID to the data
  data <- data |>
    left_join(
      reviewer |> select(evaluator_id, acad_title, reviewer_id = id),
      by = c("evaluator_id", "acad_title")
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
    select(
      rotation_id,
      reviewer_id,
      acad_yr
    ) |>
    distinct()

  check <- evaluation |> group_by(rotation_id, reviewer_id) |> filter(n() > 1)
  if (nrow(check) > 0) {
    head(check)
    stop("Evaluations are not unique")
  }

  evaluation <- tbl_insert(evaluation, conn, "evaluation", commit = F)

  # Add the new evaluation ID to the data
  data <- data |>
    left_join(
      evaluation |> select(rotation_id, reviewer_id, evaluation_id = id),
      by = c("rotation_id", "reviewer_id")
    )

  # --- Insert question data
  question <- data |>
    select(
      question,
      summary_flg
    ) |>
    distinct() |>
    mutate(summary_flg = ifelse(summary_flg == "Y", 1, 0))

  question <- tbl_insert(question, conn, "question", commit = F)

  # Add the new question ID to the data
  data <- data |>
    left_join(
      question |>
        select(question, summary_flg, question_id = id) |>
        mutate(summary_flg = ifelse(summary_flg == 1, "Y", "N")),
      by = c("question", "summary_flg")
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
    left_join(reviewer, by = c("reviewer_id" = "id")) |>
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
