#' Generate a document for manual review of evaluations
#'
#' @param path Path the file where to write the output (html or txt)
#' @param dbInfo Database info
#' @param ids IDs to pull, if empty, n must be provided
#' @param n Ignored if ids is set, otherwise number of random evaluations to pull
#' @param completeOnly (Default = FALSE) Only include complete reviews
#' @param redacted (Default = TRUE) Show redacted text
#' @param includeQuestions (Default = TRUE) Add the questions to the text
#' @param html (Default = FALSE) Output HTML instead of plain text
#'
#' @returns A file is written to the specified path and a data frame with
#' all evaluations is returned
#'
#' @export
#'
reviewDoc <- function(
  path,
  dbInfo,
  ids,
  n,
  completeOnly = F,
  redacted = T,
  includeQuestions = T,
  html = F
) {
  conn <- dbGetConn(dbInfo)

  if (missing(ids)) {
    ids <- tbl(conn, "evaluation") |>
      filter(
        complete %in%
          local(
            if (completeOnly) {
              1
            } else {
              c(0, 1)
            }
          )
      ) |>
      slice_sample(n = n) |>
      pull(id)
  } else {
    n <- length(ids)
    ids <- tbl(conn, "evaluation") |>
      filter(id %in% local(ids)) |>
      pull(id)
  }

  if (length(ids) != n) {
    stop("The number of requested IDs and those in the database don't match")
  }

  evals <- getEvals(
    ids,
    conn,
    redacted = redacted,
    includeQuestions = includeQuestions,
    html = html
  )

  review <- evals |>
    summarise(
      review = paste(
        ifelse(html, "<h2>", "#### "),
        "Evaluation ID:",
        evaluation_id,
        ifelse(html, "</h2>", "\n\n"),
        review,
        ifelse(html, "<br><br><hr>", "\n\n"),
        sep = "",
        collapse = ifelse(html, "<br>", "\n")
      )
    ) |>
    pull(review)

  if (is.character(dbInfo)) {
    dbFinish(conn)
  }

  writeLines(review, path)

  return(evals)
}
