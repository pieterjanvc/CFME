library(htmltools)

dbInfo <- "local/dev.db"
nReviews = 10

conn <- dbGetConn(dbInfo)
evals <- data.frame(summary_flg = 1)

seed <- sample(1:10000, 1)
set.seed(seed)

# Fix this in DB as the flag should be in the eval table not question
while (any(evals$summary_flg == 1)) {
  evals <- tbl(conn, "answer") |>
    inner_join(
      tbl(conn, "evaluation") |> slice_sample(n = nReviews) |> select(id),
      by = c("evaluation_id" = "id")
    ) |>
    left_join(tbl(conn, "question"), by = c("question_id" = "id")) |>
    collect()
}

result <- evals |>
  group_by(evaluation_id) |>
  summarise(
    review = paste(
      "<h3>",
      question,
      "</h3>",
      answer_txt,
      sep = "",
      collapse = "<br>"
    ),
    .groups = "drop"
  ) |>
  summarise(
    review = paste(
      "<h2>Evaluation ID:",
      evaluation_id,
      "</h2>",
      review,
      "<br><br><hr>",
      sep = "",
      collapse = "<br>"
    )
  )

writeLines(result$review, "local/manualReviewSamples.html")
