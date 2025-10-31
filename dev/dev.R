# devtools::install_github("pieterjanvc/sqlife", ref = "v0.1.2")

dbInfo <- "local/test.db"
dbInfo <- "local/dev.db"

# Start from scratch ----
# ***********************
if (file.exists(dbInfo)) {
  file.remove(dbInfo)
}

dbSetup(dbInfo, "inst/cfme.sql", validateSchema = T)
combined_data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx"
)

dbAddEvaluations(combined_data, dbInfo)


# Run eval through LLM and insert into DB
# ***************************************

prompt <- readLines("inst/rubricPrompt.txt") |> paste(collapse = "\n")
review_prompt_id <- dbAddPrompt(prompt, dbInfo)


test <- llm_review(
  dbInfo,
  review_prompt_id = 1,
  evaluation_id = 2,
  log = "local/apiLog.csv",
  include_questions = T,
  redacted = T,
  maxTries = 3
)

review_scores_ids <- dbAddLLMreview(dbInfo, test)

# Open the DB
shell.exec(normalizePath(dbInfo))


# Generate manual review doc
# **************************

conn <- dbGetConn(dbInfo)

samples <- c("MEDICINE" = 5, "NEUROLOGY" = 5)
evaluation_ids <- sapply(
  1:length(samples),
  function(i) {
    tbl(conn, "evaluation") |>
      filter(summary_flg == 0) |>
      left_join(
        tbl(conn, "rotation") |> select(id, clerkship_id),
        by = c("rotation_id" = "id")
      ) |>
      left_join(tbl(conn, "clerkship"), by = c("clerkship_id" = "id")) |>
      filter(clerkship == local(names(samples[i]))) |>
      slice_sample(n = samples[i]) |>
      pull(id)
  }
) |>
  as.integer()

evaluationsToReview <- reviewDoc(
  "local/evalTest.html",
  conn,
  evaluation_ids = evaluation_ids,
  html = T,
  includeClerkship = T,
  includeQuestions = T
)

evaluation_ids <- c(120, 241, 734, 1209, 1236, 1636, 1693, 1968, 1979, 2022)
evals <- dbGetEvals(evaluation_ids, dbInfo)

dbDisconnect(conn)
