# devtools::install_github("pieterjanvc/sqlife", ref = "notNUllUpdate")

dbInfo <- "local/test.db"
usernames <- c("PJ", "TK", "AW")
# dbInfo <- "local/dev.db"

# Start from scratch ----
# ***********************
if (file.exists(dbInfo)) {
  file.remove(dbInfo)
}

# Setup DB and add evaluation data
dbSetup(dbInfo, "inst/cfme.sql", validateSchema = T)
conn <- dbGetConn(dbInfo)
combined_data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx"
)
dbAddEvaluations(combined_data, dbInfo, redactedOnly = T)
# Add human reviewers
dbReviewerHuman(dbInfo, username = usernames)
# Add prompt
prompt <- readLines("inst/rubricPrompt.txt") |> paste(collapse = "\n")
review_prompt_id <- dbAddPrompt(prompt, dbInfo)
# Assign the same n random evals to each reviewer
evalSample <-
  tbl(conn, "evaluation") |>
  group_by(summary_flg, complete) |>
  slice_sample(n = 5) |>
  pull(id)
dbReviewAssignment(
  dbInfo,
  reviewer_id = rep(1:3, each = 15),
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T
)
dbFinish(conn)

# Run eval through LLM and insert into DB
# ***************************************
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


dbReviewAssignment(
  dbInfo,
  reviewer_id = 1,
  evaluation_id = 1,
  review_prompt_id = 1
)


test <- function(x) {
  stop("oh no ...", call. = F)
}

iris |>
  group_by(Species) |>
  slice_sample(n = ifelse(Species == "Versicolor", 2, 5))
