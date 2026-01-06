# ARGUMENTS
# *********
seed <- 20260106
db_path <- "local/ai_review.db"
dbSetup(db_path, "inst/cfme.sql")
Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))

# SETUP
# *****

# Add all data
conn <- dbGetConn(db_path)
combined_data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx"
)
. <- dbAddEvaluations(combined_data, db_path, redactedOnly = T)
# Add default AI reviewer
. <- dbReviewerAI(conn, model = formals(llm_call)$model)
# Add default prompt
prompt <- readLines("inst/rubricPrompt.md") |> paste(collapse = "\n")
review_prompt_id <- dbAddPrompt(prompt, conn)

# Assign the same n random evals to each reviewer
set.seed(seed)
evalSample <-
  tbl(conn, "evaluation") |>
  group_by(summary_flg, complete) |>
  slice_sample(n = 15) |>
  pull(id)
test <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T
)

# Run eval through LLM and insert into DB
# ***************************************
review_assignment_ids <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == 1) |>
  pull(id)

# Only do two reviews for now
llmReview <- llm_review(
  conn,
  review_assignment_id = review_assignment_ids[1:4],
  log = "local/apiLog.csv",
  maxTries = 3
)
saveRDS(llmReview, "local/llmReviewBackup.rds")
# llmReview <- readRDS("local/llmReviewBackup.rds")

dbAIreview(conn, llmReview)
system(paste("xdg-open", normalizePath(db_path)), wait = F)

# Summary stats
# *************
specScaling <- 0.3
utilScaling <- 1.5
sentScaling <- 0.3

test <- tbl(conn, "review_assignment") |>
  filter(statusCode == 2) |>
  left_join(
    tbl(conn, "competency_score") |>
      group_by(id = review_assignment_id) |>
      summarise(
        score = sum(specificity * specScaling),
        nComp = n(),
        minSpec = min(specificity),
        maxSpec = min(specificity),
        meanSpec = mean((specificity))
      ),
    by = "id"
  ) |>
  left_join(
    tbl(conn, "answer") |>
      group_by(id = evaluation_id) |>
      summarise(
        nchar = sum(nchar(answer_txt_redacted))
      ),
    by = "evaluation_id"
  ) |>
  collect() |>
  mutate(
    score = score + utility * utilScaling + sentiment * sentScaling
  ) |>
  arrange(desc(score))
