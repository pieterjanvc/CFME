# devtools::install_github("pieterjanvc/sqlife", ref = "main")

dbInfo <- "local/test.db"
usernames <- c("PJ", "TK", "AW")
seed <- 12345
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
# Add default AI reviewer
dbReviewerAI(dbInfo, model = formals(llm_call)$model)
# Add prompt
prompt <- readLines("inst/rubricPrompt.txt") |> paste(collapse = "\n")
review_prompt_id <- dbAddPrompt(prompt, dbInfo)
# Assign the same n random evals to each reviewer
set.seed(seed)
evalSample <-
  tbl(conn, "evaluation") |>
  group_by(summary_flg, complete) |>
  slice_sample(n = 5) |>
  pull(id)
dbReviewAssignment(
  dbInfo,
  reviewer_id = rep(1:((length(usernames) + 1)), each = 15),
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T
)


# Run eval through LLM and insert into DB
# ***************************************
review_assignment_ids <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == 4) |>
  pull(id)
# ONy do two reviews for now
llmReview <- llm_review(
  dbInfo,
  review_assignment_id = review_assignment_ids[1:2],
  log = "local/apiLog.csv",
  maxTries = 3
)

dbAIreview(conn, llmReview)
dbFinish(conn)
# Open the DB
shell.exec(normalizePath(dbInfo))


# Generate manual review doc
# **************************

test <- dbGetEvals(
  ids = 45,
  dbInfo = conn,
  redacted = T,
  includeQuestions = T,
  html = T,
  subtitleTag = "b"
)


evals |>
  mutate(
    answer = ifelse(
      includeQuestions,
      paste0(
        ifelse(html, sprintf("<%s>", subtitleTag), "---"),
        question,
        ifelse(html, sprintf("</%s><br>", subtitleTag), "\n"),
        answer
      ),
      answer
    )
  ) |>
  group_by(evaluation_id) |>
  summarise(
    summary = summary_flg[1] == 1,
    complete = complete[1] == 1,
    clerkship = clerkship[1],
    evaluation = paste(
      answer,
      sep = "",
      collapse = ifelse(html, "<br><br>", "\n\n")
    ),
    .groups = "drop"
  ) |>
  pull(evaluation) |>
  cat()
