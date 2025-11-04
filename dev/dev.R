# devtools::install_github("pieterjanvc/sqlife", ref = "main")

dbInfo <- "local/preview.db"
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
. <- dbAddEvaluations(combined_data, dbInfo, redactedOnly = T)
# Add human reviewers
. <- dbReviewerHuman(dbInfo, username = usernames)
# Add default AI reviewer
. <- dbReviewerAI(dbInfo, model = formals(llm_call)$model)
# Add prompt
prompt <- readLines("inst/rubricPrompt.txt") |> paste(collapse = "\n")
review_prompt_id <- dbAddPrompt(prompt, dbInfo)
# Assign the same n random evals to each reviewer
set.seed(seed)
. <- evalSample <-
  tbl(conn, "evaluation") |>
  group_by(summary_flg, complete) |>
  slice_sample(n = 5) |>
  pull(id)
. <- dbReviewAssignment(
  dbInfo,
  reviewer_id = rep(1:((length(usernames) + 1)), each = 15),
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T
)
dbFinish(conn)

# Add manual reviews
# *******************

man <- bind_rows(
  readxl::read_xlsx("local/manualReviewScores.xlsx", 1) |>
    mutate(reviewer_id = 3),
  readxl::read_xlsx("local/manualReviewScores.xlsx", 2) |>
    mutate(reviewer_id = 2)
) |>
  mutate(evaluation_id = str_extract(eID, "\\d+") |> as.integer()) |>
  tidyr::fill(evaluation_id) |>
  filter(!is.na(cID), !is.na(spec), !is.na(utility), !is.na(sent)) |>
  filter(spec != 0) |>
  select(-eID)

# man |> group_by(reviewer_id, eID) |> filter(n_distinct(cID) != n())

eIds <- man |> select(reviewer_id, evaluation_id) |> distinct()

assigned <- dbReviewAssignment(
  dbInfo,
  reviewer_id = eIds$reviewer_id,
  evaluation_id = eIds$evaluation_id,
  redacted = T,
  include_questions = T
)

scores <- man |>
  mutate(text = ifelse(is.na(text), ".", text)) |>
  left_join(
    assigned |> select(id, evaluation_id, reviewer_id),
    by = c("evaluation_id", "reviewer_id")
  ) |>
  rename(
    review_assignment_id = id,
    competency_id = cID,
    specificity = spec,
    sentiment = sent,
    text_matches = text,
    note = comment
  ) |>
  select(-evaluation_id, -reviewer_id)

result <- dbReviewScore(dbInfo, scores, reviewStatus = 2)


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

# Open the DB
shell.exec(normalizePath(dbInfo))

# Generate manual review doc
# **************************

man <- bind_rows(
  readxl::read_xlsx("local/manualReviewScores.xlsx", 1) |>
    mutate(reviewer_id = 3),
  readxl::read_xlsx("local/manualReviewScores.xlsx", 2) |>
    mutate(reviewer_id = 2)
) |>
  mutate(evaluation_id = str_extract(eID, "\\d+") |> as.integer()) |>
  tidyr::fill(evaluation_id) |>
  filter(!is.na(cID), !is.na(spec), !is.na(utility), !is.na(sent)) |>
  filter(spec != 0) |>
  select(-eID)
