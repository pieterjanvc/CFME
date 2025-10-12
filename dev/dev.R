# devtools::install_github("pieterjanvc/sqlife", ref = "data_manipulation")

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
# ---

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

review_scores_ids <- dbAddLLMresponse(dbInfo, test)

# Open the DB
shell.exec(normalizePath(dbInfo))
