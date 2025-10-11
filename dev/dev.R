# devtools::install_github("pieterjanvc/sqlife", ref = "data_manipulation")

dbInfo <- "local/dev.db"

# Start from scratch ----
# ***********************
if (file.exists(dbInfo)) {
  file.remove(dbInfo)
}

dbSetup(dbInfo, "inst/cfme.sql", validateSchema = T)
data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx"
)

dbAddEvaluations(data, dbInfo)
# ---

# Run eval through LLM and insert into DB
# ***************************************
ids = 1
evals <- getEvals(ids, dbInfo = dbInfo)

prompt <- readLines("inst/rubricPrompt.txt") |> paste(collapse = "\n")
promptID <- dbAddPrompt(prompt, dbInfo)


test <- llm_evaluation(
  dbInfo,
  prompt_id = 1,
  evaluation_id = 1,
  log = "local/apiLog.csv",
  include_questions = T,
  redacted = T,
  maxTries = 3
)
