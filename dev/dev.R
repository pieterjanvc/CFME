# devtools::install_github("pieterjanvc/sqlife", ref = "data_manipulation")

# Get an evaluation
dbInfo <- "local/dev.db"

eval <- getEvals(1, dbInfo, includeQuestions = F)
systemPrompt <- readLines("inst/rubricPrompt.txt") |> paste(collapse = "\n")

hash <- rlang::hash(systemPrompt)

result <- llm_call(
  eval$review,
  system = systemPrompt,
  log = "local/apiLog.csv"
)

string <- result$choices[[1]]$message$content
llm_csv_response(string)$statusCode
