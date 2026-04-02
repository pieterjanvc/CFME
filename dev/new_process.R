Sys.setenv("HMS_AZURE_API" = keyring::key_get("HMS_AZURE_API"))

# Load prompt
prompt_extract <- readLines("inst/prompt_comp_extract.md") |>
  paste(collapse = "\n")

# Test on a single evaluation
conn <- dbGetConn("local/ai_review.db")
eval_text <- dbGetEvals(1, conn)$evaluation

test <- llm_comp_extract(evaluation_text = eval_text, prompt = prompt_extract)


test$data

comp_extraction_validate(test)

test$data
