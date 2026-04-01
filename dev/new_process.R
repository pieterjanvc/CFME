library(httr2)
library(jsonlite)
library(dplyr)

# =============================================================================
# STEP 1: COMPETENCY EXTRACTION
# =============================================================================
# Separate from scoring to allow human correction before scores are assigned.
# Key differences from the existing llm_chat_completion wrapper:
#   - temperature = 0 where supported (gpt-4o); gpt-5-mini ignores it (reasoning model)
#   - response_format = json_object  (API guarantees valid JSON, no retry needed)
#   - max_completion_tokens vs max_tokens depending on model generation

#' Extract competencies and verbatim text from a clerkship evaluation
#'
#' @param evaluation_text Character string with the evaluation text
#' @param prompt System prompt (contents of inst/extractPrompt.md)
#' @param model Azure deployment name
#' @param endpoint Azure endpoint
#'
#' @returns List with:
#'   - statusCode: 0 = API error, 1 = parse error, 2 = success
#'   - data: data.frame with columns cID and text_match (on success), NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response string for debugging
llm_extract_competencies <- function(
  evaluation_text,
  prompt,
  model = "gpt-5-mini",
  endpoint = "https://azure-ai.hms.edu",
  version = "2024-10-21"
) {
  baseURL <- sprintf(
    "%s/openai/deployments/%s/chat/completions?api-version=%s",
    endpoint,
    model,
    version
  )

  # gpt-4o uses max_tokens + supports temperature;
  # gpt-5-mini uses max_completion_tokens and does not accept temperature
  is_gpt4o <- grepl("gpt-4o", model)
  token_param <- if (is_gpt4o) "max_tokens" else "max_completion_tokens"

  body <- c(
    list(
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user", content = evaluation_text)
      ),
      response_format = list(type = "json_object")
    ),
    if (is_gpt4o) list(temperature = 0) else list(),
    # gpt-4o: 800 output tokens is sufficient
    # gpt-5-mini: reasoning model — tokens cover internal thinking + output,
    # so budget must be large enough for both; 4000 leaves ~3200 for actual output
    setNames(list(if (is_gpt4o) 800L else 4000L), token_param)
  )

  req <- request(baseURL) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0,
      data = NULL,
      tokens_in = NA,
      tokens_out = NA,
      raw = resp_body_string(req)
    ))
  }

  resp <- resp_body_json(req)
  raw <- resp$choices[[1]]$message$content
  tokens_in <- resp$usage$prompt_tokens
  tokens_out <- resp$usage$completion_tokens

  # Parse and validate
  parsed <- tryCatch(
    fromJSON(raw, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !("extractions" %in% names(parsed))) {
    return(list(
      statusCode = 1,
      data = NULL,
      tokens_in = tokens_in,
      tokens_out = tokens_out,
      raw = raw
    ))
  }

  # Flatten to a data.frame with one row per (competency, text_span) pair
  if (length(parsed$extractions) == 0) {
    data <- data.frame(cID = integer(0), text_match = character(0))
  } else {
    data <- do.call(
      rbind,
      lapply(parsed$extractions, function(x) {
        data.frame(
          cID = x$cID,
          text_match = unlist(x$text),
          stringsAsFactors = FALSE
        )
      })
    )
  }

  list(
    statusCode = 2,
    data = data,
    tokens_in = tokens_in,
    tokens_out = tokens_out,
    raw = raw
  )
}

#' Parse and validate the extraction output
#'
#' Checks that:
#' - cID values are integers in 1:8
#' - text_match values are non-empty strings
#' - no duplicate (cID, text_match) pairs
#'
#' @param result Output of llm_extract_competencies()
#' @returns List with statusCode (2 = valid, 1 = validation issue) and data
validate_extraction <- function(result) {
  if (result$statusCode != 2) {
    return(result)
  }
  d <- result$data

  if (nrow(d) == 0) {
    return(result)
  } # empty is valid

  issues <- character(0)

  if (!all(d$cID %% 1 == 0) || !all(d$cID %in% 1:8)) {
    issues <- c(issues, "cID out of range 1:8")
  }
  if (any(nchar(trimws(d$text_match)) == 0)) {
    issues <- c(issues, "empty text_match")
  }
  if (anyDuplicated(d)) {
    issues <- c(issues, "duplicate rows")
    d <- unique(d)
  }

  if (length(issues) > 0) {
    message("Extraction validation: ", paste(issues, collapse = "; "))
  }

  result$data <- d
  result
}

# =============================================================================
# USAGE
# =============================================================================

# Load dependencies
# source("R/llm.R")        # existing helpers
# source("R/dbOperations.R")

# Load prompt
extract_prompt <- readLines("inst/extractPrompt.md") |> paste(collapse = "\n")

# Test on a single evaluation
conn <- dbGetConn("local/ai_review.db")
eval_text <- dbGetEvals(1, conn)$evaluation

tbl(conn, "competency_score")

result <- llm_extract_competencies(
  evaluation_text = eval_text,
  prompt = extract_prompt
)

result |> validate_extraction()

result2 <- llm_extract_competencies(
  evaluation_text = eval_text,
  prompt = extract_prompt
)

result3 <- llm_extract_competencies(
  evaluation_text = eval_text,
  prompt = extract_prompt,
  model = "gpt-4.1"
)

result4 <- llm_extract_competencies(
  evaluation_text = eval_text,
  prompt = extract_prompt,
  model = "gpt-4.1"
)

result$statusCode # 2 = success
result$data # data.frame: cID, text_match
result$tokens_in
result$tokens_out


result <- llm_extract_competencies(
  evaluation_text = "She was a great student to have on this rotation.
   Every morning she would start by reviewing the patients with the nurses,
   then she would make her own summaries of the patient's status change since
   the last round and use this to update us with the most relevant information.
   In addition, she took the time to properly introduce herself to all staff and
   always asked about their job so to learn more about their role and how it
   would affect patient care and teamwork. I have rarely seen this in an internship!",
  prompt = extract_prompt
)

result2 <- llm_extract_competencies(
  evaluation_text = "She was a great student to have on this rotation.
   Every morning she would start by reviewing the patients with the nurses,
   then she would make her own summaries of the patient's status change since
   the last round and use this to update us with the most relevant information.
   In addition, she took the time to properly introduce herself to all staff and
   always asked about their job so to learn more about their role and how it
   would affect patient care and teamwork. I have rarely seen this in an internship!",
  prompt = extract_prompt
)

test <- fromJSON(result2$raw)
test$extractions$text[[2]]
