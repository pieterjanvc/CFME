#' Check if the LLM response is in the expected CSV format
#'
#' @param string To parse as CSV
#' @param expColnames (Optional) Named list of column names / type to expect.
#'  Defaults:
#' "cID" = "integer",
#' "text" = "character"
#' "spec" = "integer"
#' "utility" = "integer"
#' "sent" = "integer"
#'
#' @returns List with two elements
#'  - statusCode: 0 not valid CSV, 1 column names / number issue,
#'     2 column type error, 3 expected CSV format
#'  - data: Data frame with results for statusCode 3, NULL otherwise
#' @export
#'
llm_csv_response <- function(string, expColnames) {
  if (missing(expColnames)) {
    expColnames <- c(
      "cID" = "integer",
      "text" = "character",
      "spec" = "integer",
      "utility" = "integer",
      "sent" = "integer"
    )
  }

  suppressWarnings({
    tryCatch(
      {
        # Try and load as CSV
        check <- read.csv(textConnection(string), fill = F)

        # Check number of columns and names
        if (!all(colnames(check) == names(expColnames))) {
          return(list(statusCode = 1, data = NULL))
        }

        # Check column type
        if (!all(sapply(check, class) == expColnames)) {
          return(list(statusCode = 2, data = NULL))
        }

        return(list(statusCode = 3, data = check))
      },
      error = function(e) {
        return(list(statusCode = 0, data = NULL))
      }
    )
  })
}

#' Call an Azure LLM
#'
#' Note that this function expects an environment variable HMS_AZURE_API
#' that contains the API key You can set this up using
#' `Sys.setenv(HMS_AZURE_API = "API token here")`
#'
#' @param user User prompt
#' @param system Default = "You are a helpful AI assistant". System prompt
#' @param model Default = gpt-4o-1120. LLM model to use
#' @param maxTokens Default = 500. Max Number of tokens to return
#' @param version Default = 2024-10-21.
#' @param endpoint Default = https://azure-ai.hms.edu. Azure endpoint
#' @param log If set, the token usage is kept track of in this CSV file
#'
#' @import httr2
#'
#' @returns LLM Response object (list).
#'
#' If a log file is et a line is added to a CSV file with columns
#' timestamp, promptTokens and responseTokens (headers not included)
#'
#' @export
#'
llm_call <- function(user, system, model, maxTokens, version, endpoint, log) {
  # API info https://learn.microsoft.com/en-us/azure/ai-foundry/openai/reference

  # Model info
  endpoint <- ifelse(missing(endpoint), "https://azure-ai.hms.edu", endpoint)
  version <- ifelse(missing(version), "2024-10-21", endpoint)
  model <- ifelse(missing(model), "gpt-4o-1120", endpoint)

  # Build the URL
  baseURL <- sprintf(
    "%s/openai/deployments/%s/chat/completions?api-version=%s",
    endpoint,
    model,
    version
  )

  # Build the body
  system <- ifelse(missing(system), "You are a helpful AI assistant", system)
  maxTokens <- ifelse(missing(maxTokens), 500, maxTokens)
  body <- list(
    messages = list(
      list(role = "system", content = system),
      list(role = "user", content = user)
    ),
    max_tokens = maxTokens
  )

  # Send the request
  req <- request(baseURL) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_perform()

  if (resp_status(req) != 200) {
    stop(req)
  }

  resp <- resp_body_json(req)

  # Write token usage to log file if set
  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$prompt_tokens,
        resp$usage$completion_tokens
      ),
      log,
      append = T
    )
  }

  return(resp)
}

# NOT READY YET
dbAddLLMresponse <- function(
  dbInfo,
  evaluation_id,
  includes_questions,
  redacted,
  prompt_hash,
  prompt,
  llm_response
) {
  conn <- dbGetConn(dbInfo)
  promptID <- tbl(conn, "llm_prompt") |>
    filter(hash == prompt_hash) |>
    pull(id)

  # Add new prompt if needed
  if (length(promptID) == 0) {
    toInsert <- data.frame(
      hash = prompt_hash,
      text = prompt
    )

    promptID <- tbl_insert(toInsert, conn, "llm_prompt", commit = F) |> pull(id)
  }

  # Check the CSV output
  data <- llm_csv_response(llm_response$choices[[1]]$message$content)

  # Add the llm_response metadata
  toInsert <- data.frame(
    evaluation_id = evaluation_id,
    prompt_id = promptID,
    model = llm_response$model,
    includes_questions = includes_questions,
    redacted = redacted,
    statusCode = data$statusCode,
    tokens_in = llm_response$usage$prompt_tokens,
    tokens_out = llm_response$usage$completion_tokens
  )

  responseID <- tbl_insert(toInsert, conn, "llm_response", commit = F) |>
    pull(id)

  # In case parsing of the result failed, end here
  if (data$statusCode != 3) {
    if (class(dbInfo) == "character") {
      dbFinish(conn)
    } else {
      dbCommit(conn)
    }

    return(list(
      success = F,
      promptID = promptID,
      responseID = responseID,
      evaluationID = NA
    ))
  }

  # Add the evaluation data
  data$llm_response_id = responseID
  data <- data |>
    rename(
      competency_id = cID,
      specificity = spec,
      utility = util,
      sentiment = sent,
      text_matches = text
    )

  evaluationID <- tbl_insert(data, conn, "llm_evaluation", commit = F) |>
    pull(id)

  # Finsh and return
  if (class(dbInfo) == "character") {
    dbFinish(conn)
  } else {
    dbCommit(conn)
  }

  return(list(
    success = T,
    promptID = promptID,
    responseID = responseID,
    evaluationID = evaluationID
  ))
}
