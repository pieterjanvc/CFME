library(httr2)

#' Call an Azure LLM
#'
#' @param user User prompt
#' @param system Default = "You are a helpful AI assistant". System prompt
#' @param model Default = gpt-4o-1120. LLM model to use
#' @param maxTokens Default = 250. Max Number of tokens to return
#' @param version Default = 2024-10-21.
#' @param endpoint Default = https://azure-ai.hms.edu. Azure endpoint
#' @param log If set, the token usage is kept track of in this CSV file
#'
#' @returns LLM Response object (list).
#'
#' If a log file is et a line is added to a CSV file with columns
#' timestamp, promptTokens and responseTokens (headers not included)
#'
#' @export
#'
llm_call <- function(user, system, model, maxTokens, version, endpoint, log) {
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
  system <- ifelse(missing(system), "You are a helpful AI assistant", endpoint)
  maxTokens <- ifelse(missing(maxTokens), 250, maxTokens)
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
