library(httr2)

llm_call <- function(user, system, model, maxTokens, version, endpoint) {
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
    # Maybe use if needed later
  }

  return(resp_body_json(req))
}

# Test
resp <- llm_call("Write a limerick about a bird")
resp$choices[[1]]$message$content |> cat()
