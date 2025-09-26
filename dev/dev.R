# Test
resp <- llm_call("Write a limerick about a bird", log = "local/apiLog.csv")
resp$choices[[1]]$message$content |> cat()
