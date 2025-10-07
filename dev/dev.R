library(dplyr)
library(readxl)
library(stringr)
library(sqlife)
# devtools::install_github("pieterjanvc/sqlife", ref = "data_manipulation")

# Create a prompt to evaluate the reviews according to the rubric
data <- read_xlsx("local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx", 1)
colnames(data) <- str_replace_all(tolower(colnames(data)), "\\s+", "_")
data <- data |>
  mutate(
    rowid = as.integer(rowid),
    submission_date = as.character(submission_date)
  )

dataDict <- read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx",
  2
) |>
  mutate(COLUMN = str_replace_all(tolower(COLUMN), "\\s+", "_"))
colnames(dataDict) <- str_replace_all(tolower(colnames(dataDict)), "\\s+", "_")

dataDict <- dataDict |>
  left_join(
    data.frame(
      column = colnames(data),
      type = sapply(data[1, ], function(x) {
        ifelse(
          class(x)[1] == "numeric",
          "INTEGER",
          "TEXT"
        )
      }) |>
        unlist()
    ),
    by = "column"
  ) |>
  select(column, type, definition)

dataDict |> select(-definition) |> print(n = 50)

sqlife::sql_create(data, "evals")

result <- dbSetup("local/cfme.db", "inst/cfme.sql", validateSchema = T)
result <- tbl_insert(data, "local/cfme.db", "evals")

# Test
sysPrompt <- readLines("inst/rubricPrompt.txt") |> paste(collapse = "\n")
prompt <- paste(sysPrompt, "#EVALUATION\n\n", data$answer_txt[1])
resp <- llm_call(prompt, log = "local/apiLog.csv")
resp$choices[[1]]$message$content |> cat()
