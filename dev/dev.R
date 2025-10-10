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


result <- dbSetup("local/cfme.db", "inst/cfme.sql", validateSchema = F)
conn <- dbGetConn("local/cfme.db")

test <- tbl(conn, "evals") |> collect()
test <- test |>
  # filter(learner_anon_id == learner_anon_id[1]) |>
  group_by(submission_date, evaluator_id, learner_anon_id) |>
  mutate(eval_id = cur_group_id()) |>
  ungroup()

test |> group_by(learner_anon_id) |> filter(n_distinct(first_nbme_score) > 1)


combined_data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx",
  1
)
dbInfo <- "local/dev.db"

addDataToDB(combined_data, dbInfo)
shell.exec(dbInfo)

tbl(conn, "answer") |>
  inner_join(
    tbl(conn, "evaluation") |>
      select(id, summary_flg),
    by = c("evaluation_id" = "id")
  ) |>
  group_by(evaluation_id, summary_flg) |>
  summarise(n = n(), .groups = "drop") |>
  select(-evaluation_id) |>
  distinct()


test <- data |>
  group_by(
    rotation_id,
    reviewer_id,
    summary_flg,
    acad_yr
  ) |>
  summarise(n = n(), firstRow = rowid[1], .groups = "drop")


dbInfo <- "local/dev.db"
seed <- sample(1:10000, 1)
set.seed(seed)
test <- reviewDoc("D:/Desktop/testReviews.txt", dbInfo, n = 10, html = F)

test <- reviewDoc(
  "D:/Desktop/testReviews.html",
  dbInfo,
  ids = c(1, 5, 8),
  html = T
)
