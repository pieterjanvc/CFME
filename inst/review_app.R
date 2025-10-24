library(shiny)
library(DT)
library(stringr)

dbInfo <- "../local/test.db"

ui <- fluidPage(
  fluidRow(
    column(
      5,
      wellPanel(
        h3("Competency review"),
        selectInput("cID", "Competency", choices = NULL),
        radioButtons("spec", "Specificity score", choices = c(1:4), inline = T),
        radioButtons("util", "Utility score", choices = c(1:3), inline = T),
        radioButtons("sent", "Sentiment score", choices = c(1:5), inline = T),
        actionButton("add", "Add competency review")
      )
    ),
    column(7, uiOutput("rubric"))
  ),
  fluidRow(column(6, uiOutput("review")), column(6, DTOutput("eval")))
)

server <- function(input, output, session) {
  # DB Connection
  conn <- dbGetConn(dbInfo)
  onStop(function(x) {
    dbFinish(conn)
  })

  prompt <- reactive({
    x <- tbl(conn, "review_prompt") |>
      filter(id == 2) |>
      pull(prompt)
    list(text = x, parsed = parsePrompt(x))
  })

  output$rubric <- renderUI({
    div(
      prompt()$text |>
        markdown(),
      style = "height: 500px; overflow: scroll;"
    )
  })
}

shinyApp(ui, server)
