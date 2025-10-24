library(shiny)
library(DT)
library(stringr)

# dbInfo <- "local/test.db"
dbInfo <- "../local/test.db"

ui <- fluidPage(
  fluidRow(
    column(
      7,
      wellPanel(
        h3("Rubric"),
        selectInput("cID", "Competency", choices = NULL),
        uiOutput("compDescr"),
        radioButtons("spec", "Specificity score", choices = c(1:4), inline = T),
        radioButtons("util", "Utility score", choices = c(1:3), inline = T),
        radioButtons("sent", "Sentiment score", choices = c(1:5), inline = T),
        actionButton("add", "Add competency review")
      )
    ),
    column(
      5,
      wellPanel(
        h3("Student evaluation"),
        uiOutput("review")
      )
    )
  ),
  fluidRow(DTOutput("eval"))
)

server <- function(input, output, session) {
  # DB Connection
  conn <- dbGetConn(dbInfo)
  onStop(function(x) {
    dbFinish(conn)
  })

  prompt <- reactive({
    text <- tbl(conn, "review_prompt") |>
      filter(id == 2) |>
      pull(prompt)
    parsed <- parsePrompt(text)

    # Update all inputs based on rubric phrasing
    updateSelectInput(
      inputId = "cID",
      choices = setNames(
        1:6,
        sapply(parsed$content$competencies, "[[", "name")
      )
    )
    updateRadioButtons(
      inputId = "spec",
      label = parsed$content$scoring$spec$desciption,
      choices = setNames(
        1:length(parsed$content$scoring$spec$options),
        parsed$content$scoring$spec$options
      )
    )
    updateRadioButtons(
      inputId = "util",
      label = parsed$content$scoring$util$desciption,
      choices = setNames(
        1:length(parsed$content$scoring$util$options),
        parsed$content$scoring$util$options
      )
    )
    updateRadioButtons(
      inputId = "sent",
      label = parsed$content$scoring$sent$desciption,
      choices = setNames(
        1:length(parsed$content$scoring$sent$options),
        parsed$content$scoring$sent$options
      )
    )
    list(text = text, parsed = parsed)
  })

  output$compDescr <- renderUI({
    tagList(tags$i(
      prompt()$parsed$content$competencies[[input$cID]]$description,
      br(),
      br()
    ))
  })

  output$review <- renderUI({
    HTML(
      dbGetEvals(
        1,
        dbInfo,
        redacted = T,
        includeQuestions = T,
        html = T,
        subtitleTag = "b"
      ) |>
        pull(evaluation)
    )
  })

  # output$rubric <- renderUI({
  #   div(
  #     prompt()$text |>
  #       markdown(),
  #     style = "height: 500px; overflow: scroll;"
  #   )
  # })
}

shinyApp(ui, server)
