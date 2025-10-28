library(shiny)
library(DT)
library(stringr)

# dbInfo <- "local/test.db"
dbInfo <- "../local/test.db"

ui <- fluidPage(
  # Capture highlighted text on the screen
  tags$script(HTML(
    "
    document.addEventListener('mouseup', function() {
      var selected = window.getSelection().toString();
      Shiny.setInputValue('highlighted_text', selected);
    });
  "
  )),
  # Layout
  fluidRow(
    column(
      6,
      wellPanel(
        h3("Student evaluation"),
        selectInput("evalID", "Select an evaluation", choices = c()),
        checkboxInput("showQuestions", "Show questions", value = T),
        tags$hr(),
        uiOutput("evaluation")
      )
    ),
    column(
      6,
      wellPanel(
        h3("Rubric"),
        selectInput("cID", "Competency", choices = NULL),
        uiOutput("compDescr"),
        textInput(
          "txtEvidence",
          "Text evidence",
          placeholder = "Copy-paste pieces of text, separate by ; if multiple"
        ),
        radioButtons("spec", "Specificity score", choices = c(1:4), inline = T),
        radioButtons("util", "Utility score", choices = c(1:3), inline = T),
        radioButtons("sent", "Sentiment score", choices = c(1:5), inline = T),
        actionButton("add", "Add competency review")
      )
    )
  ),
  fluidRow(DTOutput("review"))
)

server <- function(input, output, session) {
  # DB Connection
  conn <- dbGetConn(dbInfo)
  onStop(function(x) {
    dbFinish(conn)
  })

  # For now select random IDs
  evalIDs <- tbl(conn, "evaluation") |>
    slice_sample(n = 10) |>
    pull(id) |>
    sort()

  # Get the prompt and use it to create the rubric
  prompt <- reactive({
    req(input$evalID)

    # Check if the eval was already reviewed and use the same prompt version
    review_prompt_id <- tbl(conn, "review_response") |>
      filter(evaluation_id == as.integer(input$evalID)) |>
      pull(review_prompt_id)

    # Otherwise use the latest prompt version
    if (length(review_prompt_id) == 0) {
      review_prompt_id <- tbl(conn, "review_prompt") |>
        filter(id == max(id)) |>
        pull(id)
    }

    # Get the prompt text and parse it
    text <- tbl(conn, "review_prompt") |>
      filter(id == review_prompt_id) |>
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

  # Set the eval IDs
  updateSelectInput(
    session,
    "evalID",
    choices = setNames(
      evalIDs,
      paste("Evaluation", evalIDs)
    )
  )

  # Reset rubric on competency change
  observeEvent(input$cID, {
    updateRadioButtons(inputId = "spec", selected = 1)
    updateRadioButtons(inputId = "util", selected = 1)
    updateRadioButtons(inputId = "sent", selected = 1)
    updateTextInput(inputId = "txtEvidence", value = "")
  })

  output$compDescr <- renderUI({
    tagList(tags$i(
      prompt()$parsed$content$competencies[[input$cID]]$description,
      br(),
      br()
    ))
  })

  # The UI that shows the evaluation
  output$evaluation <- renderUI({
    req(input$evalID)
    div(
      HTML(
        dbGetEvals(
          ids = as.integer(input$evalID),
          dbInfo = dbInfo,
          redacted = T,
          includeQuestions = input$showQuestions,
          html = T,
          subtitleTag = "b"
        ) |>
          pull(evaluation)
      ),
      style = "max-height: 600px; overflow-y: auto;"
    )
  })

  compReviews <- reactiveVal()

  # Add or update a competency review
  observeEvent(input$add, {
    check <- str_trim(input$txtEvidence) != ""
    if (!check) {
      showModal(modalDialog(
        HTML(
          "Please make sure to provide text evidence",
          "by copy-pasting text from the evaluation. Separate multiple pieces",
          "using the semi-colon if needed.<b>;</b><br><br><i>Example</i><br>",
          "text evidence 1; text evidence 2"
        ),
        title = "Text evidence issue"
      ))
    }

    req(check)

    # Check if the text evidence is copy pasted
    check <- str_split(input$txtEvidence, ";")[[1]] |> str_squish()

    curEval <- dbGetEvals(
      ids = as.integer(input$evalID),
      dbInfo = dbInfo,
      redacted = T,
      includeQuestions = input$showQuestions,
      html = F
    ) |>
      pull(evaluation) |>
      str_squish()

    check <- all(str_detect(curEval, fixed(check)))

    if (!check) {
      showModal(modalDialog(
        HTML(
          "Please make sure you copy-paste text evidence exactly",
          "from the evaluation and separate multiple pieces by using a",
          "the semi-colon <b>;</b><br><br><i>Example</i><br>",
          "text evidence 1; text evidence 2"
        ),
        title = "Text evidence issue"
      ))
    }

    req(check)

    #Update the competency reviews data frame
    if (input$cID %in% compReviews()$competency_id) {
      compReviews(compReviews()[
        -c(input$cID %in% compReviews()$competency_id),
      ])
    }

    compReviews(
      rbind(
        data.frame(
          competency_id = as.integer(input$cID),
          specificity = as.integer(input$spec),
          utility = as.integer(input$util),
          sentiment = as.integer(input$sent),
          text_matches = input$txtEvidence
        ),
        compReviews()
      )
    )
  })

  output$review <- renderDT(
    {
      req(compReviews())
      # Replace ID with the competency description
      compReviews() |>
        left_join(
          data.frame(
            competency_id = 1:6,
            competency = sapply(
              prompt()$parsed$content$competencies,
              "[[",
              "name"
            ) |>
              str_trunc(20)
          ),
          by = "competency_id"
        ) |>
        select(-competency_id) |>
        select(competency, everything())
    },
    rownames = F
  )
}

shinyApp(ui, server)
