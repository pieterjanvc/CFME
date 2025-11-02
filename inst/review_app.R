library(shiny)
library(DT)
library(stringr)

dbInfo <- "../local/test.db"
# dbInfo <- "../local/dev.db"

ui <- fluidPage(
  # Layout
  wellPanel(
    fluidRow(
      column(
        4,
        h3("1. Select Reviewer"),
        selectInput("reviewerID", "Reviewer", choices = c()),
        HTML(
          "<i>A <b>reviewer</b> is the person who is assessing the quality of an evaluation here<br>",
          "An <b>evaluator</b> is the person who wrote the student evaluation below</i>"
        )
      ),
      column(
        4,
        h3("2. Pick an evaluation"),
        selectInput(
          "reviewID",
          "0 to start - 0 in progress - 0 competed",
          choices = c()
        ),
        checkboxInput("includeCompeted", "List completed", value = F),
        checkboxInput("showQuestions", "Show questions", value = T),
      ),
      column(
        4,
        h3("3. Submit once complete"),
        textAreaInput("generalComment", "Optional comment"),
        checkboxInput("flag", "Add issue flag"),
        actionButton("complete", "Mark as complete")
      )
    ),
  ),
  fluidRow(
    column(
      6,
      wellPanel(
        h3("Student evaluation"),
        uiOutput("evaluation")
      )
    ),
    column(
      6,
      wellPanel(
        h3("Rubric"),
        selectInput("cID", "Competency", choices = NULL),
        uiOutput("compDescr"),
        mod_highlight_ui("highlights", "evaluation", "Text evidence"),
        radioButtons("spec", "Specificity score", choices = c(1:4), inline = T),
        radioButtons("util", "Utility score", choices = c(1:3), inline = T),
        radioButtons("sent", "Sentiment score", choices = c(1:5), inline = T),
        textAreaInput(
          "comment",
          "Optional comment",
          placeholder = "Use this for highlighting rubric issues"
        ),
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

  reviewScores <- reactiveVal()

  # Highlight selection module
  defaultEvidence <- reactiveVal(c())
  resetSel <- reactiveVal()
  txtEvidence <- mod_highlight_server(
    "highlights",
    defaults = defaultEvidence,
    reset = resetSel
  )

  # Populate reviewers
  x <- tbl(conn, "reviewer") |>
    filter(human == 1) |>
    select(id, username) |>
    collect()

  updateSelectInput(session, "reviewerID", choices = setNames(x$id, x$username))

  # Populate evaluations
  observeEvent(c(input$reviewerID, input$includeCompeted), {
    reviews <- tbl(conn, "review_assignment") |>
      filter(reviewer_id == as.integer(input$reviewerID)) |>
      arrange(statusCode, evaluation_id) |>
      collect() |>
      mutate(
        descr = sprintf(
          "Evaluation %s - %s",
          evaluation_id,
          case_when(
            statusCode == 0 ~ "New",
            statusCode == 1 ~ "In progress",
            statusCode == 2 ~ "Completed",
            statusCode == -1 ~ "Completed with flag",
            TRUE ~ "Error"
          )
        )
      )

    lblInfo <- reviews |>
      filter(statusCode < 3) |>
      group_by(statusCode) |>
      summarise(n = n())

    lblInfo <- data.frame(statusCode = 0:2) |>
      left_join(lblInfo, by = "statusCode") |>
      mutate(n = ifelse(is.na(n), 0, n)) |>
      pull(n)

    # Set the eval IDs
    updateSelectInput(
      session,
      "reviewID",
      label = sprintf(
        "%i to start - %i in progress - %i competed",
        lblInfo[1],
        lblInfo[2],
        lblInfo[3]
      ),
      choices = setNames(
        reviews$id,
        reviews$descr
      )
    )
  })

  # Get the prompt and use it to create the rubric
  prompt <- eventReactive(
    input$reviewID,
    {
      reviewID <- as.integer(input$reviewID)
      print("HI")
      # Get any previous scores
      scores <- tbl(conn, "review_score") |>
        filter(review_assignment_id == reviewID) |>
        select(competency_id, specificity, utility, sentiment, text_matches) |>
        collect()

      # Check if the eval was already reviewed and use the same prompt version
      review_prompt_id <- tbl(conn, "review_assignment") |>
        filter(id == as.integer(input$reviewID)) |>
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
        ),
        selected = ifelse(
          length(scores$specificity) == 0,
          1,
          scores$specificity
        )
      )
      updateRadioButtons(
        inputId = "util",
        label = parsed$content$scoring$util$desciption,
        choices = setNames(
          1:length(parsed$content$scoring$util$options),
          parsed$content$scoring$util$options
        ),
        selected = ifelse(length(scores$utility) == 0, 1, scores$utility)
      )
      updateRadioButtons(
        inputId = "sent",
        label = parsed$content$scoring$sent$desciption,
        choices = setNames(
          1:length(parsed$content$scoring$sent$options),
          parsed$content$scoring$sent$options
        ),
        selected = ifelse(length(scores$sentiment) == 0, 1, scores$sentiment)
      )

      # Set the highlighted text list in the module
      txt <- str_split(scores$text_matches, "; ")
      if (length(txt) == 0) {
        txt <- list(c())
      }
      defaultEvidence(txt[[1]])
      resetSel(Sys.time())

      # Update the competency review var
      reviewScores(scores)

      list(text = text, parsed = parsed)
    },
    ignoreInit = T
  )

  # Reset rubric on competency change
  observeEvent(input$cID, {
    if (
      is.null(reviewScores()) ||
        !as.integer(input$cID) %in% reviewScores()$competency_id
    ) {
      updateRadioButtons(inputId = "spec", selected = 1)
      updateRadioButtons(inputId = "util", selected = 1)
      updateRadioButtons(inputId = "sent", selected = 1)
      updateActionButton(inputId = "add", label = "Add competency review")
      defaultEvidence(c())
      resetSel(Sys.time())
      return()
    }
    prev <- reviewScores() |> filter(competency_id == as.integer(input$cID))
    updateRadioButtons(inputId = "spec", selected = prev$spec)
    updateRadioButtons(inputId = "util", selected = prev$util)
    updateRadioButtons(inputId = "sent", selected = prev$sent)
    updateActionButton(inputId = "add", label = "Update competency review")
    defaultEvidence(str_split(prev$text_matches, "; ")[[1]])
    resetSel(Sys.time())
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
    req(input$reviewID)
    # Get the evaluation ID for the assigned review
    evalID <- tbl(conn, "review_assignment") |>
      filter(id == as.integer(input$reviewID)) |>
      pull(evaluation_id)

    div(
      HTML(
        dbGetEvals(
          ids = evalID,
          dbInfo = dbInfo,
          redacted = T,
          includeQuestions = input$showQuestions,
          html = T,
          subtitleTag = "b"
        ) |>
          pull(evaluation)
      ),
      style = "max-height: 700px; overflow-y: auto;"
    )
  })

  # Add or update a competency review
  observeEvent(input$add, {
    evidence <- str_trim(txtEvidence()$text) |> paste(collapse = "; ")
    if (evidence == "") {
      showModal(modalDialog(
        HTML(
          "Please make sure to provide miminal text evidence by higlighting",
          "pieces of text and clicking the 'Add highlighted' button in the rubric"
        ),
        title = "Text evidence missing"
      ))
    }

    req(evidence != "")

    #Update the competency reviews data frame
    if (input$cID %in% reviewScores()$competency_id) {
      reviewScores(reviewScores()[
        -c(input$cID %in% reviewScores()$competency_id),
      ])
    }

    changed <- data.frame(
      competency_id = as.integer(input$cID),
      specificity = as.integer(input$spec),
      utility = as.integer(input$util),
      sentiment = as.integer(input$sent),
      text_matches = evidence
    )

    id <- reviewScores() |>
      filter(competency_id == as.integer(input$cID)) |>
      pull(id)

    if (length(id) > 0) {
      changed$id = id
    }

    #TODO add / update review
    changed <- dbReviewScore(dbInfo, changed)

    reviewScores(rbind(changed, reviewScores()))

    updateActionButton(inputId = "add", label = "Update competency review")
  })

  output$review <- renderDT(
    {
      req(reviewScores())
      # Replace ID with the competency description
      reviewScores() |>
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
        select(-competency_id, -review_score_id) |>
        select(competency, everything())
    },
    rownames = F
  )
}

shinyApp(ui, server)
