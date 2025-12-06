library(bslib)
library(shiny)
library(DT)
library(stringr)

# https://rstudio.github.io/bslib/articles/cards/index.html

dbInfo <- "../local/test.db"

ui <- page_fluid(
  theme = bs_theme(preset = "journal"),
  tags$style(HTML(
    "
    .control-label {
      font-weight: bold;
    }
  /* Make sure the dropdowns are not clipped by parent container */
  .html-fill-item {
    overflow: visible !important;
  }
  "
  )),
  div(
    mod_dbSetup_ui("dbMod", "link"),
    style = "position:absolute;right:20px;top=0"
  ),
  # Layout
  layout_columns(
    card(
      card_header("1. Select Reviewer"),

      selectInput("reviewerID", "Reviewer", choices = c()),
      HTML(
        "<i>You are the <b>reviewer</b> assessing the quality of an evaluation. ",
        "The <b>evaluator</b> is the person who wrote the student evaluation below</i>"
      )
    ),
    card(
      card_header("2. Pick an evaluation"),
      selectInput(
        "reviewID",
        "0 to start - 0 in progress - 0 competed",
        choices = c(),
        width = "100%"
      ),
      # checkboxInput("includeCompeted", "List completed", value = F),
      checkboxInput("showQuestions", "Show questions", value = T)
    )
  ),

  layout_columns(
    card(
      card_header("Student evaluation"),
      uiOutput("evaluation")
    ),
    navset_card_tab(
      nav_panel(
        "1. Competencies",
        selectInput("cID", "Competency", choices = NULL, width = "100%"),
        uiOutput("compDescr"),
        mod_highlight_ui(
          "highlights",
          "evaluation",
          "Text evidence (required)"
        ),
        radioButtons(
          "spec",
          "Specificity score",
          choices = c(1:4),
          inline = T
        ),
        textAreaInput(
          "competencyComment",
          "Optional competency comment",
          placeholder = "Not part of the rubric, used internally",
          width = "100%"
        ),
        actionButton("addComp", "Save competency review"),
        id = "compTab"
      ),
      nav_panel(
        "2. Overall Scores",
        radioButtons(
          "util",
          "Utility score",
          choices = c(1:3),
          inline = F,
          width = "100%"
        ),
        radioButtons(
          "sent",
          "Sentiment score",
          choices = c(1:5),
          inline = F,
          width = "100%"
        ),
        textAreaInput(
          "reviewComment",
          "Optional review comment",
          placeholder = "Not part of the rubric, used internally",
          width = "100%"
        ),
        actionButton("addOverall", "Add overall review"),
        id = "overallTab"
      ),
      nav_panel(
        "3. Submit",
        uiOutput("summary"),
        tags$b("OPTIONAL"),
        checkboxInput("flag", "Add issue flag"),
        actionButton("complete", "Mark as complete"),
        id = "submitTab"
      )
    )
  )
)

server <- function(input, output, session) {
  # DB Setup module (only needed for download)
  . <- mod_dbSetup_server(
    "dbMod",
    localFolder = "../local",
    tempFolder = "../local",
    schema = "../inst/cfme.sql",
    useDB = dbInfo
  )
  # DB Connection
  conn <- dbGetConn(dbInfo, session = session)

  reviewScores <- reactiveVal()
  prompt <- reactiveVal()

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
    # filter(human == 1) |>
    select(id, username) |>
    collect()

  updateSelectInput(session, "reviewerID", choices = setNames(x$id, x$username))

  # Populate review selection dropdown
  updateReviewID <- function(selected) {
    reviews <- tbl(conn, "review_assignment") |>
      filter(reviewer_id == as.integer(input$reviewerID)) |>
      left_join(
        tbl(conn, "evaluation") |>
          select(evaluation_id = id, complete, summary_flg),
        by = "evaluation_id"
      ) |>
      collect() |>
      arrange(
        match(statusCode, c(1, 0, -1, 2, setdiff(c(0:2), unique(statusCode)))),
        evaluation_id
      ) |>
      mutate(
        descr = sprintf(
          "%s (%s %s) - %s",
          evaluation_id,
          ifelse(complete == 1, "complete", "incomplete"),
          ifelse(summary_flg == 1, "summative", "formative"),
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

    # Calculate each for status
    lblInfo <- data.frame(statusCode = -1:2) |>
      left_join(lblInfo, by = "statusCode") |>
      mutate(n = ifelse(is.na(n), 0, n)) |>
      pull(n)

    # Set the eval IDs
    updateSelectInput(
      session,
      "reviewID",
      label = sprintf(
        "%i to start - %i in progress - %i competed",
        lblInfo[2],
        lblInfo[3],
        lblInfo[4] + lblInfo[1]
      ),
      choices = setNames(
        reviews$id,
        reviews$descr
      ),
      selected = ifelse(missing(selected), reviews$id[1], selected)
    )
  }

  observeEvent(input$reviewerID, {
    updateReviewID()
  })

  # Get the prompt and use it to create the rubric
  observeEvent(
    input$reviewID,
    {
      reviewID <- as.integer(input$reviewID)

      # Get any previous scores
      review_assingment <- tbl(conn, "review_assignment") |>
        filter(id == reviewID) |>
        collect()

      compScores <- tbl(conn, "competency_score") |>
        filter(review_assignment_id == reviewID) |>
        collect()

      compText <- tbl(conn, "competency_text") |>
        filter(competency_score_id %in% local(compScores$id)) |>
        collect()

      # Check if the eval was already reviewed and use the same prompt version
      review_prompt_id <- review_assingment$review_prompt_id

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

      # Update inputs based on rubric phrasing

      # Competency list
      updateSelectInput(
        inputId = "cID",
        choices = setNames(
          1:6,
          sapply(parsed$content$competencies, "[[", "name")
        )
      )

      # Specificity score
      #  The value is adjusted in the input$cID function
      updateRadioButtons(
        inputId = "spec",
        label = parsed$content$compScore$spec$desciption,
        choices = setNames(
          1:length(parsed$content$compScore$spec$options),
          parsed$content$compScore$spec$options
        ),
        selected = NULL
      )

      # Overall scores
      updateRadioButtons(
        inputId = "util",
        label = parsed$content$overallScore$util$desciption,
        choices = setNames(
          1:length(parsed$content$overallScore$util$options),
          parsed$content$overallScore$util$options
        ),
        selected = if (length(review_assingment$utility) == 0) {
          character(0)
        } else {
          review_assingment$utility
        }
      )
      updateRadioButtons(
        inputId = "sent",
        label = parsed$content$overallScore$sent$desciption,
        choices = setNames(
          1:length(parsed$content$overallScore$sent$options),
          parsed$content$overallScore$sent$options
        ),
        selected = if (length(review_assingment$sentiment) == 0) {
          character(0)
        } else {
          review_assingment$sentiment
        }
      )

      updateTextAreaInput(
        input = "reviewComment",
        value = review_assingment$note
      )

      # Submission tab
      updateCheckboxInput(
        inputId = "flag",
        value = review_assingment$statusCode == -1
      )

      updateActionButton(
        inputId = "complete",
        label = ifelse(
          review_assingment$statusCode %in% c(-1, 2),
          "Resubmit",
          "Mark as complete"
        )
      )

      # Update the competency review var
      reviewScores(list(
        overallScores = review_assingment,
        compScores = compScores,
        compText = compText
      ))

      prompt(list(text = text, parsed = parsed))
    },
    ignoreInit = T
  )

  # Update the rubric on competency change
  observeEvent(c(input$cID, input$reviewID), {
    req(reviewScores())
    # Get the previous values (if any)
    compScores <- reviewScores()$compScores |>
      filter(competency_id == as.integer(input$cID))

    compText <- reviewScores()$compText |>
      filter(competency_score_id %in% compScores$id)

    # Update all competency scoring values
    defaultEvidence(compText$text_match)
    resetSel(Sys.time())

    updateRadioButtons(
      inputId = "spec",
      selected = if (nrow(compScores) == 0) {
        character(0)
      } else {
        compScores$specificity
      }
    )

    updateTextAreaInput(
      input = "competencyComment",
      value = ifelse(nrow(compScores) == 0, "", compScores$note)
    )

    updateActionButton(
      inputId = "addComp",
      label = ifelse(
        nrow(compScores) == 0,
        "Add competency review",
        "Update competency review"
      )
    )
  })

  # This is the definition underneath the selected competency
  output$compDescr <- renderUI({
    tagList(tags$i(
      prompt()$parsed$content$competencies[[input$cID]]$description
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
          conn = conn,
          redacted = T,
          includeQuestions = input$showQuestions,
          html = T,
          subtitleTag = "b"
        ) |>
          pull(evaluation)
      ),
      style = "max-height: 75vh; overflow-y: auto;"
    )
  })

  # Add or update a competency review
  observeEvent(input$addComp, {
    evidence <- str_trim(txtEvidence()$text)
    if (length(evidence) == 0) {
      showModal(modalDialog(
        HTML(
          "Please make sure to provide miminal text evidence by higlighting",
          "pieces of text and clicking the 'Add highlighted' button in the rubric"
        ),
        title = "Text evidence missing"
      ))
    }

    req(length(evidence) > 0)

    if (is.null(input$spec)) {
      showModal(modalDialog(
        HTML("Please make sure to select a specificity score"),
        title = "Text evidence missing"
      ))
    }

    req(input$spec)

    comment <- str_trim(input$competencyComment)
    compScores <- data.frame(
      review_assignment_id = as.integer(input$reviewID),
      competency_id = as.integer(input$cID),
      specificity = as.integer(input$spec),
      note = ifelse(comment == "", NA, comment)
    )

    compText <- data.frame(
      review_assignment_id = as.integer(input$reviewID),
      competency_id = as.integer(input$cID),
      text_match = evidence
    )

    scores <- dbReviewUpdate(
      conn = conn,
      statusCode = 1,
      compScores = compScores,
      compText = compText,
      removeNotListed = F,
      commit = T
    )

    # Update the reviewScores var (used in submission tab)
    reviewScores(scores)

    updateActionButton(inputId = "addComp", label = "Update competency review")
    updateReviewID(input$reviewID)

    showNotification(sprintf("Competency updated"), type = "message")
  })

  # Add or update the overall scores
  observeEvent(input$addOverall, {
    # Check
    if (is.null(input$util) || is.null(input$sent)) {
      showModal(modalDialog(
        HTML("Please make sure to select a Utility adn Sentiment score"),
        title = "Score missing"
      ))
    }

    req(!is.null(input$util) && !is.null(input$sent))

    overallScores <- data.frame(
      id = as.integer(input$reviewID),
      utility = as.integer(input$util),
      sentiment = as.integer(input$sent),
      note = str_trim(input$reviewComment)
    )

    scores <- dbReviewUpdate(
      conn = conn,
      statusCode = 1,
      overallScores = overallScores,
      removeNotListed = F,
      commit = T
    )

    reviewScores(scores)

    updateActionButton(inputId = "addOverall", label = "Update overall review")
    updateReviewID(input$reviewID)

    showNotification(sprintf("Scores updated"), type = "message")
  })
  # The summary of the review scores before submitting
  output$summary <- renderUI({
    req(reviewScores())

    # Add prompt text to IDs and scores
    promptText <- prompt()$parsed$content

    compScores <- reviewScores()$compScores |>
      select(specificity, competency_id) |>
      left_join(
        data.frame(
          competency_id = 1:6,
          competency = sapply(promptText$competencies, "[[", "name")
        ),
        by = "competency_id"
      ) |>
      left_join(
        data.frame(
          specificity = 1:length(promptText$compScore$spec$options),
          specificity_score = promptText$compScore$spec$options
        ),
        by = "specificity"
      ) |>
      select(competency, score = specificity_score)

    overallScores <- reviewScores()$overallScores

    # ACTUAL UI
    tagList(
      tags$h3("Review Summary"),
      tags$b("COMPETENCIES"),
      datatable(
        compScores,
        options = list(paging = FALSE, searching = FALSE, info = FALSE),
        selection = "none",
        rownames = FALSE
      ),
      tags$b("UTILITY"),
      p(promptText$overallScore$util$options[overallScores$utility]),
      tags$b("SENTIMENT"),
      p(promptText$overallScore$sent$options[overallScores$sentiment]),
      tags$hr()
    )
  })

  #When the complete button is clicked
  observeEvent(input$complete, {
    if (
      (nrow(reviewScores()$compScores) == 0 ||
        nrow(reviewScores()$overallScores) == 0) &
        !input$flag
    ) {
      showModal(modalDialog(
        "You must have reviewed at least one competency or checked the issue",
        "flag in order to compete a review",
        title = "Error"
      ))
      return()
    }

    data <- data.frame(
      id = input$reviewID,
      statusCode = ifelse(input$flag, -1, 2)
    )
    tbl_update(data, conn, "review_assignment", returnData = F)

    updateReviewID(input$reviewID)
    showNotification("Changes marked as complete", type = "message")
  })
}

shinyApp(ui, server)
