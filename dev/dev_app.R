library(shiny)


mod_highlight_ui <- function(id, element) {
  ns <- NS(id)
  tagList(
    # Capture highlighted text on the screen
    tags$script(HTML(sprintf(
      "
  document.addEventListener('mouseup', function() {
    var selBox = document.getElementById('%s'); // target element
    var selection = window.getSelection();

    // Check if selection exists and is inside selBox
    if (selection.rangeCount > 0 && selBox.contains(selection.anchorNode) && selBox.contains(selection.focusNode)) {
      Shiny.setInputValue('%s', selection.toString());
    } else {
      Shiny.setInputValue('%s', ''); // optional: clear input if selection is outside
    }
  });
  ",
      element,
      ns("highlighted_text"),
      ns("highlighted_text")
    ))),
    actionButton(ns("addSel"), "Add highlighted"),
    div(id = ns("selList"))
  )
}

mod_highlight_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    counter <- reactiveVal(0)

    sels <- reactiveVal(data.frame(
      id = integer(),
      btn = character(),
      text = character()
    ))

    observeEvent(input$addSel, {
      if (input$highlighted_text == "") {
        showModal(modalDialog(
          title = "Selection issue",
          "You must select text from the expected area to proceed"
        ))
      }

      req(input$highlighted_text != "")

      counter(counter() + 1)
      insertUI(
        paste0("#", NS(id, "selList")),
        "afterBegin",
        tags$div(
          actionButton(
            paste0(NS(id, "selList"), counter(), "-del"),
            label = NULL,
            icon = icon("trash"),
            style = "padding: 3px;"
          ),
          input$highlighted_text,
          id = paste0(NS(id, "selList"), counter())
        )
      )

      sels(rbind(
        sels(),
        data.frame(
          id = counter(),
          btn = paste0("selList", counter(), "-del"),
          text = input$highlighted_text
        )
      ))
    })

    observe({
      n <- nrow(sels())
      req(n > 0)
      lapply(sels()$btn, function(btn) {
        # Use local() to capture loop variable correctly
        local({
          observeEvent(input[[btn]], {
            sels(sels()[sels()$btn != btn, ])
            removeUI(paste0("#", NS(id, str_remove(btn, "-del"))))
          })
        })
      })
    })

    return({
      reactive({
        df <- sels()

        if (nrow(df) > 0) {
          df <- df |>
            arrange(desc(id)) |>
            mutate(id = 1:n(), text = text, .keep = "none")
        }
      })
    })
  })
}

ui <- fluidPage(
  div("fheoifjeowi fhweifjewoi efwiwejqfo"),
  tags$div(
    id = "sel-box",
    "oke with student doctor [name_redact] about next steps in her growth. We agreed to continue focusing on moving from mainly reporting the data/events, to synthesizing them into an assessment and actionable items for the day. Also encouraged her to re-organize problem lists as needed and not always stick to what is written in the overnight admission note. She bega"
  ),
  mod_highlight_ui("test", "sel-box")
)

server <- function(input, output, session) {
  test <- mod_highlight_server("test")

  observe({
    print(test())
  })
}

shinyApp(ui, server)
