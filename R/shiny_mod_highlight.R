#' Module UI for text highlights
#'
#' UI has a button with underneath a compact element where a list of selected
#' peices of text are displayed and can be removed again.
#'
#' @param id Module ID
#' @param element (Optional) Limit text selection to a specific HTML element ID
#' and its children (don't provide the #)
#' @param label (Default = = "Text selections"). Label for the UI list
#' @param info (Has default) HTML to display underneath the label
#'
#' @import shiny
#'
#' @returns Shiny UI element
#' @export
#'
mod_highlight_ui <- function(
  id,
  element,
  label = "Text evidence",
  info = "<i>Select pieces of text and click the 'Add highlighted' button to add them to the list below</i>"
) {
  # When no element is set '' JS wil choose the body as default
  if (missing(element)) {
    element = ""
  }

  ns <- NS(id)
  tagList(
    # Capture highlighted text on the screen
    tags$script(HTML(sprintf(
      "
  document.addEventListener('mouseup', function() {
    var selBox = document.getElementById('%s'); // target element
    if (!selBox) {
      el = document.body;
    }
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
    tags$label(label, class = "control-label"),
    HTML(info),
    actionButton(ns("addSel"), "Add highlighted", width = "150px"),
    div(id = ns("selList"))
  )
}

#' Module server for text highlights
#'
#' @param id Module ID
#' @param defaults A (reactive) vector of default list options
#' @param reset An optional reactive variable that will rest to the defaults when triggered
#'
#' @import shiny dplyr
#'
#' @returns A reactive data frame with selected pieces of text and an ID in the
#' order they were listed on the page
#'
#' @export
#'
mod_highlight_server <- function(id, defaults, reset = reactiveVal()) {
  moduleServer(id, function(input, output, session) {
    # Keep track of how many highlights were saved
    counter <- reactiveVal(0)
    if (!is.reactive(defaults)) {
      defaults <- reactiveVal(defaults)
    }

    # Current list of saved higlights
    sels <- reactiveVal(data.frame(
      id = integer(),
      btn = character(),
      text = character(),
      new = logical()
    ))

    # Function to text to the UI list
    addToList <- function(id, countIds, values) {
      for (i in 1:length(countIds)) {
        insertUI(
          paste0("#", NS(id, "selList")),
          "afterBegin",
          tags$div(
            actionButton(
              paste0(NS(id, "selList"), countIds[i], "-del"),
              label = NULL,
              icon = icon("trash"),
              style = "padding: 3px;"
            ),
            values[i],
            id = paste0(NS(id, "selList"), countIds[i])
          )
        )
      }
    }

    observeEvent(
      c(defaults(), reset()),
      {
        n <- length(defaults())

        # Remove old UI
        for (btn in sels()$btn) {
          removeUI(paste0("#", NS(id, str_remove(btn, "-del"))))
        }

        # Add to result
        if (n > 0) {
          # Add new UI
          addToList(id, counter() + 1:n, rev(defaults()))

          sels(data.frame(
            id = counter() + 1:n,
            btn = paste0("selList", counter() + 1:n, "-del"),
            text = rev(defaults()),
            new = T
          ))
          counter(counter() + n)
        } else {
          sels(data.frame(
            id = integer(),
            btn = character(),
            text = character(),
            new = logical()
          ))
        }
      },
      ignoreNULL = F
    )

    # When the add button is clicked add highighted text to the list
    observeEvent(input$addSel, {
      if (input$highlighted_text == "") {
        showModal(modalDialog(
          title = "Selection issue",
          "You must select text from the expected area to proceed"
        ))
      }

      req(input$highlighted_text != "")

      counter(counter() + 1)

      # Add to the UI lsit
      . <- addToList(id, counter(), input$highlighted_text)

      # Update the resulting dataframe
      sels(rbind(
        sels(),
        data.frame(
          id = counter(),
          btn = paste0("selList", counter(), "-del"),
          text = input$highlighted_text,
          new = T
        )
      ))
    })

    # Dynamically add observations for delete buttons
    observe({
      n <- length(sels()$new == T)
      req(n > 0)
      toUpdate <- sels() |> filter(new)
      lapply(toUpdate$btn, function(btn) {
        # Use local() to capture loop variable correctly
        local({
          observeEvent(input[[btn]], {
            sels(sels()[sels()$btn != btn, ])
            removeUI(paste0("#", NS(id, str_remove(btn, "-del"))))
          })
        })
      })
      sels(sels() |> mutate(new = F))
    })

    # Return the data frame with seved highlights
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
