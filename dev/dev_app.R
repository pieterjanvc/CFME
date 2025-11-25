ui <- fluidPage(
  div("fheoifjeowi fhweifjewoi efwiwejqfo"),
  actionButton("btn", "Click"),
  tags$div(
    id = "sel-box",
    "oke with student doctor [name_redact] about next steps in her growth. We agreed to continue focusing on moving from mainly reporting the data/events, to synthesizing them into an assessment and actionable items for the day. Also encouraged her to re-organize problem lists as needed and not always stick to what is written in the overnight admission note. She bega"
  ),
  mod_highlight_ui("test", element = "sel-box")
)

server <- function(input, output, session) {
  selections <- reactiveVal(c("111", "222"))
  test <- mod_highlight_server(
    "test",
    defaults = selections,
    reset = reactive(input$btn)
  )

  # observeEvent(input$btn, {
  #   selections(sample(1:100, sample(1:3, 1)) |> as.character())
  # })

  observe({
    test()
  })
}

shinyApp(ui, server)
