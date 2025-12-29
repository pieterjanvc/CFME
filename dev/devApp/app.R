library(shiny)

ui <- fluidPage(
  tags$h2("Hello World")
)

server <- function(input, output, session) {}

shinyApp(ui, server)
