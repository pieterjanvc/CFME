library(plumber2)

x <- api() |>
  api_shiny(
    "/ui/",
    shiny::shinyAppDir(
      system.file("examples-shiny", "01_hello", package = "shiny")
    )
  )

x |> api_run()
