# devtools::install_github("posit-dev/plumber2")

#* Echo the parameter that was sent in
#*
#* @get /echo/<msg>
#*
#* @param msg:string The message to echo back.
#*
function(msg) {
  list(
    msg = paste0("The message is: '", msg, "'")
  )
}

#* @shiny /dev_app
shiny::shinyAppDir(system.file("examples-shiny", "01_hello", package = "shiny"))
