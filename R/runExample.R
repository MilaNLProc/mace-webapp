#' @export

runExample <- function() {
  options(shiny.port = 8787)
  appDir <- system.file("shiny-examples", "model_app", package = "testpackage")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `testpackage`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", host = '0.0.0.0')
}
