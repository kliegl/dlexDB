#' Launch the dlexDB Shiny App
#'
#' Opens a graphical interface to search and filter the dlexDB database.
#' @export
dlex_run_app <- function() {
  app_dir <- system.file("shiny", "dlex_app", package = "dlexDB")
  if (app_dir == "") {
    stop("Could not find the app directory. Try re-installing `dlexDB`.")
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
