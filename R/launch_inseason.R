#' Launch the inseason dashboard
#'
#' Runs the Shiny application shipped with this package.
#'
#' @param ... Passed to [shiny::runApp()], e.g. `launch.browser = TRUE`.
#' @param quiet Passed to [shiny::runApp()].
#'
#' @return (Invisibly) the result of [shiny::runApp()].
#' @export
launch_inseason <- function(..., quiet = TRUE) {
  app_dir <- system.file("shiny", "inseason", package = "inseasonDashboard")
  if (app_dir == "") {
    stop("Could not find app directory. Is the package installed correctly?")
  }
  shiny::runApp(app_dir, ..., quiet = quiet)
}
