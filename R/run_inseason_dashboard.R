#' Run the inseason Shiny dashboard
#'
#' Launches the AFSC inseason dashboard bundled with this package.
#' The app lives under \code{inst/shiny/inseason/}.
#'
#' @param launch.browser Logical; open in a browser. Default TRUE.
#' @param ... Passed to \code{shiny::runApp()}.
#'
#' @return Invisibly returns the Shiny app object.
#'
#' @examples
#' \dontrun{
#' run_inseason_dashboard()
#' }
#'
#' @export
run_inseason_dashboard <- function(launch.browser = TRUE, ...) {
  app_dir <- system.file("shiny", "inseason", package = utils::packageName())
  if (app_dir == "") {
    stop("Shiny app directory not found in installed package. ",
         "Expected inst/shiny/inseason/ during package build.")
  }
  shiny::runApp(appDir = app_dir, launch.browser = launch.browser, ...)
}
