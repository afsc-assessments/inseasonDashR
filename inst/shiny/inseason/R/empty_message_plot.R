#' Create an empty plot with a centered message
#'
#' This helper function returns a ggplot object containing a centered
#' informational message. It is intended for use when no data are available
#' for plotting (e.g., no length data for a selected species and time period),
#' allowing Quarto / R Markdown workflows to continue without errors while
#' clearly communicating the reason for the empty figure.
#'
#' @param text Character string giving the message to display in the center
#'   of the plot.
#'
#' @return A ggplot object with no axes or background and centered text.
#'
#' @examples
#' empty_message_plot()
#'
#' empty_message_plot(
#'   "No length data available for this time period for this species"
#' )
#'
#' @export
empty_message_plot <- function(
  text = "No length data available for this time period for this species"
) {

  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = text,
      size = 5,
      hjust = 0.5,
      vjust = 0.5
    ) +
    ggplot2::theme_void()
}
