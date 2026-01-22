#' Make NOAA-style catch figure (date-range driven; points or grid)
#'
#' Wrapper around:
#' \itemize{
#'   \item \code{connect_afsc_akfin()}
#'   \item \code{get_catch_data()} (date-driven; year inferred from date_min inside SQL)
#'   \item \code{plot_catch_locations_noaa_np_date()}      (points)
#'   \item \code{plot_catch_locations_noaa_np_grid_date()} (grid)
#' }
#'
#' All temporal filtering is controlled exclusively by \code{date_min} and
#' \code{date_max} (formatted as "mm/dd/yyyy").
#'
#' @param fsh_sp_label Species code(s) passed to \code{get_catch_data()}.
#' @param species_name Character label for the plot title.
#' @param date_min Optional start date (inclusive) as "mm/dd/yyyy".
#' @param date_max Optional end date (inclusive) as "mm/dd/yyyy".
#' @param figure One of \code{"points"} or \code{"grid"}.
#'
#' @param region Regions to plot (e.g., "AI","BS","GOA","BSWGOA").
#' @param gear Gear types to plot: \code{"Trawl"}, \code{"Pot"}, \code{"Longline"}.
#'
#' @param size_range Point plot only: size scaling range for weight (metric tons).
#' @param cell_km Grid plot only: grid cell size (km).
#' @param pad_frac Grid plot only: padding fraction for map extent.
#'
#' @param conn Optional existing connection list with \code{$afsc} and \code{$akfin}.
#' @param disconnect If TRUE and wrapper created the connections, disconnect on exit.
#' @param tz Timezone used for date parsing inside \code{get_catch_data()}.
#' @param return_data If TRUE, return pulled data along with plot.
#' @param return_sql If TRUE, include SQL text from \code{get_catch_data()}.
#'
#' @return
#' If \code{return_data = FALSE}: a ggplot object. \cr
#' If \code{return_data = TRUE}: a list with elements
#' \itemize{
#'   \item \code{plot}
#'   \item \code{data_o}
#'   \item \code{data_em}
#'   \item \code{conn}
#'   \item \code{sql} (optional)
#' }
#'
#' @export
make_catch_figure_date <- function(
  fsh_sp_label,
  species_name,
  date_min = NULL,
  date_max = NULL,
  figure = c("points", "grid"),
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  size_range = c(1, 6),
  cell_km = 20,
  pad_frac = 0.08,
  conn = NULL,
  disconnect = TRUE,
  tz = "UTC",
  return_data = FALSE,
  return_sql = FALSE
) {

  figure <- match.arg(figure)
  stopifnot(is.character(species_name), length(species_name) == 1)

  # ---- connect ----
  created_conn <- FALSE
  if (is.null(conn)) {
    conn <- connect_afsc_akfin()
    created_conn <- TRUE
  }

  if (isTRUE(disconnect) && isTRUE(created_conn)) {
    on.exit({
      try(DBI::dbDisconnect(conn$afsc), silent = TRUE)
      try(DBI::dbDisconnect(conn$akfin), silent = TRUE)
    }, add = TRUE)
  }

  # ---- pull data (date-driven) ----
  pulled <- get_catch_data(
    afsc = conn$afsc,
    akfin = conn$akfin,
    fsh_sp_label = fsh_sp_label,
    date_min = date_min,
    date_max = date_max,
    tz = tz,
    return_sql = isTRUE(return_sql)
  )

  data_o  <- pulled$data_o
  data_em <- pulled$data_em

  # ---- plot ----
  if (figure == "points") {
    p <- plot_catch_locations_noaa_np_date(
      data_o = data_o,
      data_em = data_em,
      species_name = species_name,
      date_min = date_min,
      date_max = date_max,
      region = region,
      gear = gear,
      size_range = size_range
    )
  } else {
    p <- plot_catch_locations_noaa_np_grid_date(
      data_o = data_o,
      data_em = data_em,
      species_name = species_name,
      date_min = date_min,
      date_max = date_max,
      region = region,
      gear = gear,
      cell_km = cell_km,
      pad_frac = pad_frac
    )
  }

  if (!isTRUE(return_data)) return(p)

  out <- list(
    plot = p,
    data_o = data_o,
    data_em = data_em,
    conn = conn
  )

  if (isTRUE(return_sql) && !is.null(pulled$sql)) {
    out$sql <- pulled$sql
  }

  out
}
