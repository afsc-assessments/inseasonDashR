#' Make a NOAA-style North Pacific catch map from AFSC + AKFIN pulls
#'
#' Wrapper around:
#' \itemize{
#'   \item \code{connect_afsc_akfin()}
#'   \item \code{get_catch_data()}
#'   \item \code{plot_catch_locations_noaa_np_date()} (points)
#'   \item \code{plot_catch_locations_noaa_np_grid_date()} (grid)
#' }
#'
#' This wrapper:
#' \itemize{
#'   \item Connects to AFSC and AKFIN using keyring credentials
#'   \item Pulls observer + EM catch data for a species and date range
#'   \item Produces either a point map or a gridded map
#' }
#'
#' @param fsh_sp_label Numeric/integer species code(s) used in SQL IN clause.
#' @param species_name Character string used in the plot title.
#' @param date_min Optional start date (inclusive) as \code{"mm/dd/yyyy"}.
#' @param date_max Optional end date (inclusive) as \code{"mm/dd/yyyy"}.
#' @param plot_type Either \code{"points"} or \code{"grid"}.
#' @param region Character vector of regions: \code{"AI"}, \code{"BS"}, \code{"GOA"}, \code{"BSWGOA"}.
#' @param gear Character vector of gear types: \code{"Trawl"}, \code{"Pot"}, \code{"Longline"}.
#'
#' @param facet_gear Logical; if TRUE, facet the plot by gear (passed to plot funcs).
#' @param show_titles Logical; if FALSE, remove titles (passed to plot funcs).
#' @param show_label Logical; if FALSE, remove upper-right label (passed to plot funcs).
#'
#' @param size_range Points-only: numeric length-2, point size range for weight scaling (mt).
#' @param cell_km Grid-only: grid cell size in km.
#' @param pad_frac Map padding fraction around data extent (passed to plot funcs where supported).
#'
#' @param tz Passed to \code{get_catch_data()} for date parsing.
#' @param return_sql Passed to \code{get_catch_data()} (default TRUE).
#' @param keep_connections If FALSE (default), DB connections are closed on exit.
#' @param return_data If TRUE, return a list including data + SQL in addition to the plot.
#'
#' @return By default, a \code{ggplot} object.
#'   If \code{return_data=TRUE}, returns a list with \code{$plot}, \code{$data_o}, \code{$data_em},
#'   and optionally \code{$sql}.
#' @export
make_catch_map <- function(
  fsh_sp_label,
  species_name,
  date_min = NULL,
  date_max = NULL,
  plot_type = c("points", "grid"),
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  facet_gear = FALSE,
  show_titles = TRUE,
  show_label = TRUE,
  size_range = c(1, 6),
  cell_km = 20,
  pad_frac = 0.08,
  tz = "UTC",
  return_sql = TRUE,
  keep_connections = FALSE,
  return_data = FALSE
) {

  plot_type <- match.arg(plot_type)

  # ---- connect ----
  con <- connect_afsc_akfin()

  if (!isTRUE(keep_connections)) {
    on.exit({
      try(DBI::dbDisconnect(con$afsc), silent = TRUE)
      try(DBI::dbDisconnect(con$akfin), silent = TRUE)
    }, add = TRUE)
  }

  # ---- pull data ----
  pulled <- get_catch_data(
    afsc = con$afsc,
    akfin = con$akfin,
    fsh_sp_label = fsh_sp_label,
    date_min = date_min,
    date_max = date_max,
    tz = tz,
    return_sql = return_sql
  )

  data_o  <- pulled$data_o
  data_em <- pulled$data_em

  # ---- plot ----
  if (plot_type == "points") {
    p <- plot_catch_locations_noaa_np_date(
      data_o = data_o,
      data_em = data_em,
      species_name = species_name,
      date_min = date_min,
      date_max = date_max,
      region = region,
      gear = gear,
      size_range = size_range,
      facet_gear = facet_gear,
      show_titles = show_titles,
      show_label = show_label,
      pad_frac = pad_frac
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
      pad_frac = pad_frac,
      facet_gear = facet_gear,
      show_titles = show_titles,
      show_label = show_label
    )
  }

  if (isTRUE(return_data)) {
    out <- list(
      plot = p,
      data_o = data_o,
      data_em = data_em
    )
    if (isTRUE(return_sql) && !is.null(pulled$sql)) out$sql <- pulled$sql
    return(out)
  }

  p
}
