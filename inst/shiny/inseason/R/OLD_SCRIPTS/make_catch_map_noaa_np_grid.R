#' Make NOAA-style gridded catch map (Observer + EM) in one call
#'
#' Convenience wrapper that:
#' \enumerate{
#'   \item connects to AFSC + AKFIN (optional),
#'   \item pulls Observer + EM catch data for a species and start year, and
#'   \item plots gridded catch weight on a NOAA-style North Pacific map.
#' }
#'
#' This wraps \code{\link{connect_afsc_akfin}}, \code{\link{get_catch_data}}, and
#' \code{\link{plot_catch_locations_noaa_np_grid}}.
#'
#' @param species Species code(s) used by your SQL templates (passed to \code{get_catch_data()}).
#' @param species_name Character scalar for the plot title (e.g., "Pacific cod").
#' @param first_year First year (inclusive) used in the SQL pull (passed to \code{get_catch_data()}).
#' @param conn Optional list with \code{$afsc} and \code{$akfin} DBI connections.
#'   If \code{NULL}, the wrapper calls \code{connect_afsc_akfin()}.
#' @param disconnect Logical; if \code{TRUE} and the wrapper created the connections,
#'   it will disconnect them on exit. Default \code{TRUE}.
#' @param pkg Package name used to locate SQL files via your \code{sql_read()} helper
#'   (passed to \code{get_catch_data()}). Defaults to \code{utils::packageName()}.
#'
#' @param year Optional single year to plot (overrides \code{year_min} and \code{year_max}).
#' @param year_min Optional minimum year (inclusive) for plotting.
#' @param year_max Optional maximum year (inclusive) for plotting.
#' @param quarter Integer vector of calendar quarters to include (1–4).
#' @param region Character vector of management regions to include: \code{"AI"}, \code{"BS"},
#'   \code{"GOA"}, or \code{"BSWGOA"}.
#' @param gear Character vector of gear types to include: \code{"Trawl"}, \code{"Pot"}, \code{"Longline"}.
#' @param cell_km Grid cell size in kilometers for aggregation (default 20).
#' @param pad_frac Fractional padding added to the plotted data extent (default 0.08).
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' \dontrun{
#' # One-shot: connect, pull, plot (then disconnect)
#' p <- make_catch_map_noaa_np_grid(
#'   species = 202,
#'   species_name = "Pacific cod",
#'   first_year = 2007,
#'   year = 2023,
#'   quarter = c(1, 2),
#'   region = "BS",
#'   gear = "Pot"
#' )
#' print(p)
#'
#' # Reuse existing connections (no disconnect)
#' conn <- connect_afsc_akfin()
#' p <- make_catch_map_noaa_np_grid(
#'   species = 202,
#'   species_name = "Pacific cod",
#'   first_year = 2007,
#'   conn = conn,
#'   disconnect = FALSE,
#'   year_min = 2019,
#'   year_max = 2024,
#'   region = c("BS", "AI")
#' )
#' print(p)
#' }
make_catch_map_noaa_np_grid <- function(
  species,
  species_name,
  first_year,
  conn = NULL,
  disconnect = TRUE,
  pkg = utils::packageName(),
  year = NULL,
  year_min = NULL,
  year_max = NULL,
  quarter = c(1, 2, 3, 4),
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  cell_km = 20,
  pad_frac = 0.08
) {
  stopifnot(is.character(species_name), length(species_name) == 1)
  stopifnot(length(first_year) == 1)

  created_conn <- FALSE
  if (is.null(conn)) {
    conn <- connect_afsc_akfin()
    created_conn <- TRUE
  }

  # Disconnect only if we created the connections here
  if (isTRUE(disconnect) && isTRUE(created_conn)) {
    old_exit <- get0(".Last.value", ifnotfound = NULL, envir = baseenv())
    on.exit({
      # be forgiving if user already disconnected
      try(DBI::dbDisconnect(conn$afsc), silent = TRUE)
      try(DBI::dbDisconnect(conn$akfin), silent = TRUE)
      invisible(old_exit)
    }, add = TRUE)
  }

  pulls <- get_catch_data(
    afsc = conn$afsc,
    akfin = conn$akfin,
    fsh_sp_label = species,
    first_year = first_year
  )

  plot_catch_locations_noaa_np_grid(
    data_o = pulls$data_o,
    data_em = pulls$data_em,
    species_name = species_name,
    year = year,
    year_min = year_min,
    year_max = year_max,
    quarter = quarter,
    region = region,
    gear = gear,
    cell_km = cell_km,
    pad_frac = pad_frac
  )
}
