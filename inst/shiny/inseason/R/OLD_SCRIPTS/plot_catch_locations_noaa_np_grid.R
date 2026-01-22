#' Plot gridded catch locations on a NOAA-style North Pacific map
#'
#' Aggregate observer and electronic monitoring (EM) catch data into a regular
#' spatial grid (default 20 km) and plot grid-cell total catch weight on a
#' NOAA-style North Pacific basemap using a Lambert Azimuthal Equal-Area
#' projection.
#'
#' Catch data are filtered to latitudes north of 50°N, optionally filtered by
#' year (single year or range), calendar quarter, management region, and gear
#' type. Catch weight is aggregated within grid cells on a consistent metric-ton
#' scale (observer weights are converted from kilograms internally).
#'
#' @details
#' \strong{Gear code mapping}
#' \itemize{
#'   \item 1–5 = Trawl
#'   \item 6   = Pot
#'   \item 8   = Longline
#' }
#'
#' \strong{Region mapping}
#' \itemize{
#'   \item \code{"AI"}  = Aleutian Islands (NMFS areas 540–544)
#'   \item \code{"BS"}  = Bering Sea (NMFS areas 500–539)
#'   \item \code{"GOA"} = Gulf of Alaska (NMFS areas 600–699)
#'   \item \code{"BSWGOA"} = Bering Sea and Western Gulf of Alaska (NMFS areas 500-539 and 610–620)
#' }
#'
#' \strong{Required columns}
#'
#' Observer data (\code{data_o}):
#' \itemize{
#'   \item \code{GEAR_TYPE}
#'   \item \code{LATDD_END}
#'   \item \code{LONDD_END}
#'   \item \code{RETRIEVAL_DATE}
#'   \item \code{YEAR}
#'   \item \code{WEIGHT} (kilograms)
#'   \item \code{NMFS_AREA}
#' }
#'
#' Electronic Monitoring data (\code{data_em}):
#' \itemize{
#'   \item \code{OBS_GEAR_CODE}
#'   \item \code{RETRIEVAL_END_LATITUDE_DD}
#'   \item \code{RETRIEVAL_END_LONGITUDE_DD}
#'   \item \code{RETRIEVAL_END_DATE}
#'   \item \code{YEAR}
#'   \item \code{EXTRAPOLATED_WEIGHT_MT} (metric tons)
#'   \item \code{REPORTING_AREA_CODE}
#' }
#'
#' @param data_o A data.frame containing observer catch data.
#' @param data_em A data.frame containing electronic monitoring catch data.
#' @param species_name Character string used in the plot title.
#' @param year Optional single year to plot. If supplied, overrides
#'   \code{year_min} and \code{year_max}.
#' @param year_min Optional minimum year (inclusive).
#' @param year_max Optional maximum year (inclusive).
#' @param quarter Integer vector of calendar quarters to include (1–4).
#'   Default includes all quarters.
#' @param region Character vector specifying management regions to include.
#'   One or more of \code{"AI"}, \code{"BS"}, \code{"GOA"}, or \code{"BSWGOA"}.
#'   Default includes all regions.
#' @param gear Character vector specifying gear types to include.
#'   One or more of \code{"Trawl"}, \code{"Pot"}, or \code{"Longline"}.
#'   Default includes all gear types.
#' @param cell_km Grid cell size in kilometers. Default is 20 km.
#' @param pad_frac Fractional padding added to the data extent when constructing
#'   the grid. Default is 0.08.
#'
#' @return
#' A \code{ggplot} object showing gridded total catch weight (metric tons) on a
#' North Pacific map.
#'
#' @seealso
#' \code{\link[ggplot2]{geom_sf}},
#' \code{\link[sf]{st_make_grid}},
#' \code{\link[rnaturalearth]{ne_countries}}
#'
#' @examples
#' \dontrun{
#' # Single year, Bering Sea, pot gear, Q1–Q2
#' plot_catch_locations_noaa_np_grid(
#'   data_o = observer_data,
#'   data_em = em_data,
#'   species_name = "Pacific cod",
#'   year = 2023,
#'   quarter = c(1, 2),
#'   region = "BS",
#'   gear = "Pot"
#' )
#'
#' # Multiple years, BS + AI, all gears
#' plot_catch_locations_noaa_np_grid(
#'   data_o = observer_data,
#'   data_em = em_data,
#'   species_name = "Pacific cod",
#'   year_min = 2019,
#'   year_max = 2024,
#'   region = c("BS", "AI")
#' )
#' }
#'
#' @export


plot_catch_locations_noaa_np_grid <- function(
  data_o,
  data_em,
  species_name,
  year = NULL,                    # single-year selector (overrides year_min/year_max)
  year_min = NULL,
  year_max = NULL,
  quarter = c(1, 2, 3, 4),
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  cell_km = 20,
  pad_frac = 0.08
) {

  stopifnot(is.data.frame(data_o), is.data.frame(data_em))
  stopifnot(is.character(species_name), length(species_name) == 1)

  # ---- required columns (fail fast with clear messages) ----
  req_o  <- c("GEAR_TYPE","LATDD_END","LONDD_END","RETRIEVAL_DATE","YEAR","WEIGHT","NMFS_AREA")
  req_em <- c("OBS_GEAR_CODE","RETRIEVAL_END_LATITUDE_DD","RETRIEVAL_END_LONGITUDE_DD",
              "RETRIEVAL_END_DATE","YEAR","EXTRAPOLATED_WEIGHT_MT","REPORTING_AREA_CODE")

  miss_o  <- setdiff(req_o,  names(data_o))
  miss_em <- setdiff(req_em, names(data_em))
  if (length(miss_o))  stop("data_o missing column(s): ", paste(miss_o, collapse = ", "))
  if (length(miss_em)) stop("data_em missing column(s): ", paste(miss_em, collapse = ", "))

  # ---- validate selections ----
  allowed_gears <- c("Trawl", "Pot", "Longline")
  gear <- unique(as.character(gear))
  bad_g <- setdiff(gear, allowed_gears)
  if (length(bad_g) > 0) stop("Unknown gear: ", paste(bad_g, collapse = ", "),
                              ". Allowed: ", paste(allowed_gears, collapse = ", "))

  quarter <- unique(as.integer(quarter))
  bad_q <- setdiff(quarter, 1:4)
  if (length(bad_q) > 0) stop("quarter must be within 1:4. Bad: ", paste(bad_q, collapse = ", "))

  region_map <- list(
    AI  = 540:544,
    BS  = 500:539,
    GOA = 600:699,
    BSWGOA = c(500:539,610,620)
  )
  region <- unique(toupper(as.character(region)))
  bad_r <- setdiff(region, names(region_map))
  if (length(bad_r) > 0) stop("Unknown region: ", paste(bad_r, collapse = ", "),
                              ". Allowed: AI, BS, GOA.")
  area_codes <- sort(unique(unlist(region_map[region])))

  # ---- helpers ----
  recode_gear <- function(x) {
    x <- suppressWarnings(as.integer(x))
    g <- rep("Other", length(x))
    g[x %in% 1:5] <- "Trawl"
    g[x == 6]     <- "Pot"
    g[x == 8]     <- "Longline"
    g
  }

  parse_dt <- function(x) {
    if (inherits(x, c("POSIXct","POSIXt","Date"))) return(x)
    y <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (all(is.na(y))) y <- suppressWarnings(as.Date(x))
    y
  }

  get_quarter <- function(dt) {
    lt <- as.POSIXlt(dt, tz = "UTC")
    m <- lt$mon + 1L
    ((m - 1L) %/% 3L) + 1L
  }

  num <- function(x) suppressWarnings(as.numeric(x))

  # ---- standardize inputs to a common schema ----
  d_o <- data.frame(
    GEAR   = recode_gear(data_o$GEAR_TYPE),
    LAT    = num(data_o$LATDD_END),
    LON    = num(data_o$LONDD_END),
    DT     = parse_dt(data_o$RETRIEVAL_DATE),
    YEAR   = suppressWarnings(as.integer(data_o$YEAR)),
    AREA   = suppressWarnings(as.integer(data_o$NMFS_AREA)),
    WT_MT  = num(data_o$WEIGHT) / 1000,            # kg -> metric tons
    SOURCE = "Observer",
    stringsAsFactors = FALSE
  )

  d_em <- data.frame(
    GEAR   = recode_gear(data_em$OBS_GEAR_CODE),
    LAT    = num(data_em$RETRIEVAL_END_LATITUDE_DD),
    LON    = num(data_em$RETRIEVAL_END_LONGITUDE_DD),
    DT     = parse_dt(data_em$RETRIEVAL_END_DATE),
    YEAR   = suppressWarnings(as.integer(data_em$YEAR)),
    AREA   = suppressWarnings(as.integer(data_em$REPORTING_AREA_CODE)),
    WT_MT  = num(data_em$EXTRAPOLATED_WEIGHT_MT),  # already metric tons
    SOURCE = "EM",
    stringsAsFactors = FALSE
  )

  d <- rbind(d_o, d_em)

  # ---- base filtering + cleaning BEFORE sf ----
  d <- d[is.finite(d$LAT) & is.finite(d$LON) & is.finite(d$WT_MT), ]
  d <- d[d$LAT >= 50, ]
  d <- d[d$WT_MT > 0, ]
  d <- d[d$AREA %in% area_codes, ]
  d <- d[d$GEAR %in% gear, ]

  # Year selection
  if (!is.null(year)) {
    year <- as.integer(year)[1]
    d <- d[!is.na(d$YEAR) & d$YEAR == year, ]
  } else {
    if (!is.null(year_min)) d <- d[!is.na(d$YEAR) & d$YEAR >= year_min, ]
    if (!is.null(year_max)) d <- d[!is.na(d$YEAR) & d$YEAR <= year_max, ]
  }

  # Quarter selection (derived from DT)
  d$QTR <- suppressWarnings(get_quarter(d$DT))
  d <- d[d$QTR %in% quarter, ]

  if (nrow(d) == 0) {
    stop("No records remain after filtering (lat/year/quarter/region/gear/weight>0).")
  }

  # ---- normalize longitude to [-180, 180] ----
  d$LON <- ifelse(d$LON > 180, d$LON - 360, d$LON)
  d$LON <- ifelse(d$LON < -180, d$LON + 360, d$LON)

  # ---- sf + NOAA-ish NP projection ----
  np_crs <- sf::st_crs("+proj=laea +lat_0=60 +lon_0=-160 +datum=WGS84 +units=m +no_defs")

  # Make sf operations robust across sf versions
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)

  pts <- sf::st_as_sf(d, coords = c("LON", "LAT"), crs = 4326, remove = FALSE)
  pts_p <- sf::st_transform(pts, np_crs)

  # Drop any points that became invalid after projection
  xy <- sf::st_coordinates(pts_p)
  keep <- is.finite(xy[, 1]) & is.finite(xy[, 2])
  n_drop <- sum(!keep)
  pts_p <- pts_p[keep, ]
  xy <- xy[keep, , drop = FALSE]

  if (nrow(xy) == 0) {
    stop("All points became invalid after projection (check lon/lat). Dropped ", n_drop, " points.")
  }

  # ---- robust bbox built from finite projected coordinates (prevents NA bbox) ----
  xmin <- min(xy[, 1]); xmax <- max(xy[, 1])
  ymin <- min(xy[, 2]); ymax <- max(xy[, 2])

  # Handle degenerate extents (all points share same X or Y)
  if (xmin == xmax) { xmin <- xmin - 1000; xmax <- xmax + 1000 }
  if (ymin == ymax) { ymin <- ymin - 1000; ymax <- ymax + 1000 }

  pad_x <- (xmax - xmin) * pad_frac
  pad_y <- (ymax - ymin) * pad_frac

  bb_pad <- sf::st_bbox(
    c(xmin = xmin - pad_x, ymin = ymin - pad_y,
      xmax = xmax + pad_x, ymax = ymax + pad_y),
    crs = np_crs
  )

  # ---- build regular grid ----
  cell_m <- cell_km * 1000
  grid <- sf::st_make_grid(
    sf::st_as_sfc(bb_pad),
    cellsize = c(cell_m, cell_m),
    what = "polygons",
    square = TRUE
  )
  grid <- sf::st_sf(GRID_ID = seq_along(grid), geometry = grid)

  # ---- assign points to grid cells ----
  hit <- sf::st_intersects(pts_p, grid)
  pts_p$GRID_ID <- vapply(hit, function(x) if (length(x) == 0) NA_integer_ else x[1], integer(1))
  pts_p <- pts_p[!is.na(pts_p$GRID_ID), ]

  if (nrow(pts_p) == 0) stop("No points fell within the constructed grid (unexpected).")

  # ---- aggregate weight (sum mt by cell) ----
  w <- sf::st_drop_geometry(pts_p)
  agg <- aggregate(WT_MT ~ GRID_ID, data = w, FUN = sum, na.rm = TRUE)
  names(agg)[2] <- "WT_MT_SUM"

  grid2 <- merge(grid, agg, by = "GRID_ID", all.x = FALSE)

  # ---- title labels ----
  yr_label <- if (!is.null(year)) {
    as.character(year)
  } else if (!is.null(year_min) && !is.null(year_max)) {
    paste0(year_min, "\u2013", year_max)
  } else if (!is.null(year_min)) {
    paste0(year_min, "\u2013present")
  } else if (!is.null(year_max)) {
    paste0("\u2264", year_max)
  } else {
    "All years"
  }

  q_label <- paste0("Q", paste(sort(unique(quarter)), collapse = ",Q"))
  plot_title <- paste0(
    species_name, " (", yr_label, ", ", q_label, ") \u2014 ",
    paste(gear, collapse = ", "), " \u2014 ", paste(region, collapse = ", "),
    " \u2014 ", cell_km, " km grid"
  )

  # ---- basemap ----
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world_p <- sf::st_transform(world, np_crs)

  # ---- plot ----
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = world_p,
      fill = "grey95",
      color = "grey70",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = grid2,
      ggplot2::aes(fill = WT_MT_SUM),
      color = NA
    ) +
    ggplot2::scale_fill_viridis_c(name = "Weight (mt)", trans = "sqrt") +
    ggplot2::coord_sf(
      xlim = c(bb_pad$xmin, bb_pad$xmax),
      ylim = c(bb_pad$ymin, bb_pad$ymax),
      expand = FALSE
    ) +
    ggplot2::labs(title = plot_title) +
    ggplot2::theme_bw()
}
