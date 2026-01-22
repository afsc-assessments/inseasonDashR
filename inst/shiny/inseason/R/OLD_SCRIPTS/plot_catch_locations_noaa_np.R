#' Plot catch locations on a NOAA-style North Pacific map
#'
#' Plot fishing event locations from observer and electronic monitoring (EM)
#' datasets on a NOAA-style North Pacific basemap using a Lambert Azimuthal
#' Equal-Area projection centered near the Bering Sea.
#'
#' Points are filtered to latitudes north of 50°N, optionally filtered by year
#' range, filtered by region (AI/BS/GOA), and optionally filtered by gear type.
#' Point size is scaled by catch weight (metric tons), where observer weights
#' are converted from kilograms to metric tons internally. Point transparency
#' is scaled by time such that earlier observations are more transparent and
#' more recent observations are more opaque.
#'
#' Gear codes are internally recoded as:
#' \itemize{
#'   \item 1–5 = Trawl
#'   \item 6   = Pot
#'   \item 8   = Longline
#' }
#'
#' Regions are mapped to NMFS area code ranges as:
#' \itemize{
#'   \item \code{"AI"}  = 540–544
#'   \item \code{"BS"}  = 500–539
#'   \item \code{"GOA"} = 600–699
#'   \item \code{"BSWGOA"} = 500-539 and 610-620
#' }
#'
#' @details
#' This function assumes the following column names are present:
#'
#' \strong{Observer data (`data_o`)}:
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
#' \strong{Electronic Monitoring data (`data_em`)}:
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
#' @param year_min Optional minimum year (inclusive). If \code{NULL}, no lower
#'   bound is applied.
#' @param year_max Optional maximum year (inclusive). If \code{NULL}, no upper
#'   bound is applied.
#' @param region Character vector specifying which regions to plot. One or more
#'   of \code{"AI"}, \code{"BS"}, or \code{"GOA"}. Default is all regions.
#' @param gear Character vector specifying which gear types to plot. One or more
#'   of \code{"Trawl"}, \code{"Pot"}, or \code{"Longline"}. Default is all three.
#' @param size_range Numeric vector of length 2 giving the minimum and maximum
#'   point sizes for scaling weight (metric tons).
#'
#' @return A \code{ggplot} object.
#'
#' @seealso
#' \code{\link[ggplot2]{geom_sf}},
#' \code{\link[sf]{st_transform}},
#' \code{\link[rnaturalearth]{ne_countries}}
#'
#' @examples
#' \dontrun{
#' # Bering Sea only, pot + longline, 2019–2024
#' plot_catch_locations_noaa_np(
#'   data_o = observer_data,
#'   data_em = em_data,
#'   species_name = "Pacific cod",
#'   year_min = 2019,
#'   year_max = 2024,
#'   region = "BS",
#'   gear = c("Pot", "Longline")
#' )
#'
#' # BS + AI, all gears, all years
#' plot_catch_locations_noaa_np(
#'   data_o = observer_data,
#'   data_em = em_data,
#'   species_name = "Pacific cod",
#'   region = c("BS", "AI")
#' )
#' }
#'
#' @export
plot_catch_locations_noaa_np <- function(
  data_o,
  data_em,
  species_name,
  year_min = NULL,
  year_max = NULL,
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  size_range = c(1, 6)
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

  # ---- validate gear selection ----
  allowed_gears <- c("Trawl", "Pot", "Longline")
  if (is.null(gear) || length(gear) == 0) {
    stop("gear must include at least one of: ", paste(allowed_gears, collapse = ", "))
  }
  gear <- unique(as.character(gear))
  bad <- setdiff(gear, allowed_gears)
  if (length(bad) > 0) {
    stop("Unknown gear selection(s): ", paste(bad, collapse = ", "),
         ". Allowed: ", paste(allowed_gears, collapse = ", "))
  }

  # ---- region to area-code mapping ----
  region_map <- list(
    AI  = 540:544,
    BS  = 500:539,
    GOA = 600:699,
    BSWGOA = c(610:620,500:539)
  )

  if (is.null(region) || length(region) == 0) stop("region must include at least one of: AI, BS, GOA")
  region <- unique(toupper(as.character(region)))
  bad_region <- setdiff(region, names(region_map))
  if (length(bad_region) > 0) {
    stop("Unknown region(s): ", paste(bad_region, collapse = ", "),
         ". Allowed regions are: AI, BS, GOA.")
  }

  area_codes <- sort(unique(unlist(region_map[region])))

  # ---------- helpers ----------
  recode_gear <- function(x) {
    x <- suppressWarnings(as.integer(x))
    g <- rep("Other", length(x))
    g[x %in% 1:5] <- "Trawl"
    g[x == 6]     <- "Pot"
    g[x == 8]     <- "Longline"
    g
  }

  parse_dt <- function(x) {
    if (inherits(x, c("POSIXct", "POSIXt", "Date"))) return(x)
    y <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (all(is.na(y))) y <- suppressWarnings(as.Date(x))
    y
  }

  scale_01 <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(r[1]) || r[1] == r[2]) return(rep(1, length(x)))
    (x - r[1]) / (r[2] - r[1])
  }

  num <- function(x) suppressWarnings(as.numeric(x))

  # ---- build tables (hard-coded columns) ----
  d_o <- data.frame(
    GEAR   = recode_gear(data_o$GEAR_TYPE),
    LAT    = num(data_o$LATDD_END),
    LON    = num(data_o$LONDD_END),
    DT     = parse_dt(data_o$RETRIEVAL_DATE),
    YEAR   = suppressWarnings(as.integer(data_o$YEAR)),
    AREA   = suppressWarnings(as.integer(data_o$NMFS_AREA)),
    WT_MT  = num(data_o$WEIGHT) / 1000, # kg -> metric tons
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
    WT_MT  = num(data_em$EXTRAPOLATED_WEIGHT_MT), # already metric tons
    SOURCE = "EM",
    stringsAsFactors = FALSE
  )

  d <- rbind(d_o, d_em)

  # ---- filters ----
  d <- d[is.finite(d$LAT) & is.finite(d$LON) & d$LAT >= 50, ]

  if (!is.null(year_min)) d <- d[!is.na(d$YEAR) & d$YEAR >= year_min, ]
  if (!is.null(year_max)) d <- d[!is.na(d$YEAR) & d$YEAR <= year_max, ]

  # region filter (applies to both sources)
  d <- d[d$AREA %in% area_codes, ]

  # gear filter (exclude "Other" automatically)
  d <- d[d$GEAR %in% gear, ]

  if (nrow(d) == 0) {
    stop("No records to plot after filtering (LAT>=50, year range, region, gear).")
  }

  # size scaling: ignore non-positive / missing weights
  d$WT_MT <- ifelse(is.finite(d$WT_MT) & d$WT_MT > 0, d$WT_MT, NA_real_)

  # ---- alpha by date (older faint, newer opaque) ----
  tnum <- suppressWarnings(as.numeric(d$DT))
  if (all(is.na(tnum))) {
    tnum <- rep(0, nrow(d))
  } else {
    tnum[is.na(tnum)] <- min(tnum, na.rm = TRUE)
  }
  d$RECENCY <- scale_01(tnum)

  # ---- title ----
  yr_label <- if (!is.null(year_min) && !is.null(year_max)) {
    paste0(year_min, "\u2013", year_max)
  } else if (!is.null(year_min)) {
    paste0(year_min, "\u2013present")
  } else if (!is.null(year_max)) {
    paste0("\u2264", year_max)
  } else {
    "All years"
  }

  plot_title <- paste0(
    species_name, " (", yr_label, ") \u2014 ",
    paste(gear, collapse = ", "),
    " \u2014 ", paste(region, collapse = ", ")
  )

  # ---- sf + NOAA-ish NP projection ----
  pts <- sf::st_as_sf(d, coords = c("LON", "LAT"), crs = 4326, remove = FALSE)
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  np_crs <- sf::st_crs("+proj=laea +lat_0=60 +lon_0=-160 +datum=WGS84 +units=m +no_defs")

  world_p <- sf::st_transform(world, np_crs)
  pts_p   <- sf::st_transform(pts, np_crs)

  bb <- sf::st_bbox(pts_p)
  pad_x <- as.numeric(bb$xmax - bb$xmin) * 0.08
  pad_y <- as.numeric(bb$ymax - bb$ymin) * 0.08

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = world_p,
      fill = "grey95",
      color = "grey70",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = pts_p,
      ggplot2::aes(color = GEAR, shape = SOURCE, alpha = RECENCY, size = WT_MT),
      show.legend = TRUE
    ) +
    ggplot2::scale_alpha_continuous(range = c(0.15, 0.95), guide = "none") +
    ggplot2::scale_size_continuous(range = size_range, name = "Weight (metric tons)") +
    ggplot2::coord_sf(
      xlim = c(bb$xmin - pad_x, bb$xmax + pad_x),
      ylim = c(bb$ymin - pad_y, bb$ymax + pad_y),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = plot_title,
      color = "Gear",
      shape = "Data source"
    ) +
    ggplot2::theme_bw()
}
