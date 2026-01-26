#' Plot gridded catch by depth vs latitude/longitude (date-range filter; dateline-safe)
#'
#' Aggregates observer catch into a 2D grid with depth binned in meters (default 10 m)
#' and the x-axis (latitude or longitude) binned at a user-specified resolution in km.
#' The plotted value is the sum of catch weight (metric tons) within each grid cell.
#'
#' This version is **dateline-safe** for Alaska data: if longitude spans 180°, the function
#' automatically switches to a continuous longitude representation to avoid the large
#' artificial break (wraps to 0–360 when that yields the smaller span).
#'
#' Fill scale is aligned with your gridded map style: viridis + sqrt transform and
#' legend title "Weight (mt)".
#'
#' @param data_o Observer data.frame/data.table with required columns:
#'   GEAR_TYPE, LATDD_END, LONDD_END, RETRIEVAL_DATE, WEIGHT, NMFS_AREA, BOTTOM_DEPTH_FATHOMS.
#' @param species_name Character scalar used in the plot title (if titles shown).
#' @param date_min Optional start date (inclusive) as "mm/dd/yyyy".
#' @param date_max Optional end date (inclusive) as "mm/dd/yyyy".
#' @param region Vector: one or more of "AI","BS","GOA","BSWGOA", or numeric NMFS area code(s).
#' @param gear Character vector: one or more of "Trawl","Pot","Longline". Default all.
#' @param facet_gear If TRUE, facet by gear.
#' @param x_axis Which horizontal axis to use: "lat" or "lon".
#' @param x_bin_km Horizontal bin size in kilometers (default 20 km).
#' @param depth_bin_m Depth bin size in meters (default 10 m).
#' @param show_titles If FALSE, remove plot title/subtitle/caption.
#' @param show_label If FALSE, remove the upper-right info label.
#'
#' @return A ggplot2 object.
#' @export
plot_catch_depth_noaa_np_grid_date <- function(
  data_o,
  species_name,
  date_min = NULL,
  date_max = NULL,
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  facet_gear = FALSE,
  x_axis = c("lat", "lon"),
  x_bin_km = 20,
  depth_bin_m = 10,
  show_titles = TRUE,
  show_label = TRUE
) {

  stopifnot(is.data.frame(data_o))
  stopifnot(is.character(species_name), length(species_name) == 1)

  x_axis <- match.arg(tolower(x_axis), c("lat", "lon"))
  stopifnot(is.numeric(x_bin_km), length(x_bin_km) == 1, is.finite(x_bin_km), x_bin_km > 0)
  stopifnot(is.numeric(depth_bin_m), length(depth_bin_m) == 1, is.finite(depth_bin_m), depth_bin_m > 0)

  # ---- required columns (observer) ----
  req_o <- c("GEAR_TYPE","LATDD_END","LONDD_END","RETRIEVAL_DATE","WEIGHT","NMFS_AREA","BOTTOM_DEPTH_FATHOMS")
  miss_o <- setdiff(req_o, names(data_o))
  if (length(miss_o)) stop("data_o missing column(s): ", paste(miss_o, collapse = ", "))

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

  parse_mdy <- function(s) {
    if (is.null(s) || !nzchar(s)) return(NULL)
    if (!is.character(s) || length(s) != 1) {
      stop("date_min/date_max must be character scalars like \"mm/dd/yyyy\".")
    }
    out <- suppressWarnings(as.Date(s, format = "%m/%d/%Y"))
    if (is.na(out)) {
      stop("Could not parse date \"", s, "\". Expected format is \"mm/dd/yyyy\" (e.g., \"01/15/2023\").")
    }
    out
  }

  num <- function(x) suppressWarnings(as.numeric(x))

  # dateline-safe longitude handling
  wrap_lon_180 <- function(lon) {
    lon <- suppressWarnings(as.numeric(lon))
    ((lon + 180) %% 360) - 180
  }
  wrap_lon_360 <- function(lon) {
    lon <- wrap_lon_180(lon)
    ifelse(lon < 0, lon + 360, lon)
  }
  lon_continuous <- function(lon) {
    lon180 <- wrap_lon_180(lon)
    lon360 <- wrap_lon_360(lon)
    r180 <- diff(range(lon180, na.rm = TRUE))
    r360 <- diff(range(lon360, na.rm = TRUE))
    if (is.finite(r360) && r360 < r180) {
      list(lon = lon360, mode = "360")
    } else {
      list(lon = lon180, mode = "180")
    }
  }
  lon_labels_360 <- function(x) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(NA_character_)
    v <- round(v)

    # keep within [0,360) just in case
    v <- v %% 360

    if (v == 180) return("180°")
    if (v < 180)  return(paste0(v, "°E"))
    paste0(360 - v, "°W")
  }, character(1))
}


  # ---- region mapping ----
  if (any(toupper(as.character(region)) %in% c("AI","BS","GOA","BSWGOA"))) {
    region_map <- list(
      AI     = 540:544,
      BS     = 500:539,
      GOA    = 600:699,
      BSWGOA = c(500:539, 610:620)
    )
    region <- unique(toupper(as.character(region)))
    bad_r <- setdiff(region, names(region_map))
    if (length(bad_r) > 0) stop("Unknown region: ", paste(bad_r, collapse = ", "),
                                ". Allowed: ", paste(names(region_map), collapse = ", "))
    area_codes <- sort(unique(unlist(region_map[region])))
  } else {
    area_codes <- region
  }

  # ---- validate gears ----
  allowed_gears <- c("Trawl", "Pot", "Longline")
  gear <- unique(as.character(gear))
  bad_g <- setdiff(gear, allowed_gears)
  if (length(bad_g) > 0) stop("Unknown gear: ", paste(bad_g, collapse = ", "),
                              ". Allowed: ", paste(allowed_gears, collapse = ", "))

  # ---- standardize observer data ----
  d <- data.frame(
    GEAR     = recode_gear(data_o$GEAR_TYPE),
    LAT      = num(data_o$LATDD_END),
    LON_RAW  = num(data_o$LONDD_END),
    DT       = parse_dt(data_o$RETRIEVAL_DATE),
    AREA     = suppressWarnings(as.integer(data_o$NMFS_AREA)),
    WT_MT    = num(data_o$WEIGHT) / 1000,                     # kg -> mt
    DEPTH_M  = num(data_o$BOTTOM_DEPTH_FATHOMS) * 1.8288,      # fathoms -> meters
    stringsAsFactors = FALSE
  )

  # ---- base filters ----
  d <- d[is.finite(d$LAT) & is.finite(d$LON_RAW) & is.finite(d$WT_MT) & is.finite(d$DEPTH_M), ]
  d <- d[d$LAT >= 50, ]
  d <- d[d$WT_MT > 0, ]
  d <- d[d$AREA %in% area_codes, ]
  d <- d[d$GEAR %in% gear, ]

  # ---- date range filter (inclusive) ----
  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)
  d$DT_DATE <- as.Date(d$DT)

  if (!is.null(dmin)) d <- d[!is.na(d$DT_DATE) & d$DT_DATE >= dmin, ]
  if (!is.null(dmax)) d <- d[!is.na(d$DT_DATE) & d$DT_DATE <= dmax, ]

  # ---- friendly empty plot ----
  if (nrow(d) == 0) {
    msg <- "No data available for this time period / filters."
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 6) +
        ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1)
    )
  }

  # ---- choose x axis values and binning width (in degrees) ----
  # Convert km bin width into degrees (approx):
  mean_lat <- mean(d$LAT, na.rm = TRUE)
  km_per_deg_lat <- 111.32
  km_per_deg_lon <- 111.32 * cos(mean_lat * pi / 180)

  lon_mode <- NULL

  if (x_axis == "lat") {
    deg_bin <- x_bin_km / km_per_deg_lat
    d$XVAL <- d$LAT
    x_lab <- "Latitude"
  } else {
    # Dateline-safe continuous longitude (pick -180..180 or 0..360 to minimize span)
    lc <- lon_continuous(d$LON_RAW)
    d$XVAL <- lc$lon
    lon_mode <- lc$mode
    x_lab <- "Longitude"

    if (!is.finite(km_per_deg_lon) || km_per_deg_lon <= 0) km_per_deg_lon <- 1e-6
    deg_bin <- x_bin_km / km_per_deg_lon
  }

  # ---- binning (midpoint bins) ----
  d$XBIN <- floor(d$XVAL / deg_bin) * deg_bin + (deg_bin / 2)
  d$DBIN <- floor(d$DEPTH_M / depth_bin_m) * depth_bin_m + (depth_bin_m / 2)

  # ---- aggregate ----
  if (isTRUE(facet_gear)) {
    agg <- stats::aggregate(WT_MT ~ XBIN + DBIN + GEAR, data = d, FUN = sum, na.rm = TRUE)
    names(agg)[names(agg) == "WT_MT"] <- "WT_MT_SUM"
  } else {
    agg <- stats::aggregate(WT_MT ~ XBIN + DBIN, data = d, FUN = sum, na.rm = TRUE)
    names(agg)[names(agg) == "WT_MT"] <- "WT_MT_SUM"
  }

  # ---- title: species + date only ----
  date_label <- if (!is.null(dmin) && !is.null(dmax)) {
    paste0(format(dmin, "%Y-%m-%d"), " to ", format(dmax, "%Y-%m-%d"))
  } else if (!is.null(dmin)) {
    paste0(format(dmin, "%Y-%m-%d"), " to present")
  } else if (!is.null(dmax)) {
    paste0("Up to ", format(dmax, "%Y-%m-%d"))
  } else {
    "All dates"
  }
  plot_title <- paste0(species_name, " (", date_label, ")")

  # ---- upper-right label ----
  gear_label <- if (isTRUE(facet_gear)) {
    "Gear: faceted"
  } else {
    paste0("Gear: ", paste(gear, collapse = ", "))
  }
  info_label <- paste0(
    gear_label,
    "\nX bin: ", x_bin_km, " km (", x_axis, ")",
    "\nDepth bin: ", depth_bin_m, " m"
  )

  # ---- plot ----
  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(x = XBIN, y = DBIN, fill = WT_MT_SUM)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_viridis_c(name = "Weight (mt)", trans = "sqrt") +
    ggplot2::labs(x = x_lab, y = "Depth (m)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(7, 12, 7, 7)
    )

  # If we used 0..360, label ticks as E/W (prevents the visual "break" confusion)
  if (!is.null(lon_mode) && identical(lon_mode, "360")) {
    p <- p + ggplot2::scale_x_continuous(labels = lon_labels_360)
  }

  if (isTRUE(facet_gear)) {
    p <- p + ggplot2::facet_wrap(~GEAR, ncol = 1)
  }

  if (isTRUE(show_titles)) {
    p <- p + ggplot2::labs(title = plot_title)
  } else {
    p <- p + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  }

  if (isTRUE(show_label)) {
    p <- p + ggplot2::annotate(
      "label",
      x = Inf, y = Inf,
      label = info_label,
      hjust = 1.02, vjust = 1.02,
      size = 5
    )
  }

  p
}
