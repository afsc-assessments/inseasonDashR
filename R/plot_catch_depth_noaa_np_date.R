#' Plot catch depth by latitude or longitude (NOAA North Pacific style)
#'
#' Creates a depth–position scatter plot of catch observations using Observer
#' and optional EM data. Depth is shown on a reversed y-axis, point size is
#' scaled by catch weight (metric tons), and transparency is scaled by recency.
#' The plot can be faceted by gear and includes optional annotations for total
#' hauls and vessels. Longitude is automatically handled to avoid discontinuities
#' at the dateline for Alaska-centric data.
#'
#' @param data_o data.frame containing Observer catch data.
#' @param data_em Optional data.frame containing EM catch data. If NULL or empty,
#'   the plot will be generated using Observer data only.
#' @param species_name Character string giving the species name for the title.
#' @param date_min Optional start date in \code{"mm/dd/yyyy"} format.
#' @param date_max Optional end date in \code{"mm/dd/yyyy"} format.
#' @param region Character vector specifying regions (e.g., \code{"AI"},
#'   \code{"BS"}, \code{"GOA"}) or numeric NMFS area codes.
#' @param gear Character vector of gear types to include (e.g.,
#'   \code{"Trawl"}, \code{"Pot"}, \code{"Longline"}).
#' @param x_axis Character string specifying the x-axis variable; one of
#'   \code{"Lat"} or \code{"Lon"}.
#' @param facet_gear Logical; if TRUE, facet the plot by gear type.
#' @param show_titles Logical; if TRUE, display the plot title.
#' @param show_label Logical; if TRUE, display the upper-right label describing
#'   gear selection and x-axis variable.
#' @param show_counts Logical; if TRUE, display total haul and vessel counts in
#'   the upper-left of each panel.
#' @param size_range Numeric vector of length two giving the minimum and maximum
#'   point sizes used to scale catch weight.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' Depth is plotted in meters with a reversed y-axis. Catch weight is converted
#' to metric tons where necessary. When longitude is selected for the x-axis,
#' the function automatically chooses a continuous representation (either
#' \eqn{-180}–\eqn{180} or \eqn{0}–\eqn{360}) to avoid artificial breaks near
#' the dateline.
#'
#' @examples
#' \dontrun{
#' plot_catch_depth_noaa_np_date(
#'   data_o = obs_data,
#'   data_em = em_data,
#'   species_name = "Pacific cod",
#'   date_min = "01/01/2020",
#'   date_max = "12/31/2023",
#'   region = c("BS"),
#'   gear = c("Trawl", "Longline"),
#'   x_axis = "Lon",
#'   facet_gear = TRUE
#' )
#' }
#'
#' @export

plot_catch_depth_noaa_np_date <- function(
  data_o,
  data_em = NULL,
  species_name,
  date_min = NULL,
  date_max = NULL,
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  x_axis = c("Lat", "Lon"),
  facet_gear = FALSE,
  show_titles = TRUE,
  show_label = TRUE,
  show_counts = TRUE,
  size_range = c(0.1, 6)
) {

  stopifnot(is.data.frame(data_o))
  stopifnot(is.character(species_name), length(species_name) == 1)

  x_axis <- match.arg(x_axis)

  # ---- helpers ----
  recode_gear <- function(x) {
    g <- rep("Other", length(x))
    x <- suppressWarnings(as.integer(x))
    g[x %in% 1:5] <- "Trawl"
    g[x == 6]     <- "Pot"
    g[x == 8]     <- "Longline"
    g
  }

  num <- function(x) suppressWarnings(as.numeric(x))

  parse_dt <- function(x) {
    if (inherits(x, c("POSIXct","POSIXt"))) return(x)
    if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
    y <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (all(is.na(y))) y <- suppressWarnings(as.POSIXct(as.Date(x), tz = "UTC"))
    y
  }

  parse_mdy <- function(s) {
    if (is.null(s) || !nzchar(s)) return(NULL)
    out <- suppressWarnings(as.Date(s, format = "%m/%d/%Y"))
    if (is.na(out)) stop("Date must be in mm/dd/yyyy format: ", s)
    out
  }

  scale_01 <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(r[1]) || r[1] == r[2]) return(rep(1, length(x)))
    (x - r[1]) / (r[2] - r[1])
  }

  # Longitude continuity helpers
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
    if (is.finite(r360) && is.finite(r180) && r360 < r180) {
      list(lon = lon360, mode = "360")
    } else {
      list(lon = lon180, mode = "180")
    }
  }
  lon_labels_360 <- function(x) {
    vapply(x, function(v) {
      if (!is.finite(v)) return(NA_character_)
      v <- round(v) %% 360
      if (v == 180) return("180°")
      if (v < 180)  return(paste0(v, "°E"))
      paste0(360 - v, "°W")
    }, character(1))
  }

  # ---- region mapping (same as your other functions) ----
  if (any(toupper(as.character(region)) %in% c("AI","BS","GOA","BSWGOA"))) {
    region_map <- list(
      AI     = 540:544,
      BS     = 500:539,
      GOA    = 600:699,
      BSWGOA = c(500:539, 610:620)
    )
    rsel <- unique(toupper(as.character(region)))
    bad_r <- setdiff(rsel, names(region_map))
    if (length(bad_r) > 0) {
      stop("Unknown region: ", paste(bad_r, collapse = ", "),
           ". Allowed: ", paste(names(region_map), collapse = ", "))
    }
    area_codes <- sort(unique(unlist(region_map[rsel])))
  } else {
    area_codes <- suppressWarnings(as.integer(region))
  }

  # ---- standardize Observer ----
  d_o <- data.frame(
    SOURCE    = "Observer",
    SHAPE_KEY = "Observer",
    GEAR      = recode_gear(data_o$GEAR_TYPE),
    LAT       = num(data_o$LATDD_END),
    LON       = num(data_o$LONDD_END),
    DT        = parse_dt(data_o$RETRIEVAL_DATE),
    DT_DATE   = as.Date(parse_dt(data_o$RETRIEVAL_DATE)),
    DEPTH_M   = num(data_o$BOTTOM_DEPTH_FATHOMS) * 1.8288,     # fathoms -> m
    AREA      = suppressWarnings(as.integer(data_o$NMFS_AREA)),
    WT_MT     = num(data_o$WEIGHT) / 1000,                     # kg -> mt
    VESSEL_ID = if ("VESSEL" %in% names(data_o)) as.character(data_o$VESSEL) else NA_character_,
    stringsAsFactors = FALSE
  )

  # ---- standardize EM (optional) ----
  d_em <- NULL
  if (!is.null(data_em) && is.data.frame(data_em) && nrow(data_em) > 0) {
    depth_col <- if ("BOTTOM_DEPTH_FATHOMS" %in% names(data_em)) "BOTTOM_DEPTH_FATHOMS" else NULL
    d_em <- data.frame(
      SOURCE    = "EM",
      SHAPE_KEY = "EM",
      GEAR      = recode_gear(data_em$OBS_GEAR_CODE),
      LAT       = num(data_em$RETRIEVAL_END_LATITUDE_DD),
      LON       = num(data_em$RETRIEVAL_END_LONGITUDE_DD),
      DT        = parse_dt(data_em$RETRIEVAL_END_DATE),
      DT_DATE   = as.Date(parse_dt(data_em$RETRIEVAL_END_DATE)),
      DEPTH_M   = if (!is.null(depth_col)) num(data_em[[depth_col]]) * 1.8288 else NA_real_,
      AREA      = suppressWarnings(as.integer(data_em$REPORTING_AREA_CODE)),
      WT_MT     = num(data_em$EXTRAPOLATED_WEIGHT_MT),          # already mt
      VESSEL_ID = if ("OBS_VESSEL_ID" %in% names(data_em)) as.character(data_em$OBS_VESSEL_ID) else NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    d_em <- data.frame(
      SOURCE=character(0), SHAPE_KEY=character(0), GEAR=character(0),
      LAT=numeric(0), LON=numeric(0), DT=as.POSIXct(character(0)),
      DT_DATE=as.Date(character(0)), DEPTH_M=numeric(0), AREA=integer(0),
      WT_MT=numeric(0), VESSEL_ID=character(0), stringsAsFactors = FALSE
    )
  }

  d <- rbind(d_o, d_em)

  # ---- filters ----
  d <- d[is.finite(d$LAT) & is.finite(d$LON) & is.finite(d$DEPTH_M), , drop = FALSE]
  d <- d[d$LAT >= 50, , drop = FALSE]
  d <- d[is.finite(d$AREA) & d$AREA %in% area_codes, , drop = FALSE]
  d <- d[d$GEAR %in% gear, , drop = FALSE]
  d$WT_MT <- ifelse(is.finite(d$WT_MT) & d$WT_MT > 0, d$WT_MT, NA_real_)
  d <- d[!is.na(d$WT_MT), , drop = FALSE]

  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)
  if (!is.null(dmin)) d <- d[!is.na(d$DT_DATE) & d$DT_DATE >= dmin, , drop = FALSE]
  if (!is.null(dmax)) d <- d[!is.na(d$DT_DATE) & d$DT_DATE <= dmax, , drop = FALSE]

  if (nrow(d) == 0) {
    stop("No data remaining after filtering (region/gear/date/lat/weight/depth).")
  }

  # ---- x-axis variable ----
  if (x_axis == "Lon") {
    lon_fix <- lon_continuous(d$LON)
    d$LOC <- lon_fix$lon
    lon_mode <- lon_fix$mode
  } else {
    d$LOC <- d$LAT
    lon_mode <- NULL
  }

  # ---- alpha by recency ----
  tnum <- suppressWarnings(as.numeric(d$DT))
  if (all(is.na(tnum))) tnum <- rep(0, nrow(d))
  tnum[is.na(tnum)] <- min(tnum, na.rm = TRUE)
  d$RECENCY <- scale_01(tnum)

  # ---- title + label text ----
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
  gear_label <- if (facet_gear) "Gear: faceted" else paste0("Gear: ", paste(gear, collapse = ", "))
  axis_label <- paste0("X: ", if (x_axis == "Lat") "Latitude" else "Longitude")
  ur_label <- paste(gear_label, axis_label, sep = "\n")

  # ---- counts (after filtering) ----
  count_label_df <- NULL
  if (isTRUE(show_counts)) {
    tmp <- d
    tmp$VESSEL_ID <- trimws(as.character(tmp$VESSEL_ID))
    tmp$VESSEL_ID[tmp$VESSEL_ID == ""] <- NA_character_

    if (isTRUE(facet_gear)) {
      split_list <- split(tmp, tmp$GEAR)

      count_label_df <- do.call(rbind, lapply(names(split_list), function(g) {
        dd <- split_list[[g]]

        n_hauls   <- nrow(dd)
        n_vessels <- length(unique(dd$VESSEL_ID[!is.na(dd$VESSEL_ID)]))

        data.frame(
          GEAR  = g,
          LABEL = paste0("Hauls: ", n_hauls, "\nVessels: ", n_vessels),
          stringsAsFactors = FALSE
        )
      }))
    } else {
      n_hauls   <- nrow(tmp)
      n_vessels <- length(unique(tmp$VESSEL_ID[!is.na(tmp$VESSEL_ID)]))

      count_label_df <- data.frame(
        LABEL = paste0("Hauls: ", n_hauls, "\nVessels: ", n_vessels),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- plot ----
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = LOC, y = DEPTH_M, color = GEAR, size = WT_MT, alpha = RECENCY, shape = SHAPE_KEY)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_size_continuous(range = size_range, name = "Weight (mt)") +
    ggplot2::scale_alpha_continuous(range = c(0.15, 0.95), guide = "none") +
    ggplot2::scale_shape_manual(values = c(Observer = 16, EM = 17), guide = ggplot2::guide_legend(title = "Data source")) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(
      x = if (x_axis == "Lat") "Latitude" else "Longitude",
      y = "Depth (m)",
      color = "Gear"
    )

  # Longitude labels when using 0..360 representation
  if (x_axis == "Lon" && identical(lon_mode, "360")) {
    p <- p + ggplot2::scale_x_continuous(labels = lon_labels_360)
  }

  if (facet_gear) p <- p + ggplot2::facet_wrap(~GEAR, ncol = 1)

  if (show_titles) {
    p <- p + ggplot2::labs(title = plot_title)
  } else {
    p <- p + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  }

  # Put label + counts on top with enough plot margin
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(7, 12, 7, 7))

  if (show_label) {
    p <- p +
      ggplot2::annotate(
        "label",
        x = Inf, y = Inf,
        label = ur_label,
        hjust = 1.02, vjust = 1.02,
        size = 4
      )
  }

  # ---- counts annotation (INSIDE, transparent box) ----
if (isTRUE(show_counts)) {
  p <- p +
    ggplot2::geom_label(
      data = count_label_df,
      ggplot2::aes(x = -Inf, y = -Inf, label = LABEL),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 4,
      label.padding = ggplot2::unit(0.15, "lines"),
      linewidth = 0.3,
      fill = scales::alpha("white", 0.4)  # <-- transparent box
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

  p
}
