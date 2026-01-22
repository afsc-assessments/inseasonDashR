#' Plot catch locations on a NOAA-style North Pacific map (date-range filter)
#'
#' Plots observer and EM catch locations with transparency scaled by recency
#' and point size scaled by catch weight (metric tons). The map extent is
#' automatically zoomed to the spatial extent of the data.
#'
#' Observer points are always circles; EM points are always triangles.
#' Robust to cases where one data source or one or more gear types are absent.
#'
#' Adds upper-left annotations:
#' - Hauls: number of plotted rows (points)
#' - Vessels: number of unique vessel IDs across both sources
#'   (data_o$VESSEL and data_em$OBS_VESSEL_ID)
#'
#' @param data_o Observer data.frame (GET_CURRENT.sql output)
#' @param data_em EM data.frame (GET_EM_CATCH.sql output)
#' @param species_name Character string used in the plot title
#' @param date_min Optional start date (inclusive) as "mm/dd/yyyy"
#' @param date_max Optional end date (inclusive) as "mm/dd/yyyy"
#' @param region Character vector: one or more of "AI","BS","GOA","BSWGOA"
#' @param gear Character vector: one or more of "Trawl","Pot","Longline"
#' @param size_range Numeric length-2 vector giving point size range (metric tons)
#' @param facet_gear Logical; if TRUE, facet map by gear type
#' @param show_titles Logical; if FALSE, suppress plot title
#' @param show_label Logical; if FALSE, suppress upper-right gear label
#' @param show_counts Logical; if TRUE, add haul/vessel counts upper-left
#' @param pad_frac Fractional padding added around data extent (default 0.08)
#'
#' @return A ggplot object
#' @export
plot_catch_locations_noaa_np_date <- function(
  data_o,
  data_em,
  species_name,
  date_min = NULL,
  date_max = NULL,
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  size_range = c(1, 6),
  facet_gear = FALSE,
  show_titles = TRUE,
  show_label = TRUE,
  show_counts = TRUE,
  pad_frac = 0.08
) {

  stopifnot(is.data.frame(data_o), is.data.frame(data_em))
  stopifnot(is.character(species_name), length(species_name) == 1)

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
    if (is.null(s)) return(NULL)
    out <- suppressWarnings(as.Date(s, format = "%m/%d/%Y"))
    if (is.na(out)) stop("Date must be in mm/dd/yyyy format: ", s)
    out
  }

  scale_01 <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(r[1]) || r[1] == r[2]) return(rep(1, length(x)))
    (x - r[1]) / (r[2] - r[1])
  }

  num <- function(x) suppressWarnings(as.numeric(x))

  # ---- region mapping ----
  region_map <- list(
    AI     = 540:544,
    BS     = 500:539,
    GOA    = 600:699,
    BSWGOA = c(500:539, 610:620)
  )
  region <- unique(toupper(as.character(region)))
  bad_r <- setdiff(region, names(region_map))
  if (length(bad_r) > 0) {
    stop("Unknown region: ", paste(bad_r, collapse = ", "),
         ". Allowed: ", paste(names(region_map), collapse = ", "))
  }
  area_codes <- sort(unique(unlist(region_map[region])))

  # ---- safe standardizer (returns correct 0-row df if input empty) ----
  std_points <- function(df, source_label,
                         gear_code, lat, lon, dt, area, wt_mt, vessel_col) {

    n <- nrow(df)
    if (n == 0) {
      return(data.frame(
        GEAR = character(0),
        LAT = numeric(0),
        LON = numeric(0),
        DT = as.POSIXct(character(0)),
        DT_DATE = as.Date(character(0)),
        AREA = integer(0),
        WT_MT = numeric(0),
        SOURCE = character(0),
        VESSEL_ID = character(0),
        RECENCY = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    # vessel column might not exist; handle gracefully
    v <- if (!is.null(vessel_col) && vessel_col %in% names(df)) {
      as.character(df[[vessel_col]])
    } else {
      rep(NA_character_, n)
    }

    out <- data.frame(
      GEAR   = recode_gear(df[[gear_code]]),
      LAT    = num(df[[lat]]),
      LON    = num(df[[lon]]),
      DT     = parse_dt(df[[dt]]),
      AREA   = suppressWarnings(as.integer(df[[area]])),
      WT_MT  = num(df[[wt_mt]]),
      SOURCE = rep(source_label, n),
      VESSEL_ID = v,
      stringsAsFactors = FALSE
    )
    out$DT_DATE <- as.Date(out$DT)
    out
  }

  # ---- build standardized tables (hard-coded columns) ----
  d_o <- std_points(
    df = data_o,
    source_label = "Observer",
    gear_code = "GEAR_TYPE",
    lat = "LATDD_END",
    lon = "LONDD_END",
    dt  = "RETRIEVAL_DATE",
    area = "NMFS_AREA",
    wt_mt = "WEIGHT",
    vessel_col = "VESSEL"
  )
  # Observer: kg -> mt
  if (nrow(d_o) > 0) d_o$WT_MT <- d_o$WT_MT / 1000

  d_em <- std_points(
    df = data_em,
    source_label = "EM",
    gear_code = "OBS_GEAR_CODE",
    lat = "RETRIEVAL_END_LATITUDE_DD",
    lon = "RETRIEVAL_END_LONGITUDE_DD",
    dt  = "RETRIEVAL_END_DATE",
    area = "REPORTING_AREA_CODE",
    wt_mt = "EXTRAPOLATED_WEIGHT_MT",
    vessel_col = "OBS_VESSEL_ID"
  )

  d <- rbind(d_o, d_em)

  # ---- filters ----
  d <- d[is.finite(d$LAT) & is.finite(d$LON), , drop = FALSE]
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
    stop("No data remaining after filtering (region/gear/date/lat/weight).")
  }

  # ---- longitude normalize ----
  d$LON <- ifelse(d$LON > 180, d$LON - 360, d$LON)
  d$LON <- ifelse(d$LON < -180, d$LON + 360, d$LON)

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

  # ---- counts (computed AFTER filtering) ----
  count_label_df <- NULL
  if (isTRUE(show_counts)) {
    # haul count = rows
    # vessel count = unique across both sources via VESSEL_ID (shared codes)
    if (isTRUE(facet_gear)) {
      tmp <- d
      tmp$VESSEL_ID <- trimws(tmp$VESSEL_ID)
      tmp$VESSEL_ID[tmp$VESSEL_ID == ""] <- NA_character_

      # per-gear: hauls = nrow; vessels = unique non-NA
      hauls <- stats::aggregate(LAT ~ GEAR, data = tmp, FUN = length)
      names(hauls)[2] <- "N_HAULS"
      vessels <- stats::aggregate(VESSEL_ID ~ GEAR, data = tmp, FUN = function(x) length(unique(x[!is.na(x)])))
      names(vessels)[2] <- "N_VESSELS"

      count_label_df <- merge(hauls, vessels, by = "GEAR", all = TRUE)
      count_label_df$LABEL <- paste0("Hauls: ", count_label_df$N_HAULS,
                                     "\nVessels: ", count_label_df$N_VESSELS)
    } else {
      v <- trimws(as.character(d$VESSEL_ID))
      v[v == ""] <- NA_character_
      count_label_df <- data.frame(
        LABEL = paste0(
          "Hauls: ", nrow(d),
          "\nVessels: ", length(unique(v[!is.na(v)]))
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- sf + projection ----
  np_crs <- sf::st_crs("+proj=laea +lat_0=60 +lon_0=-160 +datum=WGS84 +units=m +no_defs")

  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)

  pts <- sf::st_as_sf(d, coords = c("LON","LAT"), crs = 4326, remove = FALSE)
  pts_p <- sf::st_transform(pts, np_crs)

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world_p <- sf::st_transform(world, np_crs)

  # ---- extent from data (projected) ----
  xy <- sf::st_coordinates(pts_p)
  keep <- is.finite(xy[, 1]) & is.finite(xy[, 2])
  xy <- xy[keep, , drop = FALSE]
  pts_p <- pts_p[keep, ]
  if (nrow(xy) == 0) stop("All points invalid after projection (check lon/lat).")

  xmin <- min(xy[, 1]); xmax <- max(xy[, 1])
  ymin <- min(xy[, 2]); ymax <- max(xy[, 2])
  if (xmin == xmax) { xmin <- xmin - 1000; xmax <- xmax + 1000 }
  if (ymin == ymax) { ymin <- ymin - 1000; ymax <- ymax + 1000 }

  pad_x <- (xmax - xmin) * pad_frac
  pad_y <- (ymax - ymin) * pad_frac
  xlim <- c(xmin - pad_x, xmax + pad_x)
  ylim <- c(ymin - pad_y, ymax + pad_y)

  # ---- plot ----
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = world_p,
      fill = "grey95",
      color = "grey70",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = pts_p,
      ggplot2::aes(size = WT_MT, alpha = RECENCY, color = GEAR, shape = SOURCE),
      show.legend = TRUE
    ) +
    ggplot2::scale_shape_manual(
      values = c(Observer = 16, EM = 17),
      drop = FALSE,
      name = "Source"
    ) +
    ggplot2::scale_alpha_continuous(range = c(0.15, 0.95), guide = "none") +
    ggplot2::scale_size_continuous(range = size_range, name = "Weight (mt)") +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
     ggplot2::labs(x = "Longitude",y = "Latitude") +
    ggplot2::theme_bw()

  if (facet_gear) p <- p + ggplot2::facet_wrap(~GEAR,ncol=1)

  if (show_titles) {
    p <- p + ggplot2::labs(title = plot_title, color = "Gear")
  } else {
    p <- p + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  }

  if (show_label) {
    p <- p +
      ggplot2::annotate(
        "label",
        x = Inf, y = Inf,
        label = gear_label,
        hjust = 1.02, vjust = 1.02,
        size = 3
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(7, 12, 7, 7))
  }

  # Upper-left counts
  if (isTRUE(show_counts)) {
    if (facet_gear) {
      p <- p + ggplot2::geom_text(
        data = count_label_df,
        ggplot2::aes(x = -Inf, y = Inf, label = LABEL),
        inherit.aes = FALSE,
        hjust = -0.05, vjust = 1.1, size = 3
      )
    } else {
      p <- p + ggplot2::annotate(
        "text",
        x = -Inf, y = Inf,
        label = count_label_df$LABEL[1],
        hjust = -0.05, vjust = 1.1, size = 3
      )
    }
  }

  p
}
