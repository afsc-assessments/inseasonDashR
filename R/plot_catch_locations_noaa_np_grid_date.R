#' Plot gridded catch locations on a NOAA-style North Pacific map (date-range filter)
#'
#' Aggregates observer and EM catch data into a regular grid (default 20 km)
#' in a North Pacific Lambert Azimuthal Equal-Area projection, then plots
#' grid-cell totals on a NOAA-style basemap.
#'
#' Date filtering is applied (inclusive) using:
#'  - Observer: RETRIEVAL_DATE
#'  - EM:       RETRIEVAL_END_DATE
#'
#' @param data_o Observer data.frame
#' @param data_em EM data.frame
#' @param species_name Character string used in the plot title (if titles shown)
#' @param date_min Optional start date (inclusive) as "mm/dd/yyyy"
#' @param date_max Optional end date (inclusive) as "mm/dd/yyyy"
#' @param region Vector: one or more of "AI","BS","GOA","BSWGOA", or NMFS area/s.
#' @param gear Character vector: one or more of "Trawl","Pot","Longline". Default all.
#' @param cell_km Grid cell size in kilometers (default 20)
#' @param pad_frac Padding around data extent for grid creation (default 0.08)
#' @param facet_gear If TRUE, facet the gridded map by gear (aggregates by GRID_ID + GEAR).
#' @param show_titles If FALSE, remove plot title/subtitle/caption (clean figure).
#' @param show_label If FALSE, remove the upper-right info label.
#'
#' @return A ggplot object
#' @export
plot_catch_locations_noaa_np_grid_date <- function(
  data_o,
  data_em,
  species_name,
  date_min = NULL,
  date_max = NULL,
  region = c("AI", "BS", "GOA"),
  gear = c("Trawl", "Pot", "Longline"),
  cell_km = 20,
  pad_frac = 0.08,
  facet_gear = FALSE,
  show_titles = TRUE,
  show_label = TRUE
) {

  stopifnot(is.data.frame(data_o), is.data.frame(data_em))
  stopifnot(is.character(species_name), length(species_name) == 1)

  # ---- required columns (fail fast) ----
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

  if(any(region %in% c("AI","BS","GOA","BSWGOA"))){
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
  }else area_codes = region

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

  # parse "mm/dd/yyyy" safely
  parse_mdy <- function(s) {
    if (is.null(s)) return(NULL)
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

  # ---- standardize to common schema ----
  d_o <- data.frame(
    GEAR   = recode_gear(data_o$GEAR_TYPE),
    LAT    = num(data_o$LATDD_END),
    LON    = num(data_o$LONDD_END),
    DT     = parse_dt(data_o$RETRIEVAL_DATE),
    AREA   = suppressWarnings(as.integer(data_o$NMFS_AREA)),
    WT_MT  = num(data_o$WEIGHT) / 1000,           # kg -> mt
    stringsAsFactors = FALSE
  )

  d_em <- data.frame(
    GEAR   = recode_gear(data_em$OBS_GEAR_CODE),
    LAT    = num(data_em$RETRIEVAL_END_LATITUDE_DD),
    LON    = num(data_em$RETRIEVAL_END_LONGITUDE_DD),
    DT     = parse_dt(data_em$RETRIEVAL_END_DATE),
    AREA   = suppressWarnings(as.integer(data_em$REPORTING_AREA_CODE)),
    WT_MT  = num(data_em$EXTRAPOLATED_WEIGHT_MT), # already mt
    stringsAsFactors = FALSE
  )

  d <- rbind(d_o, d_em)

  # ---- base filters ----
  d <- d[is.finite(d$LAT) & is.finite(d$LON) & is.finite(d$WT_MT), ]
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

  if (nrow(d) == 0) {
    stop("No records remain after filtering (lat/date/region/gear/weight>0).")
  }

  # ---- normalize longitude to [-180, 180] ----
  d$LON <- ifelse(d$LON > 180, d$LON - 360, d$LON)
  d$LON <- ifelse(d$LON < -180, d$LON + 360, d$LON)

  # ---- sf + NOAA-ish NP projection ----
  np_crs <- sf::st_crs("+proj=laea +lat_0=60 +lon_0=-160 +datum=WGS84 +units=m +no_defs")

  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)

  pts <- sf::st_as_sf(d, coords = c("LON","LAT"), crs = 4326, remove = FALSE)
  pts_p <- sf::st_transform(pts, np_crs)

  # drop any points that become invalid after projection
  xy <- sf::st_coordinates(pts_p)
  keep <- is.finite(xy[, 1]) & is.finite(xy[, 2])
  pts_p <- pts_p[keep, ]
  xy <- xy[keep, , drop = FALSE]
  if (nrow(xy) == 0) stop("All points became invalid after projection (check lon/lat ranges).")

  # ---- robust bbox from finite projected coordinates ----
  xmin <- min(xy[, 1]); xmax <- max(xy[, 1])
  ymin <- min(xy[, 2]); ymax <- max(xy[, 2])
  if (xmin == xmax) { xmin <- xmin - 1000; xmax <- xmax + 1000 }
  if (ymin == ymax) { ymin <- ymin - 1000; ymax <- ymax + 1000 }

  pad_x <- (xmax - xmin) * pad_frac
  pad_y <- (ymax - ymin) * pad_frac

  bb_pad <- sf::st_bbox(
    c(xmin = xmin - pad_x, ymin = ymin - pad_y,
      xmax = xmax + pad_x, ymax = ymax + pad_y),
    crs = np_crs
  )

  # ---- build grid ----
  cell_m <- cell_km * 1000
  grid <- sf::st_make_grid(
    sf::st_as_sfc(bb_pad),
    cellsize = c(cell_m, cell_m),
    what = "polygons",
    square = TRUE
  )
  grid <- sf::st_sf(GRID_ID = seq_along(grid), geometry = grid)

  # ---- assign points to cells ----
  hit <- sf::st_intersects(pts_p, grid)
  pts_p$GRID_ID <- vapply(hit, function(x) if (length(x) == 0) NA_integer_ else x[1], integer(1))
  pts_p <- pts_p[!is.na(pts_p$GRID_ID), ]
  if (nrow(pts_p) == 0) stop("No points fell within the constructed grid (unexpected).")

  # ---- aggregate weights ----
  w <- sf::st_drop_geometry(pts_p)

  if (isTRUE(facet_gear)) {
    agg <- aggregate(WT_MT ~ GRID_ID + GEAR, data = w, FUN = sum, na.rm = TRUE)
    names(agg)[3] <- "WT_MT_SUM"
    grid2 <- merge(grid, agg, by = "GRID_ID", all.x = FALSE)
  } else {
    agg <- aggregate(WT_MT ~ GRID_ID, data = w, FUN = sum, na.rm = TRUE)
    names(agg)[2] <- "WT_MT_SUM"
    grid2 <- merge(grid, agg, by = "GRID_ID", all.x = FALSE)
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

  # ---- upper-right label: gear + grid size ----
  gear_label <- if (isTRUE(facet_gear)) {
    "Gear: faceted"
  } else {
    paste0("Gear: ", paste(gear, collapse = ", "))
  }
  info_label <- paste0(gear_label, "\nGrid: ", cell_km, " km")

  # ---- basemap + plot ----
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world_p <- sf::st_transform(world, np_crs)

  p <- ggplot2::ggplot() +
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
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(7, 12, 7, 7)
    )

  if (isTRUE(facet_gear)) {
    p <- p + ggplot2::facet_wrap(~GEAR,ncol=1)
  }

  # titles on/off
  if (isTRUE(show_titles)) {
    p <- p + ggplot2::labs(title = plot_title)
  } else {
    p <- p + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  }

  # label on/off
  if (isTRUE(show_label) && (isTRUE(show_titles) || isTRUE(show_label))) {
    p <- p + ggplot2::annotate(
      "label",
      x = Inf, y = Inf,
      label = info_label,
      hjust = 1.02, vjust = 1.02,
      size = 3
    )
  }

  p
}
