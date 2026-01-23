#' Plot length-frequency composition (with region & gear options)
#'
#' Expects `lf` with columns:
#'   SPECIES, NMFS_AREA, GEAR, LENGTH, FREQUENCY
#'
#' @param lf data.frame/data.table with length-frequency output
#' @param species_name Optional species label for title
#' @param date_min Optional start date (character "mm/dd/yyyy") for title only
#' @param date_max Optional end date (character "mm/dd/yyyy") for title only
#' @param region Optional region selector: "AI","BS","GOA","BSWGOA"
#' @param areas Optional explicit NMFS area codes (overrides region)
#' @param facet_gear If TRUE, facet by gear
#' @param show_titles If FALSE, remove title entirely
#' @param show_label If FALSE, remove upper-right label
#' @param show_n If TRUE, add sample-size label(s) in the upper-left
#' @param y_free If TRUE and faceting, allow free y-scales
#'
#' @return ggplot object
#' @export
plot_length_frequency_noaa <- function(
  lf,
  species_name = NULL,
  date_min = NULL,
  date_max = NULL,
  region = NULL,
  areas = NULL,
  gear = c("Trawl", "Pot", "Longline"),
  facet_gear = FALSE,
  show_titles = TRUE,
  show_label = TRUE,
  show_n = TRUE,
  y_free = TRUE
) {

parse_mdy <- function(s) {
    if (is.null(s)) return(NULL)
    if (!is.character(s) || length(s) != 1) {
      stop("date_min/date_max must be character scalars like \"mm/dd/yyyy\".")
    }
    out <- suppressWarnings(as.Date(s, format = "%m/%d/%Y"))
    if (is.na(out)) {
      stop("Could not parse date \"", s,
           "\". Expected format is \"mm/dd/yyyy\" (e.g., \"01/15/2023\").")
    }
    out
  }

  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)

if (is.null(lf$raw) || !is.data.frame(lf$raw) || nrow(lf$raw) == 0) {
  return(
    empty_message_plot(
      "No length data available"
    )
  )
  }

  dat <- as.data.frame(lf$raw)
  dat <- subset(dat, RETRIEVAL_DATE >= dmin & RETRIEVAL_DATE <= dmax & GEAR %in% gear) 

  lf <- data.table::data.table(dat)[
    is.finite(LENGTH) & is.finite(FREQUENCY),
    .(FREQUENCY = sum(FREQUENCY, na.rm = TRUE)),
    by = .(SPECIES, NMFS_AREA, GEAR, LENGTH)
  ][order(SPECIES, NMFS_AREA, GEAR, LENGTH)]

  lf <- as.data.frame(lf)


  req <- c("SPECIES", "NMFS_AREA", "GEAR", "LENGTH", "FREQUENCY")
  miss <- setdiff(req, names(lf))
  if (length(miss)) {
    stop("lf is missing required column(s): ", paste(miss, collapse = ", "))
  }

  # ---- region mapping ----
  region_map <- list(
    AI     = 540:544,
    BS     = 500:539,
    GOA    = 600:699,
    BSWGOA = c(500:539, 610:620),
    ALL = c(500:699)
  )

  


  # ---- coercion / cleanup ----
  lf$NMFS_AREA <- suppressWarnings(as.integer(lf$NMFS_AREA))
  lf$LENGTH    <- suppressWarnings(as.numeric(lf$LENGTH))
  lf$FREQUENCY <- suppressWarnings(as.numeric(lf$FREQUENCY))
  lf$GEAR      <- as.character(lf$GEAR)
  lf$SPECIES   <- as.character(lf$SPECIES)

  lf <- lf[
    is.finite(lf$NMFS_AREA) &
      is.finite(lf$LENGTH) &
      is.finite(lf$FREQUENCY) &
      lf$FREQUENCY > 0,
    , drop = FALSE
  ]

  # ---- area / region filter ----
  if (!is.null(areas)) {
    areas <- as.integer(areas)
    lf <- lf[lf$NMFS_AREA %in% areas, , drop = FALSE]
    region_label <- paste0("Area: ", paste(sort(unique(areas)), collapse = ", "))
  } else if (!is.null(region)) {
    region <- unique(toupper(as.character(region)))
    bad <- setdiff(region, names(region_map))
    if (length(bad)) {
      stop("Unknown region(s): ", paste(bad, collapse = ", "),
           ". Allowed: ", paste(names(region_map), collapse = ", "))
    }
    sel_areas <- sort(unique(unlist(region_map[region])))
    lf <- lf[lf$NMFS_AREA %in% sel_areas, , drop = FALSE]
    region_label <- paste0("Region: ", paste(region, collapse = ", "))
  } else {
    region_label <- "Region: all"
  }

  if (!nrow(lf)) stop("No data remaining after region/area filtering.")

  # ---- species label ----
  if (is.null(species_name)) {
    sp <- unique(lf$SPECIES)
    species_name <- if (length(sp) == 1) sp else "Selected species"
  }

  # ---- date label (title only) ----
  parse_mdy <- function(x) {
    if (is.null(x)) return(NULL)
    as.Date(x, format = "%m/%d/%Y")
  }
  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)

  date_label <- if (!is.null(dmin) && !is.null(dmax)) {
    paste0(format(dmin, "%Y-%m-%d"), " to ", format(dmax, "%Y-%m-%d"))
  } else if (!is.null(dmin)) {
    paste0(format(dmin, "%Y-%m-%d"), " to present")
  } else if (!is.null(dmax)) {
    paste0("Up to ", format(dmax, "%Y-%m-%d"))
  } else {
    NULL
  }

  plot_title <- if (!is.null(date_label)) {
    paste0(species_name, " (", date_label, ")")
  } else {
    species_name
  }

  # ---- info label (upper-right) ----
  gear_label <- if (facet_gear) "Gear: faceted" else "Gear: combined"
  info_label <- paste(region_label, gear_label, sep = "\n")

  # ---- N labels (upper-left) ----
  fmt_n <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
  n_label_df <- NULL

  if (isTRUE(show_n)) {
    if (isTRUE(facet_gear)) {
      # one per facet panel
      n_by_gear <- stats::aggregate(FREQUENCY ~ GEAR, data = lf, FUN = sum, na.rm = TRUE)
      n_by_gear$N_LABEL <- paste0("n = ", fmt_n(n_by_gear$FREQUENCY))
      n_label_df <- n_by_gear
    } else {
      # single label
      n_total <- sum(lf$FREQUENCY, na.rm = TRUE)
      n_label_df <- data.frame(N_LABEL = paste0("n = ", fmt_n(n_total)))
    }
  }

  # ---- plot ----
  p <- ggplot2::ggplot(lf, ggplot2::aes(x = LENGTH, y = FREQUENCY))

  if (facet_gear) {
    p <- p +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(~GEAR, scales = if (y_free) "free_y" else "fixed", ncol = 1) +
      ggplot2::labs(x = "Length (cm)", y = "Frequency")
  } else {
    p <- p +
      ggplot2::geom_col(ggplot2::aes(fill = GEAR), alpha = 0.7) +
      ggplot2::labs(x = "Length (cm)", y = "Frequency", fill = "Gear")
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(7, 12, 7, 7)
    )

  # titles on/off
  if (show_titles) {
    p <- p + ggplot2::labs(title = plot_title)
  } else {
    p <- p + ggplot2::labs(title = NULL)
  }

  # upper-right label on/off
  if (show_label) {
    p <- p + ggplot2::annotate(
      "label",
      x = Inf, y = Inf,
      hjust = 1.02, vjust = 1.02,
      label = info_label,
      size = 5
    )
  }

  # upper-left N label(s)
  if (isTRUE(show_n)) {
    if (isTRUE(facet_gear)) {
      p <- p + ggplot2::geom_text(
        data = n_label_df,
        ggplot2::aes(x = -Inf, y = Inf, label = N_LABEL),
        inherit.aes = FALSE,
        hjust = -0.05, vjust = 1.1,
        size = 5
      )
    } else {
      p <- p + ggplot2::annotate(
        "text",
        x = -Inf, y = Inf,
        hjust = -0.05, vjust = 1.1,
        label = n_label_df$N_LABEL[1],
        size = 5
      )
    }
  }

  p
}
