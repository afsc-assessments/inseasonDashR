#' Plot cumulative catch by week with one line per year
#'
#' Uses WEEK_END_DATE (YYYY-MM-DD) to compute ISO week and year.
#' Produces cumulative weekly catch curves with one line per year.
#' The most recent year is always drawn in black and thicker.
#'
#' Gear is taken from FMP_GEAR coded as:
#'   TRW = Trawl, POT = Pot, HAL = Longline, JIG = Jig
#'
#' Weight is taken from WEIGHT_POSTED (metric tons).
#' Area is taken from REPORTING_AREA_CODE.
#'
#' @param catch data.frame from GET_ALL_CATCH.sql
#' @param region Optional region selector: "AI","BS","GOA","BSWGOA"
#' @param areas Optional explicit area codes (overrides region)
#' @param facet_gear Logical; if TRUE, facet by gear type
#' @param gears Character vector of gears to include after recoding
#' @param show_titles Logical; if FALSE, remove title/subtitle
#' @param title Optional plot title
#' @param y_units Y-axis label
#' @param show_totals Logical; if TRUE, annotate final-year catch in upper-left
#'
#' @return ggplot object
#' @export
plot_cumulative_catch_by_week <- function(
  catch,
  region = NULL,
  areas = NULL,
  facet_gear = FALSE,
  gears = c("Trawl", "Pot", "Longline", "Jig"),
  show_titles = TRUE,
  title = NULL,
  y_units = "Cumulative catch (mt)",
  show_totals = TRUE
) {

  stopifnot(is.data.frame(catch))

  req <- c("WEEK_END_DATE", "REPORTING_AREA_CODE", "FMP_GEAR", "WEIGHT_POSTED")
  miss <- setdiff(req, names(catch))
  if (length(miss)) {
    stop("Missing required column(s): ", paste(miss, collapse = ", "))
  }

  if (!requireNamespace("lubridate", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages lubridate, dplyr, and tidyr are required.")
  }

  # ---- helpers ----
  recode_fmp_gear <- function(x) {
    x <- toupper(trimws(as.character(x)))
    out <- rep("Other", length(x))
    out[x == "TRW"] <- "Trawl"
    out[x == "POT"] <- "Pot"
    out[x == "HAL"] <- "Longline"
    out[x == "JIG"] <- "Jig"
    out
  }

  region_map <- list(
    AI     = 540:544,
    BS     = 500:539,
    GOA    = 600:699,
    BSWGOA = c(500:539, 610:620),
    ALL= c(500:699)
  )

  d <- catch

  # ---- date -> year/week ----
  d$DT <- as.Date(d$WEEK_END_DATE)
  if (any(is.na(d$DT))) stop("WEEK_END_DATE must be YYYY-MM-DD.")
  d$YEAR <- lubridate::year(d$DT)
  d$WEEK <- lubridate::isoweek(d$DT)

  # ---- weight, area, gear ----
  d$WT_MT <- suppressWarnings(as.numeric(d$WEIGHT_POSTED))
  d$AREA  <- suppressWarnings(as.integer(d$REPORTING_AREA_CODE))
  d$GEAR  <- recode_fmp_gear(d$FMP_GEAR)

  # ---- region / area filter ----
  region_label <- NULL
  if (!is.null(areas)) {
    areas <- as.integer(areas)
    d <- d[d$AREA %in% areas, , drop = FALSE]
    region_label <- paste0("Area: ", paste(sort(unique(areas)), collapse = ", "))
  } else if (!is.null(region)) {
    region <- unique(toupper(region))
    area_codes <- sort(unique(unlist(region_map[region])))
    d <- d[d$AREA %in% area_codes, , drop = FALSE]
    region_label <- paste0("Region: ", paste(region, collapse = ", "))
  }

  # ---- gear filter ----
  d <- d[d$GEAR %in% gears, , drop = FALSE]

  # ---- clean ----
  d <- d[
    is.finite(d$WT_MT) & d$WT_MT >= 0 &
    is.finite(d$YEAR) &
    d$WEEK >= 1 & d$WEEK <= 53,
    , drop = FALSE
  ]
  if (!nrow(d)) stop("No data remaining after filtering.")

  # ---- final year ----
  final_year <- max(d$YEAR, na.rm = TRUE)

  # ---- totals (final year only) ----
  if (isTRUE(show_totals)) {
    if (facet_gear) {
      totals_df <- stats::aggregate(
        WT_MT ~ GEAR,
        data = d[d$YEAR == final_year, ],
        FUN = sum
      )
      totals_df$LABEL <- paste0(
        final_year, ": ",
        formatC(totals_df$WT_MT, format = "f", digits = 1, big.mark = ","),
        " mt"
      )
    } else {
      totals_df <- data.frame(
        LABEL = paste0(
          final_year, ": ",
          formatC(sum(d$WT_MT[d$YEAR == final_year]), format = "f",
                  digits = 1, big.mark = ","),
          " mt"
        )
      )
    }
  }

  # ---- collapse gear if not faceting ----
  if (!facet_gear) d$GEAR <- "All"

  # ---- weekly aggregation ----
  agg <- stats::aggregate(
    WT_MT ~ YEAR + WEEK + GEAR,
    data = d,
    FUN = sum
  )

  # ---- complete weeks + cumulative ----
  agg <- dplyr::as_tibble(agg) |>
    tidyr::complete(YEAR, GEAR, WEEK = 1:53, fill = list(WT_MT = 0)) |>
    dplyr::arrange(GEAR, YEAR, WEEK) |>
    dplyr::group_by(GEAR, YEAR) |>
    dplyr::mutate(CUM_MT = cumsum(WT_MT)) |>
    dplyr::ungroup()

  # ---- color + linewidth mapping ----
  yrs <- sort(unique(agg$YEAR))
  older <- yrs[yrs != final_year]

  year_colors <- setNames(
    c(grDevices::hcl.colors(length(older), "Dark 3"), "black"),
    c(as.character(older), as.character(final_year))
  )

  line_sizes <- ifelse(agg$YEAR == final_year, 1.2, 0.6)

  # ---- title ----
  if (is.null(title)) {
    title <- paste0("Cumulative catch by week (", min(yrs), "–", max(yrs), ")")
  }

  # ---- plot ----
  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(
      x = WEEK, y = CUM_MT,
      group = YEAR,
      color = factor(YEAR),
      linewidth = factor(YEAR == final_year)
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = year_colors, name = "Year") +
    ggplot2::scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.6), guide = "none") +
    ggplot2::labs(x = "Week", y = y_units) +
    ggplot2::theme_bw()

  if (facet_gear) p <- p + ggplot2::facet_wrap(~GEAR,scales="free_y")

  if (show_titles) {
    p <- p + ggplot2::labs(title = title)
    if (!is.null(region_label)) p <- p + ggplot2::labs(subtitle = region_label)
  } else {
    p <- p + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  }

  # ---- totals annotation (upper-left) ----
  if (isTRUE(show_totals)) {
    if (facet_gear) {
      p <- p + ggplot2::geom_text(
        data = totals_df,
        ggplot2::aes(x = -Inf, y = Inf, label = LABEL),
        inherit.aes = FALSE,
        hjust = -0.05, vjust = 1.1, size = 5
      )
    } else {
      p <- p + ggplot2::annotate(
        "text",
        x = -Inf, y = Inf,
        label = totals_df$LABEL,
        hjust = -0.05, vjust = 1.1, size = 5
      )
    }
  }

  p
}
