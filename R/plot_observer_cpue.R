#' Plot Observer CPUE Time Series (Proportional Catch Method)
#'
#' Generates CPUE indices by number and weight using observer data,
#' where CPUE is weighted by the proportion of the selected species
#' in total catch. Indices may be aggregated by year or by year × gear,
#' with associated standard errors.
#'
#' This function is designed for use in in-season dashboards and
#' assessment support tools. It assumes effort definitions differ by
#' gear type (e.g., trawl duration vs hooks/pots).
#'
#' @param data_o data.frame or data.table
#'   Observer haul-level data containing species weight, effort,
#'   gear, year, and NMFS area.
#'
#' @param species Character or numeric.
#'   Species identifier used for labeling outputs.
#'
#' @param AREA Character.
#'   Human-readable area label used in plot titles (e.g., "Bering Sea").
#'
#' @param code data.frame.
#'   Lookup table containing observer program metadata (must include
#'   \code{OBS_PROGRAM_NAME}).
#'
#' @param plot_type Character.
#'   Either \code{"GEAR"} (separate CPUE series by gear) or
#'   \code{"Year"} (aggregated across gear).
#'
#' @param base_size Numeric.
#'   Base font size passed to \code{ggplot2::theme_bw()}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{cpue}{data.frame containing CPUE indices and standard errors}
#'   \item{plot_weight}{ggplot object for weight-based CPUE}
#'   \item{plot_number}{ggplot object for number-based CPUE}
#' }
#'
#' @details
#' CPUE indices are standardized by the mean CPUE within each gear and
#' area combination prior to aggregation. Standard errors are propagated
#' assuming independence.
#'
#' Weight-based CPUE is in metric tons per unit effort.
#'
#' @seealso
#' \code{\link{get_observer_cpue_data}},
#' \code{\link{plot_cumulative_catch_by_week}}
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_errorbar facet_wrap
#'   theme_bw labs
#'
#' @export

plot_observer_cpue <- function(
  pulled,
  plot_type = c("MONTH", "GEAR", "YEAR"),
  month_gear_facet =FALSE,
  base_size = 16
) {
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  requireNamespace("scales")

  plot_type <- match.arg(plot_type)


  subtitle_std <- ""
  if (is.null(pulled$data_index_month) || nrow(pulled$data_index_month) == 0) {
    return(list(
      data_index = pulled$data_index_month,
      plots = list(),
      plot_type = plot_type
    ))
  }

  has_count <- isTRUE(pulled$meta$has_count)

  di <- dplyr::as_tibble(pulled$data_index_month)

  # Choose which index columns to use.
  # - *_FLEET columns represent catch-weighted blending across gears (fleet index).
  # - *_GEAR  columns represent gear-standardized indices (not down-weighted by annual gear share).
  want_gear_index <- isTRUE(plot_type == "GEAR") || (isTRUE(plot_type == "MONTH") && isTRUE(month_gear_facet))

  if (want_gear_index && all(c("WCPUE_INDEX_GEAR","WCPUE_SE_GEAR") %in% names(di))) {
    w_idx_col <- "WCPUE_INDEX_GEAR"
    w_se_col  <- "WCPUE_SE_GEAR"
    n_idx_col <- if (has_count && "NCPUE_INDEX_GEAR" %in% names(di)) "NCPUE_INDEX_GEAR" else "NCPUE_INDEX"
    n_se_col  <- if (has_count && "NCPUE_SE_GEAR"    %in% names(di)) "NCPUE_SE_GEAR"    else "NCPUE_SE"
  } else if (!want_gear_index && all(c("WCPUE_INDEX_FLEET","WCPUE_SE_FLEET") %in% names(di))) {
    w_idx_col <- "WCPUE_INDEX_FLEET"
    w_se_col  <- "WCPUE_SE_FLEET"
    n_idx_col <- if (has_count && "NCPUE_INDEX_FLEET" %in% names(di)) "NCPUE_INDEX_FLEET" else "NCPUE_INDEX"
    n_se_col  <- if (has_count && "NCPUE_SE_FLEET"    %in% names(di)) "NCPUE_SE_FLEET"    else "NCPUE_SE"
  } else {
    # Backward compatible fallback
    w_idx_col <- "WCPUE_INDEX"
    w_se_col  <- "WCPUE_SE"
    n_idx_col <- "NCPUE_INDEX"
    n_se_col  <- "NCPUE_SE"
  }

  # Build plot_df from monthly index (this replaces the old GEAR/YEAR legacy blocks)
  if (plot_type == "MONTH") {

  if (!isTRUE(month_gear_facet)) {
    # current behavior: summed across gear
    plot_df <- di %>%
      dplyr::group_by(.data$YEAR, .data$MONTH) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data[[w_idx_col]], na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data[[w_se_col]])^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data[[n_idx_col]], na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data[[n_se_col]])^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )

  } else {
    # keep gear-specific monthly series
    #plot_df <- di

    plot_df <- di %>%
      dplyr::group_by(.data$YEAR, .data$MONTH,.data$GEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data[[w_idx_col]], na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data[[w_se_col]])^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data[[n_idx_col]], na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data[[n_se_col]])^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
  }
}
 else if (plot_type == "GEAR") {
    plot_df <- di %>%
      dplyr::group_by(.data$YEAR, .data$GEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data[[w_idx_col]], na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data[[w_se_col]])^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data[[n_idx_col]], na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data[[n_se_col]])^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
  } else { # YEAR
    plot_df <- di %>%
      dplyr::group_by(.data$YEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data[[w_idx_col]], na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data[[w_se_col]])^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data[[n_idx_col]], na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data[[n_se_col]])^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
  }

  plots <- list()

  if (plot_type == "MONTH") {

    pm <- plot_df
    pm <- subset(pm, WCPUE_INDEX > 0 )
    yrs <- sort(unique(pm$YEAR))
    yrs <- yrs[is.finite(yrs)]
    final_year <- max(yrs)

    # last 10-ish years if you want; feel free to change this logic
    keep <- yrs[yrs >= (as.integer(format(Sys.Date(), "%Y")) - 10)]
    pm <- pm[pm$YEAR %in% keep, , drop = FALSE]

    cols <- scales::hue_pal()(length(keep))
    names(cols) <- as.character(keep)
    cols[as.character(final_year)] <- "black"


    p_w <- ggplot2::ggplot(
      pm,
      ggplot2::aes(x = factor(MONTH), y = WCPUE_INDEX, color = factor(YEAR), group = YEAR)
    ) +
      ggplot2::geom_line(ggplot2::aes(linewidth = (YEAR == final_year)), alpha = 0.55) +
      ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.8), guide = "none") +
      ggplot2::geom_errorbar(
        data = pm[pm$YEAR == final_year, , drop = FALSE],
        ggplot2::aes(ymin = WCPUE_INDEX - WCPUE_SE, ymax = WCPUE_INDEX + WCPUE_SE),
        width = 0.2,
        color = "black",
        alpha = 1
      ) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::labs(x = "Month", y = "CPUE index (weight)", color = "Year")

      if (plot_type == "MONTH" &&  isTRUE(month_gear_facet)) {
          p_w <- p_w +
          ggplot2::facet_wrap(~GEAR, scales = "free_y")+ggplot2::geom_point()
        }


    plots$weight <- p_w

    if (has_count && all(c("NCPUE_INDEX","NCPUE_SE") %in% names(pm))) {
      p_n <- ggplot2::ggplot(
        pm,
        ggplot2::aes(x = factor(MONTH), y = NCPUE_INDEX, color = factor(YEAR), group = YEAR)
      ) +
        ggplot2::geom_line(ggplot2::aes(linewidth = (YEAR == final_year)), alpha = 0.55) +
        ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.8), guide = "none") +
        ggplot2::geom_errorbar(
          data = pm[pm$YEAR == final_year, , drop = FALSE],
          ggplot2::aes(ymin = NCPUE_INDEX - NCPUE_SE, ymax = NCPUE_INDEX + NCPUE_SE),
          width = 0.2,
          color = "black",
          alpha = 1
        ) +
        ggplot2::scale_color_manual(values = cols) +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::labs(x = "Month", y = "CPUE index (number)", color = "Year")

        if (plot_type == "MONTH" &&  isTRUE(month_gear_facet)) {
          p_n <- p_n +
          ggplot2::facet_wrap(~GEAR, scales = "free_y")+ggplot2::geom_point()
        }

      plots$number <- p_n
    }
  }

  if (plot_type == "GEAR") {
    pg <- plot_df
    pg <- subset(pg, WCPUE_INDEX > 0)
    final_year <- suppressWarnings(max(pg$YEAR, na.rm = TRUE))
    pg <- dplyr::mutate(pg, IS_FINAL = .data$YEAR == final_year)

    p_w <- ggplot2::ggplot(pg, ggplot2::aes(x = YEAR, y = WCPUE_INDEX, group = 1)) +
      ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = WCPUE_INDEX - WCPUE_SE, ymax = WCPUE_INDEX + WCPUE_SE), width = 0.2) +
      ggplot2::facet_wrap(~GEAR, scales = "free_y") +
      ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
      ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::labs(title = "CPUE index (weight) by gear", y = "CPUE index (weight)", x = "Year")

    plots$weight <- p_w

    if (has_count) {
      p_n <- ggplot2::ggplot(pg, ggplot2::aes(x = YEAR, y = NCPUE_INDEX, group = 1)) +
        ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = NCPUE_INDEX - NCPUE_SE, ymax = NCPUE_INDEX + NCPUE_SE), width = 0.2) +
        ggplot2::facet_wrap(~GEAR, scales = "free_y") +
        ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
        ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::labs(title = "CPUE index (number) by gear", y = "CPUE index (number)", x = "Year")

      plots$number <- p_n
    }
  }

  if (plot_type == "YEAR") {
    py <- plot_df
    py<-subset(py,WCPUE_INDEX > 0)
    final_year <- suppressWarnings(max(py$YEAR, na.rm = TRUE))
    py <- dplyr::mutate(py, IS_FINAL = .data$YEAR == final_year)

    p_w <- ggplot2::ggplot(py, ggplot2::aes(x = YEAR, y = WCPUE_INDEX, group = 1)) +
      ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = WCPUE_INDEX - WCPUE_SE, ymax = WCPUE_INDEX + WCPUE_SE), width = 0.2) +
      ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
      ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::labs(title = "CPUE index (weight) by year", y = "CPUE index (weight)", x = "Year")

    plots$weight <- p_w

    if (has_count) {
      p_n <- ggplot2::ggplot(py, ggplot2::aes(x = YEAR, y = NCPUE_INDEX, group = 1)) +
        ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = NCPUE_INDEX - NCPUE_SE, ymax = NCPUE_INDEX + NCPUE_SE), width = 0.2) +
        ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
        ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::labs(title = "CPUE index (number) by year", y = "CPUE index (number)", x = "Year")

      plots$number <- p_n
    }
  }

  list(
    data_index = as.data.frame(plot_df),
    plots = plots,
    plot_type = plot_type
  )
}