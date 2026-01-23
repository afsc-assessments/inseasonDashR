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

  if (is.null(pulled$data_index_month) || nrow(pulled$data_index_month) == 0) {
    return(list(
      data_index = pulled$data_index_month,
      plots = list(),
      plot_type = plot_type
    ))
  }

  has_count <- isTRUE(pulled$meta$has_count)

  di <- dplyr::as_tibble(pulled$data_index_month)

  # Build plot_df from monthly index (this replaces the old GEAR/YEAR legacy blocks)
  if (plot_type == "MONTH") {

  if (!isTRUE(month_gear_facet)) {
    # current behavior: summed across gear
    plot_df <- di %>%
      dplyr::group_by(.data$YEAR, .data$MONTH) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data$WCPUE_INDEX, na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data$WCPUE_SE)^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data$NCPUE_INDEX, na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data$NCPUE_SE)^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )

  } else {
    # keep gear-specific monthly series
    #plot_df <- di

    plot_df <- di %>%
      dplyr::group_by(.data$YEAR, .data$MONTH,.data$GEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data$WCPUE_INDEX, na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data$WCPUE_SE)^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data$NCPUE_INDEX, na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data$NCPUE_SE)^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
  }
}
 else if (plot_type == "GEAR") {
    plot_df <- di %>%
      dplyr::group_by(.data$YEAR, .data$GEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data$WCPUE_INDEX, na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data$WCPUE_SE)^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data$NCPUE_INDEX, na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data$NCPUE_SE)^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
  } else { # YEAR
    plot_df <- di %>%
      dplyr::group_by(.data$YEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data$WCPUE_INDEX, na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data$WCPUE_SE)^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data$NCPUE_INDEX, na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data$NCPUE_SE)^2, na.rm = TRUE)) else NA_real_,
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


# =========================
# Example usage
# =========================
# pulled <- pull_observer_cpue_prop2_data(
#   con = list(afsc = afsc, akfin = akfin),
#   species = 21740,
#   prop_min = 0.30,
#   region = "BS",
#   gear = c("Trawl","Pot"),
#   use_blend = TRUE,
#   sql_dir = "inst/sql"
# )
#
# out <- plot_observer_cpue_prop2(pulled, plot_type = "MONTH")
# out$plots$weight
