#' Plot observer CPUE indices filtered by species proportion (no data.table :=)
#'
#' @export
plot_observer_cpue_prop3 <- function(
  con,
  species,
  prop_min = 0.30,
  date_min = NULL,
  date_max = NULL,
  year_min = NULL,
  region = NULL,
  gear = c("Trawl", "Pot", "Longline"),
  plot_type = c("MONTH", "GEAR", "YEAR"),
  use_blend = TRUE,
  sql_dir = "sql",
  base_size = 16
) {
  plot_type <- match.arg(plot_type)

  # ---- validate / normalize connections ----
  if (missing(con)) stop("`con` must be provided.")
  if (missing(species) || length(species) < 1) stop("`species` must be provided.")
  if (!is.numeric(species)) stop("`species` must be numeric/integer.")
  if (!is.numeric(prop_min) || length(prop_min) != 1 || !is.finite(prop_min) || prop_min < 0 || prop_min > 1) {
    stop("`prop_min` must be a single number between 0 and 1.")
  }

  # Accept either list(afsc=..., akfin=...) or a single DBI connection (assumed AFSC)
  if (is.list(con) && !is.null(con$afsc)) {
    con_afsc <- con$afsc
    con_akfin <- con$akfin
  } else {
    con_afsc <- con
    con_akfin <- NULL
  }

  # ---- helpers ----
  parse_mdy <- function(x) {
    if (is.null(x)) return(NULL)
    d <- as.Date(x, format = "%m/%d/%Y")
    if (is.na(d)) stop("Date must be in 'mm/dd/yyyy' format: ", x)
    d
  }

  num <- function(x) suppressWarnings(as.numeric(x))
  int <- function(x) suppressWarnings(as.integer(x))

  recode_gear <- function(x) {
    x <- suppressWarnings(as.integer(x))
    g <- rep("Other", length(x))
    g[x %in% 1:5] <- "Trawl"
    g[x == 6]     <- "Pot"
    g[x == 8]     <- "Longline"
    g
  }

  region_map <- list(
    AI     = 540:544,
    BS     = 500:539,
    GOA    = 600:699,
    BSWGOA = c(500:539, 610:620),
    ALL    = c(500:699)
  )

  allowed_gears <- c("Trawl", "Pot", "Longline")
  gear <- unique(as.character(gear))
  bad_g <- setdiff(gear, allowed_gears)
  if (length(bad_g)) stop("Unknown gear: ", paste(bad_g, collapse = ", "),
                          ". Allowed: ", paste(allowed_gears, collapse = ", "))

  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)
  if (!is.null(dmin) && !is.null(dmax) && dmax < dmin) stop("date_max must be >= date_min.")
  if (is.null(year_min) && !is.null(dmin)) year_min <- as.integer(format(dmin, "%Y"))

  # ---- read + run GET_CURRENT.sql ----
  sql_file <- file.path(sql_dir, "GET_CURRENT.sql")
  if (!file.exists(sql_file)) stop("SQL file not found: ", sql_file)
  sql_code <- readLines(sql_file)

  sql_code <- sql_filter(sql_precode = "IN", x = species, sql_code = sql_code, flag = "-- insert species")
  if (!is.null(year_min)) {
    sql_code <- sql_filter(sql_precode = ">=", x = year_min, sql_code = sql_code, flag = "-- insert year")
  }

  d <- sql_run(con_afsc, sql_code)
  d <- dplyr::rename_all(d, toupper)

  req <- c(
    "SPECIES","YEAR","GEAR_TYPE","NMFS_AREA","WEIGHT","OFFICIAL_TOTAL_CATCH",
    "DEPLOYMENT_DATE","RETRIEVAL_DATE","TOTAL_HOOKS_POTS"
  )
  miss <- setdiff(req, names(d))
  if (length(miss)) stop("SQL output missing column(s): ", paste(miss, collapse = ", "))

  # ---- type coercions ----
  d$YEAR <- int(d$YEAR)
  d$NMFS_AREA <- int(d$NMFS_AREA)
  d$GEAR_TYPE <- int(d$GEAR_TYPE)
  d$WEIGHT <- num(d$WEIGHT)                              # kg
  d$OFFICIAL_TOTAL_CATCH <- num(d$OFFICIAL_TOTAL_CATCH)  # mt
  d$TOTAL_HOOKS_POTS <- num(d$TOTAL_HOOKS_POTS)

  d$DEPLOYMENT_DATE <- as.POSIXct(d$DEPLOYMENT_DATE, tz = "UTC")
  d$RETRIEVAL_DATE  <- as.POSIXct(d$RETRIEVAL_DATE,  tz = "UTC")

  if (!("MONTH" %in% names(d))) {
    d$MONTH <- suppressWarnings(as.integer(format(as.Date(d$RETRIEVAL_DATE), "%m")))
  } else {
    d$MONTH <- int(d$MONTH)
  }

  # Haul id (for counts). Prefer HAUL_JOIN if present; else create row id.
  haul_id_col <- if ("HAUL_JOIN" %in% names(d)) "HAUL_JOIN" else NULL
  if (is.null(haul_id_col)) {
    d$HAUL_JOIN <- seq_len(nrow(d))
    haul_id_col <- "HAUL_JOIN"
  }

  # Vessel id (for vessel counts). Try common candidates.
  vessel_id_col <- NULL
  for (cand in c("VESSEL", "VESSEL_ID", "OBS_VESSEL_ID", "CRUISE_VESSEL_ID")) {
    if (cand %in% names(d)) { vessel_id_col <- cand; break }
  }
  if (is.null(vessel_id_col)) {
    d$VESSEL_ID_TMP <- NA_character_
    vessel_id_col <- "VESSEL_ID_TMP"
  }

  has_count <- "COUNT" %in% names(d)
  if (has_count) d$COUNT <- num(d$COUNT)

  d$GEAR <- recode_gear(d$GEAR_TYPE)

  # ---- region filter ----
  if (!is.null(region)) {
    if (any(toupper(as.character(region)) %in% names(region_map))) {
      r <- unique(toupper(as.character(region)))
      bad_r <- setdiff(r, names(region_map))
      if (length(bad_r)) stop("Unknown region: ", paste(bad_r, collapse = ", "),
                              ". Allowed: ", paste(names(region_map), collapse = ", "))
      areas <- sort(unique(unlist(region_map[r])))
    } else {
      areas <- sort(unique(as.integer(region)))
      if (anyNA(areas)) stop("If `region` is not one of AI/BS/GOA/BSWGOA, it must be numeric NMFS area codes.")
    }
    d <- d[d$NMFS_AREA %in% areas, , drop = FALSE]
  }

  # ---- gear filter ----
  d <- d[d$GEAR %in% gear, , drop = FALSE]

  # ---- date filter (inclusive) ----
  if (!is.null(dmin) || !is.null(dmax)) {
    dd <- as.Date(d$RETRIEVAL_DATE)
    if (!is.null(dmin)) d <- d[!is.na(dd) & dd >= dmin, , drop = FALSE]
    if (!is.null(dmax)) d <- d[!is.na(dd) & dd <= dmax, , drop = FALSE]
  }

  if (nrow(d) == 0) return(list(data_obs = d, data_index = d[0,], plots = list()))

  # ---- effort ----
  d$DUR_MIN <- as.numeric(difftime(d$RETRIEVAL_DATE, d$DEPLOYMENT_DATE, units = "mins"))
  is_trawl <- d$GEAR == "Trawl"
  d$EFFORT <- NA_real_
  d$EFFORT[is_trawl]  <- d$DUR_MIN[is_trawl]
  d$EFFORT[!is_trawl] <- d$TOTAL_HOOKS_POTS[!is_trawl]

  d <- d[is.finite(d$EFFORT) & d$EFFORT > 0, , drop = FALSE]
  if (nrow(d) == 0) return(list(data_obs = d, data_index = d[0,], plots = list()))

  # ---- proportion + CPUE ----
  d$SPECIES_MT   <- d$WEIGHT / 1000
  d$PROP_SPECIES <- d$SPECIES_MT / d$OFFICIAL_TOTAL_CATCH

  d <- d[
    is.finite(d$SPECIES_MT) & d$SPECIES_MT >= 0 &
      is.finite(d$OFFICIAL_TOTAL_CATCH) & d$OFFICIAL_TOTAL_CATCH > 0 &
      is.finite(d$PROP_SPECIES) & d$PROP_SPECIES >= prop_min,
    , drop = FALSE
  ]
  if (nrow(d) == 0) return(list(data_obs = d, data_index = d[0,], plots = list()))

  d$CPUE_MT <- d$SPECIES_MT / d$EFFORT
  if (has_count) d$CPUE_N <- d$COUNT / d$EFFORT

  # ---- monthly aggregation (NO data.table) ----
  dt <- dplyr::as_tibble(d)

  haul_month <- dt %>%
    dplyr::group_by(.data$YEAR, .data$GEAR, .data$NMFS_AREA, .data$MONTH) %>%
    dplyr::summarise(
      NHAUL   = dplyr::n_distinct(.data[[haul_id_col]]),
      NVES    = dplyr::n_distinct(.data[[vessel_id_col]]),
      MCPUEW  = mean(.data$CPUE_MT, na.rm = TRUE),
      sdCPUEW = stats::sd(.data$CPUE_MT, na.rm = TRUE),
      MCPUEN  = if (has_count) mean(.data$CPUE_N, na.rm = TRUE) else NA_real_,
      sdCPUEN = if (has_count) stats::sd(.data$CPUE_N, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$NVES >= 2, .data$NHAUL > 3)

  if (nrow(haul_month) == 0) return(list(data_obs = d, data_index = as.data.frame(haul_month), plots = list()))

  haul_month <- haul_month %>%
    dplyr::mutate(
      W_SE = .data$sdCPUEW / sqrt(.data$NHAUL),
      N_SE = if (has_count) .data$sdCPUEN / sqrt(.data$NHAUL) else NA_real_
    )

  # Baseline mean across all months/years per gear-area
  base_wa <- haul_month %>%
    dplyr::group_by(.data$GEAR, .data$NMFS_AREA) %>%
    dplyr::summarise(
      ASMCPUEW = mean(.data$MCPUEW, na.rm = TRUE),
      ASMCPUEN = if (has_count) mean(.data$MCPUEN, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )

  haul_month <- haul_month %>%
    dplyr::left_join(base_wa, by = c("GEAR","NMFS_AREA")) %>%
    dplyr::mutate(
      WCPUE_INDEX = .data$MCPUEW / .data$ASMCPUEW,
      WCPUE_SE    = .data$W_SE   / .data$ASMCPUEW,
      NCPUE_INDEX = if (has_count) .data$MCPUEN / .data$ASMCPUEN else NA_real_,
      NCPUE_SE    = if (has_count) .data$N_SE   / .data$ASMCPUEN else NA_real_
    )

  # This is the “core” monthly table
  data_index_month <- haul_month

  # ---- optional: council-blend weighting (still no data.table) ----
  if (isTRUE(use_blend)) {
    if (is.null(con_akfin)) stop("use_blend=TRUE requires `con$akfin` (AKFIN connection).")

    # GET_CODES.sql
    sql_codes <- file.path(sql_dir, "GET_CODES.sql")
    if (!file.exists(sql_codes)) stop("SQL file not found: ", sql_codes)
    sc <- readLines(sql_codes)
    sc <- sql_filter("IN", species, sc, "-- insert species")
    code <- sql_run(con_afsc, sc)

    tt_path <- file.path("R", "ALT_TABLES", "TRIP_TARGET_CODES.csv")
    if (!file.exists(tt_path)) stop("Trip target code table not found: ", tt_path)
    TRIP_TARGET_CODE <- dplyr::as_tibble(utils::read.csv(tt_path))

    AREA <- NULL
    if (!is.null(region)) {
      if (any(toupper(as.character(region)) %in% c("BS","AI")) || any(as.integer(region) %in% 500:544)) AREA <- "BSAI"
      if (any(toupper(as.character(region)) %in% c("GOA")) || any(as.integer(region) %in% 600:699)) AREA <- "GOA"
      if (any(toupper(as.character(region)) %in% c("BSWGOA"))) AREA <- c("BSAI","GOA")
    } else {
      AREA <- unique(TRIP_TARGET_CODE$FMP_AREA)
    }

    if ("OBS_SPECIES_CODE" %in% names(TRIP_TARGET_CODE)) {
      TRIP_TARGET_CODE <- TRIP_TARGET_CODE %>%
        dplyr::filter(.data$OBS_SPECIES_CODE %in% species)
      if (!is.null(AREA) && "FMP_AREA" %in% names(TRIP_TARGET_CODE)) {
        TRIP_TARGET_CODE <- TRIP_TARGET_CODE %>% dplyr::filter(.data$FMP_AREA %in% AREA)
      }
    }
    TRIP_TARGET <- TRIP_TARGET_CODE$TRIP_TARGET_CODE

    # GET_BLEND.sql
    sql_blend <- file.path(sql_dir, "GET_BLEND.sql")
    if (!file.exists(sql_blend)) stop("SQL file not found: ", sql_blend)
    sb <- readLines(sql_blend)
    sb <- sql_filter("IN", TRIP_TARGET, sb, "-- insert TRIP_TAR_CODE")

    areas_blend <- sort(unique(data_index_month$NMFS_AREA))
    sb <- sql_filter("IN", areas_blend, sb, "-- insert AREA")

    if (!("AKR_PROGRAM_CODE" %in% names(code))) stop("GET_CODES.sql output missing AKR_PROGRAM_CODE.")
    SPEC_CODE <- code$AKR_PROGRAM_CODE
    sb <- sql_filter("IN", SPEC_CODE, sb, "-- insert SPECIES_GRP_CODE")

    CATCH <- dplyr::as_tibble(sql_run(con_akfin, sb))
    if (nrow(CATCH) > 0) {
      # Gear mapping
      CATCH <- CATCH %>%
        dplyr::mutate(
          OBS_GEAR_CODE = dplyr::case_when(
            .data$AGENCY_GEAR_CODE %in% c("PTR","NPT","BTR","TRW") ~ 1L,
            .data$AGENCY_GEAR_CODE == "POT" ~ 6L,
            .data$AGENCY_GEAR_CODE == "HAL" ~ 8L,
            TRUE ~ 0L
          ),
          GEAR = recode_gear(.data$OBS_GEAR_CODE)
        ) %>%
        dplyr::filter(.data$OBS_GEAR_CODE > 0)

      # Harmonize names
      if ("REPORTING_AREA_CODE" %in% names(CATCH)) {
        CATCH <- CATCH %>% dplyr::rename(NMFS_AREA = .data$REPORTING_AREA_CODE)
      }

      CATCH <- CATCH %>%
        dplyr::mutate(
          YEAR = num(.data$YEAR),
          MONTH = num(.data$MONTH),
          NMFS_AREA = num(.data$NMFS_AREA)
        )

      keycols <- c("YEAR","MONTH","GEAR","NMFS_AREA")

      # Keep only months/gear/area present in observer index table
      CATCHm <- CATCH %>%
        dplyr::inner_join(
          data_index_month %>% dplyr::select(dplyr::all_of(keycols)),
          by = keycols
        )

      # Shares
      Catch_Gear_Month <- CATCHm %>%
        dplyr::group_by(.data$YEAR, .data$MONTH, .data$GEAR, .data$NMFS_AREA) %>%
        dplyr::summarise(MonthGearCatch = sum(.data$TONS, na.rm = TRUE), .groups = "drop")

      Catch_Gear_Year <- CATCHm %>%
        dplyr::group_by(.data$YEAR, .data$GEAR, .data$NMFS_AREA) %>%
        dplyr::summarise(YearGearCatch = sum(.data$TONS, na.rm = TRUE), .groups = "drop")

      Catch_Year <- CATCHm %>%
        dplyr::group_by(.data$YEAR, .data$NMFS_AREA) %>%
        dplyr::summarise(YearCatch = sum(.data$TONS, na.rm = TRUE), .groups = "drop")

      CatchPROPYEAR <- Catch_Gear_Year %>%
        dplyr::left_join(Catch_Year, by = c("YEAR","NMFS_AREA")) %>%
        dplyr::mutate(
          YEARGEARPROP = dplyr::if_else(is.finite(.data$YearGearCatch / .data$YearCatch),
                                        .data$YearGearCatch / .data$YearCatch, 0)
        ) %>%
        dplyr::select(.data$YEAR, .data$GEAR, .data$NMFS_AREA, .data$YEARGEARPROP)

      Catch_Gearprop <- Catch_Gear_Month %>%
        dplyr::left_join(Catch_Gear_Year, by = c("YEAR","GEAR","NMFS_AREA")) %>%
        dplyr::mutate(
          CATCHGearPROP = dplyr::if_else(is.finite(.data$MonthGearCatch / .data$YearGearCatch),
                                         .data$MonthGearCatch / .data$YearGearCatch, 0)
        ) %>%
        dplyr::select(.data$YEAR, .data$MONTH, .data$GEAR, .data$NMFS_AREA, .data$CATCHGearPROP)

      # Apply month weights then year-gear weights (SE scales linearly; variances add later when aggregating)
      data_index_month <- data_index_month %>%
        dplyr::left_join(Catch_Gearprop, by = keycols) %>%
        dplyr::mutate(
          CATCHGearPROP = dplyr::if_else(is.finite(.data$CATCHGearPROP), .data$CATCHGearPROP, 0),
          WCPUE_INDEX = .data$WCPUE_INDEX * .data$CATCHGearPROP,
          WCPUE_SE    = .data$WCPUE_SE    * .data$CATCHGearPROP,
          NCPUE_INDEX = if (has_count) .data$NCPUE_INDEX * .data$CATCHGearPROP else NA_real_,
          NCPUE_SE    = if (has_count) .data$NCPUE_SE    * .data$CATCHGearPROP else NA_real_
        ) %>%
        dplyr::left_join(CatchPROPYEAR, by = c("YEAR","GEAR","NMFS_AREA")) %>%
        dplyr::mutate(
          YEARGEARPROP = dplyr::if_else(is.finite(.data$YEARGEARPROP), .data$YEARGEARPROP, 0),
          WCPUE_INDEX = .data$WCPUE_INDEX * .data$YEARGEARPROP,
          WCPUE_SE    = .data$WCPUE_SE    * .data$YEARGEARPROP,
          NCPUE_INDEX = if (has_count) .data$NCPUE_INDEX * .data$YEARGEARPROP else NA_real_,
          NCPUE_SE    = if (has_count) .data$NCPUE_SE    * .data$YEARGEARPROP else NA_real_
        )
    }
  }

  # ---- build plotting table depending on plot_type ----
  if (plot_type == "MONTH") {
    plot_df <- data_index_month
  }

  if (plot_type == "GEAR") {
    # Aggregate over months and areas -> YEAR x GEAR
    plot_df <- data_index_month %>%
      dplyr::group_by(.data$YEAR, .data$GEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data$WCPUE_INDEX, na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data$WCPUE_SE)^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data$NCPUE_INDEX, na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data$NCPUE_SE)^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
  }

  if (plot_type == "YEAR") {
    # Aggregate over months, gears, and areas -> YEAR
    plot_df <- data_index_month %>%
      dplyr::group_by(.data$YEAR) %>%
      dplyr::summarise(
        WCPUE_INDEX = sum(.data$WCPUE_INDEX, na.rm = TRUE),
        WCPUE_SE    = sqrt(sum((.data$WCPUE_SE)^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(.data$NCPUE_INDEX, na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum((.data$NCPUE_SE)^2, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
  }

  # ---- build plots ----
  plots <- list()

  if (plot_type == "MONTH") {
    pm <- plot_df

    yrs <- sort(unique(pm$YEAR))
    yrs <- yrs[is.finite(yrs)]
    if (length(yrs) == 0) return(list(data_obs = d, data_index = as.data.frame(plot_df), plots = list()))
    final_year <- max(yrs)

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
      plots$number <- p_n
    }
  }

  if (plot_type == "GEAR") {
    pg <- plot_df
    final_year <- suppressWarnings(max(pg$YEAR, na.rm = TRUE))
    pg$IS_FINAL <- (pg$YEAR == final_year)

    dw <- ggplot2::ggplot(pg, ggplot2::aes(x = YEAR, y = WCPUE_INDEX, group = 1)) +
      ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = WCPUE_INDEX - WCPUE_SE, ymax = WCPUE_INDEX + WCPUE_SE), width = 0.2) +
      ggplot2::facet_wrap(~GEAR, scales = "free_y", ncol = 1) +
      ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
      ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::labs(title = "CPUE index (weight) by gear", y = "CPUE index (weight)", x = "Year")

    plots$weight <- dw

    if (has_count) {
      dn <- ggplot2::ggplot(pg, ggplot2::aes(x = YEAR, y = NCPUE_INDEX, group = 1)) +
        ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = NCPUE_INDEX - NCPUE_SE, ymax = NCPUE_INDEX + NCPUE_SE), width = 0.2) +
        ggplot2::facet_wrap(~GEAR, scales = "free_y", ncol = 2) +
        ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
        ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::labs(title = "CPUE index (number) by gear", y = "CPUE index (number)", x = "Year")

      plots$number <- dn
    }
  }

  if (plot_type == "YEAR") {
    py <- plot_df
    final_year <- suppressWarnings(max(py$YEAR, na.rm = TRUE))
    py$IS_FINAL <- (py$YEAR == final_year)

    d1 <- ggplot2::ggplot(py, ggplot2::aes(x = YEAR, y = WCPUE_INDEX, group = 1)) +
      ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = WCPUE_INDEX - WCPUE_SE, ymax = WCPUE_INDEX + WCPUE_SE), width = 0.2) +
      ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
      ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::labs(title = "CPUE index (weight) by year", y = "CPUE index (weight)", x = "Year")

    plots$weight <- d1

    if (has_count) {
      d2 <- ggplot2::ggplot(py, ggplot2::aes(x = YEAR, y = NCPUE_INDEX, group = 1)) +
        ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = NCPUE_INDEX - NCPUE_SE, ymax = NCPUE_INDEX + NCPUE_SE), width = 0.2) +
        ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
        ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::labs(title = "CPUE index (number) by year", y = "CPUE index (number)", x = "Year")

      plots$number <- d2
    }
  }

  list(
    data_obs   = as.data.frame(d),
    data_index = as.data.frame(plot_df),
    plots      = plots
  )
}
