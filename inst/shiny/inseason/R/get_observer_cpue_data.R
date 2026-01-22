
get_observer_cpue_data <- function(
  con,
  species,
  prop_min = 0.30,
  date_min = NULL,
  date_max = NULL,
  year_min = NULL,
  region = NULL,
  gear = c("Trawl", "Pot", "Longline"),
  use_blend = TRUE,
  sql_dir = "sql"
) {
  # ---- dependencies ----
  requireNamespace("dplyr")
  requireNamespace("stats")

  # ---- validate ----
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

  allowed_gears <- c("Trawl", "Pot", "Longline")
  gear <- unique(as.character(gear))
  bad_g <- setdiff(gear, allowed_gears)
  if (length(bad_g)) stop("Unknown gear: ", paste(bad_g, collapse = ", "),
                          ". Allowed: ", paste(allowed_gears, collapse = ", "))

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
    g[x > 6]     <- "Longline"
    g
  }

  region_map <- list(
    AI     = 540:544,
    BS     = 500:539,
    GOA    = 600:699,
    BSWGOA = c(500:539, 610:620),
    ALL= c(500:699)
  )

  # ---- dates ----
  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)
  if (!is.null(dmin) && !is.null(dmax) && dmax < dmin) stop("date_max must be >= date_min.")
  if (is.null(year_min) && !is.null(dmin)) year_min <- as.integer(format(dmin, "%Y"))

  # ---- SQL: GET_CURRENT.sql ----
  sql_file <- file.path(sql_dir, "GET_CURRENT.sql")
  if (!file.exists(sql_file)) stop("SQL file not found: ", sql_file)
  sql_code <- readLines(sql_file)

  # These helper functions are assumed to exist in your package/project:
  # - sql_filter(sql_precode, x, sql_code, flag)
  # - sql_run(con, sql_code)
  sql_code <- sql_filter(sql_precode = "IN",  x = species, sql_code = sql_code, flag = "-- insert species")
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

  # ---- types ----
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

  # Haul id
  haul_id_col <- if ("HAUL_JOIN" %in% names(d)) "HAUL_JOIN" else NULL
  if (is.null(haul_id_col)) {
    d$HAUL_JOIN <- seq_len(nrow(d))
    haul_id_col <- "HAUL_JOIN"
  }

  # Vessel id (best-effort)
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
    r_chr <- toupper(as.character(region))
    if (any(r_chr %in% names(region_map))) {
      r <- unique(r_chr)
      bad_r <- setdiff(r, names(region_map))
      if (length(bad_r)) stop("Unknown region: ", paste(bad_r, collapse = ", "),
                              ". Allowed: ", paste(names(region_map), collapse = ", "))
      areas <- sort(unique(unlist(region_map[r])))
    } else {
      areas <- sort(unique(as.integer(region)))
      if (anyNA(areas)) stop("If `region` is not AI/BS/GOA/BSWGOA, it must be numeric NMFS area codes.")
    }
    d <- d[d$NMFS_AREA %in% areas, , drop = FALSE]
  }

  # ---- gear filter ----
  d <- d[d$GEAR %in% gear, , drop = FALSE]

  # ---- date filter ----
  if (!is.null(dmin) || !is.null(dmax)) {
    dd <- as.Date(d$RETRIEVAL_DATE)
    if (!is.null(dmin)) d <- d[!is.na(dd) & dd >= dmin, , drop = FALSE]
    if (!is.null(dmax)) d <- d[!is.na(dd) & dd <= dmax, , drop = FALSE]
  }

  if (nrow(d) == 0) {
    return(list(
      data_obs = d,
      data_index_month = d[0, , drop = FALSE],
      meta = list(has_count = has_count, haul_id_col = haul_id_col, vessel_id_col = vessel_id_col)
    ))
  }

  # ---- effort ----
  d <- subset(d,!is.na(RETRIEVAL_DATE) & !is.na(DEPLOYMENT_DATE))

  d$DUR_MIN <- as.numeric(difftime(d$RETRIEVAL_DATE, d$DEPLOYMENT_DATE, units = "mins"))
  is_trawl <- d$GEAR == "Trawl"
  d$EFFORT <- NA_real_
  d$EFFORT[is_trawl]  <- d$DUR_MIN[is_trawl]
  d$EFFORT[!is_trawl] <- d$TOTAL_HOOKS_POTS[!is_trawl]

  d <- d[is.finite(d$EFFORT) & d$EFFORT > 0, , drop = FALSE]
  if (nrow(d) == 0) {
    return(list(
      data_obs = d,
      data_index_month = d[0, , drop = FALSE],
      meta = list(has_count = has_count, haul_id_col = haul_id_col, vessel_id_col = vessel_id_col)
    ))
  }

  # ---- cpue + prop filter ----
  d$SPECIES_MT   <- d$WEIGHT / 1000
  d$PROP_SPECIES <- d$SPECIES_MT / d$OFFICIAL_TOTAL_CATCH

  d <- d[
    is.finite(d$SPECIES_MT) & d$SPECIES_MT >= 0 &
      is.finite(d$OFFICIAL_TOTAL_CATCH) & d$OFFICIAL_TOTAL_CATCH > 0 &
      is.finite(d$PROP_SPECIES) & d$PROP_SPECIES >= prop_min,
    , drop = FALSE
  ]

  if (nrow(d) == 0) {
    return(list(
      data_obs = d,
      data_index_month = d[0, , drop = FALSE],
      meta = list(has_count = has_count, haul_id_col = haul_id_col, vessel_id_col = vessel_id_col)
    ))
  }

  d$CPUE_MT <- d$SPECIES_MT / d$EFFORT
  if (has_count) d$CPUE_N <- d$COUNT / d$EFFORT

  # ---- monthly aggregation ----
  dt <- dplyr::as_tibble(d)

  data_index_month <- dt %>%
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
    dplyr::filter(.data$NVES >= 2, .data$NHAUL > 3) %>%
    dplyr::mutate(
      W_SE = .data$sdCPUEW / sqrt(.data$NHAUL),
      N_SE = if (has_count) .data$sdCPUEN / sqrt(.data$NHAUL) else NA_real_
    )

  if (nrow(data_index_month) == 0) {
    return(list(
      data_obs = as.data.frame(d),
      data_index_month = as.data.frame(data_index_month),
      meta = list(has_count = has_count, haul_id_col = haul_id_col, vessel_id_col = vessel_id_col)
    ))
  }

  base_wa <- data_index_month %>%
    dplyr::group_by(.data$GEAR, .data$NMFS_AREA) %>%
    dplyr::summarise(
      ASMCPUEW = mean(.data$MCPUEW, na.rm = TRUE),
      ASMCPUEN = if (has_count) mean(.data$MCPUEN, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )

  data_index_month <- data_index_month %>%
    dplyr::left_join(base_wa, by = c("GEAR","NMFS_AREA")) %>%
    dplyr::mutate(
      WCPUE_INDEX = .data$MCPUEW / .data$ASMCPUEW,
      WCPUE_SE    = .data$W_SE   / .data$ASMCPUEW,
      NCPUE_INDEX = if (has_count) .data$MCPUEN / .data$ASMCPUEN else NA_real_,
      NCPUE_SE    = if (has_count) .data$N_SE   / .data$ASMCPUEN else NA_real_
    )

  # ---- optional: blend weighting ----
  if (isTRUE(use_blend)) {
    if (is.null(con_akfin)) stop("use_blend=TRUE requires `con$akfin` (AKFIN connection).")

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
      rr <- toupper(as.character(region))
      if (any(rr %in% c("BS","AI")) || any(as.integer(region) %in% 500:544)) AREA <- "BSAI"
      if (any(rr %in% c("GOA"))    || any(as.integer(region) %in% 600:699)) AREA <- "GOA"
      if (any(rr %in% c("BSWGOA"))) AREA <- c("BSAI","GOA")
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

      CATCHm <- CATCH %>%
        dplyr::inner_join(data_index_month %>% dplyr::select(dplyr::all_of(keycols)), by = keycols)

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

  list(
    data_obs = as.data.frame(d),
    data_index_month = as.data.frame(data_index_month),
    meta = list(
      has_count = has_count,
      haul_id_col = haul_id_col,
      vessel_id_col = vessel_id_col
    )
  )
}

