#' Plot observer CPUE indices filtered by species proportion (with optional council-blend weighting)
#'
#' Pulls observer haul/species composition data using `GET_CURRENT.sql` (via your
#' `sql_filter()` + `sql_run()` utilities), filters hauls by a minimum proportion
#' of the selected species in total catch, computes effort and CPUE, and then
#' produces CPUE index plots.
#'
#' Optionally, the function can compute "council blend" weights using `GET_CODES.sql`
#' + `GET_BLEND.sql` (AKFIN) and apply those weights to observer CPUE before
#' building indices (this mirrors the logic in your current script). :contentReference[oaicite:5]{index=5}
#'
#' Species proportion is computed as:
#' \deqn{prop = (weight\_kg/1000) / official\_total\_catch\_mt}
#'
#' Effort is computed by gear:
#' \itemize{
#'   \item Trawl (GEAR_TYPE 1--5): duration in minutes = RETRIEVAL_DATE - DEPLOYMENT_DATE
#'   \item Pot (GEAR_TYPE 6) and Longline (GEAR_TYPE 8): TOTAL_HOOKS_POTS
#' }
#'
#' @param con Connection object. Recommended: a named list with at least `afsc`,
#'   and (if `use_blend=TRUE`) also `akfin`. Example: `list(afsc=afsc, akfin=akfin)`.
#'   If you pass a single DBI connection, it is assumed to be the AFSC connection.
#' @param species One or more agency species codes (numeric/integer).
#' @param prop_min Minimum species proportion of total catch required to retain a haul
#'   (0--1). Default 0.30.
#' @param date_min Optional start date (inclusive) as "mm/dd/yyyy".
#' @param date_max Optional end date (inclusive) as "mm/dd/yyyy".
#' @param year_min Optional minimum YEAR for SQL filter (YEAR >= year_min). If NULL and
#'   `date_min` is provided, it is derived from `date_min`.
#' @param region Optional region/area selector: one or more of "AI","BS","GOA","BSWGOA"
#'   or explicit NMFS area codes (numeric). If NULL, no area filtering is applied.
#' @param gear Gear selector: one or more of "Trawl","Pot","Longline". Default all.
#' @param plot_type Which aggregation to plot: "MONTH", "GEAR", or "YEAR".
#' @param use_blend Logical; if TRUE, apply council-blend weighting using AKFIN pulls.
#'   Requires `con$akfin` and the SQL files `GET_CODES.sql` and `GET_BLEND.sql`.
#' @param sql_dir Directory containing SQL files. Default "sql".
#' @param base_size ggplot base font size. Default 16.
#'
#' @return A list with components:
#' \itemize{
#'   \item data_obs: filtered haul-level observer data with CPUE fields
#'   \item data_index: indexed/aggregated data used for plotting
#'   \item plots: named list of ggplot objects (weight + number when available)
#' }
#'
#' @export
plot_observer_cpue_prop2 <- function(
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
    BSWGOA = c(500:539, 610:620)
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

  # Required core columns; others are handled opportunistically
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
  d$WEIGHT <- num(d$WEIGHT)                         # kg
  d$OFFICIAL_TOTAL_CATCH <- num(d$OFFICIAL_TOTAL_CATCH) # mt
  d$TOTAL_HOOKS_POTS <- num(d$TOTAL_HOOKS_POTS)

  d$DEPLOYMENT_DATE <- as.POSIXct(d$DEPLOYMENT_DATE, tz = "UTC")
  d$RETRIEVAL_DATE  <- as.POSIXct(d$RETRIEVAL_DATE,  tz = "UTC")

  # Fill MONTH if missing
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

  # COUNT may be missing; only compute number-CPUE if present
  has_count <- "COUNT" %in% names(d)
  if (has_count) d$COUNT <- num(d$COUNT)

  # ---- add gear labels ----
  d$GEAR <- recode_gear(d$GEAR_TYPE)

  # ---- region filter (FIXED: only apply if region is not NULL) ----
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

  if (nrow(d) == 0) {
    return(list(data_obs = d, data_index = d[0,], plots = list()))
  }

  # ---- compute effort ----
  d$DUR_MIN <- as.numeric(difftime(d$RETRIEVAL_DATE, d$DEPLOYMENT_DATE, units = "mins"))
  is_trawl <- d$GEAR == "Trawl"
  d$EFFORT <- NA_real_
  d$EFFORT[is_trawl]  <- d$DUR_MIN[is_trawl]
  d$EFFORT[!is_trawl] <- d$TOTAL_HOOKS_POTS[!is_trawl]

  d <- d[is.finite(d$EFFORT) & d$EFFORT > 0, , drop = FALSE]
  if (nrow(d) == 0) {
    return(list(data_obs = d, data_index = d[0,], plots = list()))
  }

  # ---- proportion filter + CPUE ----
  d$SPECIES_MT <- d$WEIGHT / 1000
  d$PROP_SPECIES <- d$SPECIES_MT / d$OFFICIAL_TOTAL_CATCH

  d <- d[
    is.finite(d$SPECIES_MT) & d$SPECIES_MT >= 0 &
      is.finite(d$OFFICIAL_TOTAL_CATCH) & d$OFFICIAL_TOTAL_CATCH > 0 &
      is.finite(d$PROP_SPECIES) & d$PROP_SPECIES >= prop_min,
    , drop = FALSE
  ]
  if (nrow(d) == 0) {
    return(list(data_obs = d, data_index = d[0,], plots = list()))
  }

  d$CPUE_MT <- d$SPECIES_MT / d$EFFORT
  if (has_count) d$CPUE_N <- d$COUNT / d$EFFORT

  # ---- aggregate helper ----
  dt <- data.table::as.data.table(d)

  # Counts for annotation (haul rows, unique vessels) by grouping units used later
  dt[, NHAUL := .N, by = .(YEAR, GEAR, NMFS_AREA, MONTH)]
  dt[, NVES  := data.table::uniqueN(get(vessel_id_col)), by = .(YEAR, GEAR, NMFS_AREA, MONTH)]

  # Monthly means + SE for CPUE
  haul_month <- dt[, .(
    NHAUL = data.table::uniqueN(get(haul_id_col)),
    NVES  = data.table::uniqueN(get(vessel_id_col)),
    MCPUEW = mean(CPUE_MT, na.rm = TRUE),
    sdCPUEW = stats::sd(CPUE_MT, na.rm = TRUE)
  ), by = .(YEAR, GEAR, NMFS_AREA, MONTH)]

  if (has_count) {
    haul_month_n <- dt[, .(
      MCPUEN = mean(CPUE_N, na.rm = TRUE),
      sdCPUEN = stats::sd(CPUE_N, na.rm = TRUE)
    ), by = .(YEAR, GEAR, NMFS_AREA, MONTH)]
    haul_month <- merge(haul_month, haul_month_n, by = c("YEAR","GEAR","NMFS_AREA","MONTH"), all.x = TRUE)
  }

  # Screen small samples like your original logic (but correct NVES/NHAUL meaning)
  haul_month <- haul_month[NVES >= 2 & NHAUL > 3]

  if (nrow(haul_month) == 0) {
    return(list(data_obs = d, data_index = haul_month, plots = list()))
  }

  # Standard errors (variance on mean); keep as SE (not squared) for plotting
  haul_month[, W_SE := sdCPUEW / sqrt(NHAUL)]
  if (has_count) haul_month[, N_SE := sdCPUEN / sqrt(NHAUL)]

  # Baselines (mean over time) per gear-area for indexing
  base_wa <- haul_month[, .(
    ASMCPUEW = mean(MCPUEW, na.rm = TRUE),
    ASMCPUEN = if (has_count) mean(MCPUEN, na.rm = TRUE) else NA_real_
  ), by = .(GEAR, NMFS_AREA)]

  haul_month <- merge(haul_month, base_wa, by = c("GEAR","NMFS_AREA"), all.x = TRUE)

  haul_month[, WCPUE_INDEX := MCPUEW / ASMCPUEW]
  haul_month[, WCPUE_SE    := W_SE / ASMCPUEW]

  if (has_count) {
    haul_month[, NCPUE_INDEX := MCPUEN / ASMCPUEN]
    haul_month[, NCPUE_SE    := N_SE / ASMCPUEN]
  }

  # ---- optional: apply council-blend weighting (AKFIN) ----
  # For now, keep your existing behavior but make it safe/optional.
  data_index <- haul_month

  if (isTRUE(use_blend)) {
    if (is.null(con_akfin)) stop("use_blend=TRUE requires `con$akfin` (AKFIN connection).")
    # NOTE: This section still assumes your GET_CODES.sql / GET_BLEND.sql formats match
    # what your current function expects. :contentReference[oaicite:6]{index=6}
    # If you want, we can tighten the required columns here the same way as above.

    # GET_CODES.sql
    sql_codes <- file.path(sql_dir, "GET_CODES.sql")
    if (!file.exists(sql_codes)) stop("SQL file not found: ", sql_codes)
    sc <- readLines(sql_codes)
    sc <- sql_filter("IN", species, sc, "-- insert species")
    code <- sql_run(con_afsc, sc)

    # Trip-target codes lookup (your existing path)
    tt_path <- file.path("R", "ALT_TABLES", "TRIP_TARGET_CODES.csv")
    if (!file.exists(tt_path)) stop("Trip target code table not found: ", tt_path)
    TRIP_TARGET_CODE <- data.table::as.data.table(utils::read.csv(tt_path))

    # Determine area label used in TT table
    AREA <- NULL
    if (!is.null(region)) {
      if (any(toupper(as.character(region)) %in% c("BS","AI")) || any(as.integer(region) %in% 500:544)) AREA <- "BSAI"
      if (any(toupper(as.character(region)) %in% c("GOA")) || any(as.integer(region) %in% 600:699)) AREA <- "GOA"
      if (any(toupper(as.character(region)) %in% c("BSWGOA"))) AREA <- c("BSAI","GOA")
    } else {
      # If no region provided, keep both
      AREA <- unique(TRIP_TARGET_CODE$FMP_AREA)
    }

    if ("OBS_SPECIES_CODE" %in% names(TRIP_TARGET_CODE)) {
      TRIP_TARGET_CODE <- TRIP_TARGET_CODE[OBS_SPECIES_CODE %in% species]
      if (!is.null(AREA) && "FMP_AREA" %in% names(TRIP_TARGET_CODE)) {
        TRIP_TARGET_CODE <- TRIP_TARGET_CODE[FMP_AREA %in% AREA]
      }
    }

    TRIP_TARGET <- TRIP_TARGET_CODE$TRIP_TARGET_CODE

    # GET_BLEND.sql
    sql_blend <- file.path(sql_dir, "GET_BLEND.sql")
    if (!file.exists(sql_blend)) stop("SQL file not found: ", sql_blend)
    sb <- readLines(sql_blend)

    sb <- sql_filter("IN", TRIP_TARGET, sb, "-- insert TRIP_TAR_CODE")

    # Determine areas used for blend query
    areas_blend <- sort(unique(data_index$NMFS_AREA))
    sb <- sql_filter("IN", areas_blend, sb, "-- insert AREA")

    # Species group/program code
    if (!("AKR_PROGRAM_CODE" %in% names(code))) stop("GET_CODES.sql output missing AKR_PROGRAM_CODE.")
    SPEC_CODE <- code$AKR_PROGRAM_CODE
    sb <- sql_filter("IN", SPEC_CODE, sb, "-- insert SPECIES_GRP_CODE")

    CATCH <- data.table::as.data.table(sql_run(con_akfin, sb))
    if (nrow(CATCH) > 0) {
      # Harmonize gear codes to observer-style numeric and label
      CATCH[, OBS_GEAR_CODE := 0L]
      CATCH[AGENCY_GEAR_CODE %in% c("PTR","NPT","BTR","TRW"), OBS_GEAR_CODE := 1L]
      CATCH[AGENCY_GEAR_CODE == "POT", OBS_GEAR_CODE := 6L]
      CATCH[AGENCY_GEAR_CODE == "HAL", OBS_GEAR_CODE := 8L]
      CATCH <- CATCH[OBS_GEAR_CODE > 0]
      CATCH[, GEAR := recode_gear(OBS_GEAR_CODE)]

      # Align join keys
      if ("REPORTING_AREA_CODE" %in% names(CATCH)) data.table::setnames(CATCH, "REPORTING_AREA_CODE", "NMFS_AREA")
      CATCH[, YEAR := num(YEAR)]
      CATCH[, MONTH := num(MONTH)]
      CATCH[, NMFS_AREA := num(NMFS_AREA)]

      # Merge with observer monthly index table to get available years/months
      keycols <- c("YEAR","MONTH","GEAR","NMFS_AREA")
      CATCHm <- merge(CATCH, data_index, by = keycols, all.y = TRUE)

      # Compute monthly share within gear-year-area then apply to indices (as in your script)
      Catch_Gear_Month <- CATCHm[, .(MonthGearCatch = sum(TONS, na.rm = TRUE)), by = keycols]
      Catch_Gear_Year  <- CATCHm[, .(YearGearCatch  = sum(TONS, na.rm = TRUE)), by = .(YEAR,GEAR,NMFS_AREA)]
      Catch_Year       <- CATCHm[, .(YearCatch      = sum(TONS, na.rm = TRUE)), by = .(YEAR,NMFS_AREA)]
      CatchPROPYEAR <- merge(Catch_Gear_Year, Catch_Year, by = c("YEAR","NMFS_AREA"))
      CatchPROPYEAR[, YEARGEARPROP := YearGearCatch / YearCatch]

      Catch_Gearprop <- merge(Catch_Gear_Month, Catch_Gear_Year, by = c("YEAR","GEAR","NMFS_AREA"))
      Catch_Gearprop[, CATCHGearPROP := MonthGearCatch / YearGearCatch]

      CATCH3 <- merge(data_index, Catch_Gearprop, by = keycols, all.x = TRUE)
      CATCH3[, CATCHGearPROP := ifelse(is.finite(CATCHGearPROP), CATCHGearPROP, 0)]

      # Apply month weights to index + SE (variance add)
      CATCH3[, WCPUE_INDEX := WCPUE_INDEX * CATCHGearPROP]
      CATCH3[, WCPUE_SE    := WCPUE_SE    * CATCHGearPROP]

      if (has_count) {
        CATCH3[, NCPUE_INDEX := NCPUE_INDEX * CATCHGearPROP]
        CATCH3[, NCPUE_SE    := NCPUE_SE    * CATCHGearPROP]
      }

      # Then re-weight by year gear share and sum over gear-area → YEAR,MONTH
      CATCH3 <- merge(CATCH3, CatchPROPYEAR, by = c("YEAR","GEAR","NMFS_AREA"), all.x = TRUE)
      CATCH3[, YEARGEARPROP := ifelse(is.finite(YEARGEARPROP), YEARGEARPROP, 0)]

      CATCH3[, WCPUE_INDEX := WCPUE_INDEX * YEARGEARPROP]
      CATCH3[, WCPUE_SE    := WCPUE_SE    * YEARGEARPROP]

      if (has_count) {
        CATCH3[, NCPUE_INDEX := NCPUE_INDEX * YEARGEARPROP]
        CATCH3[, NCPUE_SE    := NCPUE_SE    * YEARGEARPROP]
      }

      # Final monthly sum
      data_index <- CATCH3[, .(
        WCPUE_INDEX = sum(WCPUE_INDEX, na.rm = TRUE),
        WCPUE_SE    = sqrt(sum(WCPUE_SE^2, na.rm = TRUE)),
        NCPUE_INDEX = if (has_count) sum(NCPUE_INDEX, na.rm = TRUE) else NA_real_,
        NCPUE_SE    = if (has_count) sqrt(sum(NCPUE_SE^2, na.rm = TRUE)) else NA_real_
      ), by = .(YEAR, MONTH)]
    }
  }

  # ---- build plots ----
  plots <- list()

  if (plot_type == "MONTH") {
    pm <- data_index
    # keep last ~10 years
    yrs <- sort(unique(pm$YEAR))
    yrs <- yrs[is.finite(yrs)]
    if (length(yrs) == 0) return(list(data_obs = d, data_index = data_index, plots = list()))
    final_year <- max(yrs)

    keep <- yrs[yrs >= (as.integer(format(Sys.Date(), "%Y")) - 10)]
    pm <- pm[pm$YEAR %in% keep, , drop = FALSE]

    # colors: all years distinct, final year black
    cols <- scales::hue_pal()(length(keep))
    names(cols) <- as.character(keep)
    cols[as.character(final_year)] <- "black"

    # Weight index plot
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

    # Number index plot (if possible)
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




plot_type <- toupper(plot_type)

if (any(plot_type %in% c("GEAR","YEAR"))) {

  # Ensure data.table
  if (!data.table::is.data.table(CATCH3)) CATCH3 <- data.table::as.data.table(CATCH3)
  if (!data.table::is.data.table(CatchPROPYEAR)) CatchPROPYEAR <- data.table::as.data.table(CatchPROPYEAR)

  # Sum across months within YEAR x GEAR x AREA
  CATCH4 <- CATCH3[, .(
    SMCPUEN2 = sum(MCPUEN2, na.rm = TRUE),
    SMCPUEW2 = sum(MCPUEW2, na.rm = TRUE),
    SN_VAR   = sum(NStERR2, na.rm = TRUE),  # variance components
    SW_VAR   = sum(WStERR2, na.rm = TRUE)
  ), by = .(YEAR, GEAR, NMFS_AREA)]

  # Baseline mean across years for each GEAR x AREA
  CATCH5_base <- CATCH4[, .(
    ASMCPUEN2 = mean(SMCPUEN2, na.rm = TRUE),
    ASMCPUEW2 = mean(SMCPUEW2, na.rm = TRUE)
  ), by = .(GEAR, NMFS_AREA)]

  CATCH4 <- merge(CATCH4, CATCH5_base, by = c("GEAR", "NMFS_AREA"), all.x = TRUE)

  # Indices + variance (still variance, not SE)
  CATCH4[, `:=`(
    NCPUE_INDEX = SMCPUEN2 / ASMCPUEN2,
    WCPUE_INDEX = SMCPUEW2 / ASMCPUEW2,
    N_VAR_IDX   = SN_VAR / (ASMCPUEN2^2),
    W_VAR_IDX   = SW_VAR / (ASMCPUEW2^2)
  )]

  # Keep only needed columns
  CATCH5 <- CATCH4[, .(YEAR, GEAR, NMFS_AREA, NCPUE_INDEX, WCPUE_INDEX, N_VAR_IDX, W_VAR_IDX)]

  # Join year-gear proportions (weights)
  CATCH5 <- merge(
    CATCH5,
    CatchPROPYEAR[, .(YEAR, GEAR, NMFS_AREA, YEARGEARPROP)],
    by = c("YEAR", "GEAR", "NMFS_AREA"),
    all.x = TRUE
  )

  # Replace missing props with 0 (so missing combos contribute nothing)
  CATCH5[!is.finite(YEARGEARPROP), YEARGEARPROP := 0]

  # Apply weights to index and variance (variance scales by weight^2)
  CATCH5[, `:=`(
    NCPUE_W = NCPUE_INDEX * YEARGEARPROP,
    WCPUE_W = WCPUE_INDEX * YEARGEARPROP,
    N_VAR_W = N_VAR_IDX * (YEARGEARPROP^2),
    W_VAR_W = W_VAR_IDX * (YEARGEARPROP^2)
  )]

 }

 if (plot_type == c("GEAR")) {

  # Sum across areas -> YEAR x GEAR
  CATCH5.1 <- CATCH5[, .(
    NCPUE_INDEX = sum(NCPUE_W, na.rm = TRUE),
    WCPUE_INDEX = sum(WCPUE_W, na.rm = TRUE),
    NStERR      = sqrt(sum(N_VAR_W, na.rm = TRUE)),
    WStERR      = sqrt(sum(W_VAR_W, na.rm = TRUE))
  ), by = .(YEAR, GEAR)]

  cpue <- as.data.frame(CATCH5.1)

  # ---- plot styling: latest year thick black ----
  final_year <- suppressWarnings(max(CATCH5.1$YEAR, na.rm = TRUE))

  CATCH5.1[, IS_FINAL := (YEAR == final_year)]

  dw <- ggplot2::ggplot(CATCH5.1, ggplot2::aes(x = YEAR, y = WCPUE_INDEX, group = 1)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = WCPUE_INDEX - WStERR, ymax = WCPUE_INDEX + WStERR),
      width = 0.2
    ) +
    ggplot2::facet_wrap(~GEAR, scales = "free_y", ncol = 1) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
    ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(
      title = paste0("CPUE by weight of fish for ", AREA, " ", code$OBS_PROGRAM_NAME),
      y = "CPUE by weight",
      x = "Year"
    )

  dn <- ggplot2::ggplot(CATCH5.1, ggplot2::aes(x = YEAR, y = NCPUE_INDEX, group = 1)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = NCPUE_INDEX - NStERR, ymax = NCPUE_INDEX + NStERR),
      width = 0.2
    ) +
    ggplot2::facet_wrap(~GEAR, scales = "free_y", ncol = 2) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
    ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(
      title = paste0("CPUE by number of fish for ", AREA, " ", code$OBS_PROGRAM_NAME),
      y = "CPUE by number",
      x = "Year"
    )

  plots$weights <- dw
  plots$number  <- dn
}

if (plot_type == "YEAR") {

  if (!exists("CATCH5")) stop("plot_type='YEAR' requires CATCH5 (run/compute gear-weighted table before YEAR block).")
  if (!data.table::is.data.table(CATCH5)) CATCH5 <- data.table::as.data.table(CATCH5)

  CATCH6 <- CATCH5[, .(
    NCPUE_INDEX = sum(NCPUE_W, na.rm = TRUE),
    WCPUE_INDEX = sum(WCPUE_W, na.rm = TRUE),
    NStERR      = sqrt(sum(N_VAR_W, na.rm = TRUE)),
    WStERR      = sqrt(sum(W_VAR_W, na.rm = TRUE))
  ), by = .(YEAR)]

  cpue <- as.data.frame(CATCH6)

  final_year <- suppressWarnings(max(CATCH6$YEAR, na.rm = TRUE))
  CATCH6[, IS_FINAL := (YEAR == final_year)]

  d1 <- ggplot2::ggplot(CATCH6, ggplot2::aes(x = YEAR, y = WCPUE_INDEX, group = 1)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = WCPUE_INDEX - WStERR, ymax = WCPUE_INDEX + WStERR),
      width = 0.2
    ) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
    ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(
      title = paste0("CPUE by weight of fish for ", AREA, " ", code$OBS_PROGRAM_NAME),
      y = "CPUE by weight",
      x = "Year"
    )

  d2 <- ggplot2::ggplot(CATCH6, ggplot2::aes(x = YEAR, y = NCPUE_INDEX, group = 1)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = IS_FINAL, color = IS_FINAL)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = NCPUE_INDEX - NStERR, ymax = NCPUE_INDEX + NStERR),
      width = 0.2
    ) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
    ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey40"), guide = "none") +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(
      title = paste0("CPUE by number of fish for ", AREA, " ", code$OBS_PROGRAM_NAME),
      y = "CPUE by number",
      x = "Year"
    )

  plots$weights <- d1
  plots$number  <- d2
}

  # You can extend plotting for plot_type == "GEAR" and "YEAR" the same way,
  # but I kept this rewrite focused and correct first (your current file is
  # truncated mid-GEAR block). :contentReference[oaicite:7]{index=7}

  list(
    data_obs = as.data.frame(d),
    data_index = as.data.frame(data_index),
    plots = plots
  )
}
