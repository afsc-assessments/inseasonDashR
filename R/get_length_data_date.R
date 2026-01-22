#' Pull observer length-frequency data (AFSC) by species and date range
#'
#' Uses the SQL template `sql/GET_CURRENT_LENGTH.sql`, inserting a species filter
#' at the `-- insert species` flag and a year filter (>= first year from date_min)
#' at the `-- insert year` flag, using the legacy `utils.r` helpers.
#'
#' Length-frequency is summarized by SPECIES x GEAR x LENGTH.
#'
#' @param afsc DBI connection to the AFSC database.
#' @param species One or more species codes used in `OBSINT.CURRENT_SPCOMP.species`.
#' @param date_min Start date (inclusive) as "mm/dd/yyyy". Required.
#' @param date_max End date (inclusive) as "mm/dd/yyyy". Optional.
#' @param gear Optional gear filter: any of c("Trawl","Pot","Longline"). Default all.
#' @param tz Timezone used when parsing datetime strings (default "UTC").
#' @param return_sql If TRUE, return the final SQL text used (default TRUE).
#'
#' @return A list with:
#' \describe{
#'   \item{raw}{data.table of pulled rows after date/gear filtering}
#'   \item{lf}{data.frame length-frequency aggregated by SPECIES, GEAR, LENGTH}
#'   \item{sql}{(optional) list(sql = <character vector>, first_year_used = <int>)}
#' }
#'
#' @export
get_length_freq_data_date <- function(
  afsc,
  species,
  date_min,
  date_max = NULL,
  gear = c("Trawl", "Pot", "Longline"),
  tz = "UTC",
  return_sql = TRUE
) {

  stopifnot(!missing(afsc))
  stopifnot(length(species) >= 1)

  # ---- helpers ----
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

  parse_dt_to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
    y <- suppressWarnings(as.POSIXct(x, tz = tz))
    if (!all(is.na(y))) return(as.Date(y))
    suppressWarnings(as.Date(x))
  }

  recode_gear <- function(x) {
    x <- suppressWarnings(as.integer(x))
    g <- rep("Other", length(x))
    g[x %in% 1:5] <- "Trawl"
    g[x == 6]     <- "Pot"
    g[x == 8]     <- "Longline"
    g
  }

  # ---- parse dates ----
  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)

  # derive year for SQL performance filter
  first_year <- as.integer(format(dmin, "%Y"))

  # ---- read SQL template ----
  sql_file <- system.file("sql", "GET_CURRENT_LENGTH.sql", package = "inseasonDashR")
  sql_len <- readLines(sql_file)

  # species filter (your SQL uses column named "species")
  sql_len <- sql_filter(
    sql_precode = "IN",
    x = species,
    sql_code = sql_len,
    flag = "-- insert species"
  )

  # year filter
  sql_len <- sql_filter_num(
    sql_precode = ">=",
    x = first_year,
    sql_code = sql_len,
    flag = "-- insert year"
  )

  # ---- run query ----
  dat <- sql_run(afsc, sql_len) |>
    dplyr::rename_all(toupper) |>
    data.table::as.data.table()

  if (nrow(dat) == 0) {
    out <- list(raw = dat, lf = dat[0])
    if (isTRUE(return_sql)) {
      out$sql <- list(sql = sql_len, first_year_used = first_year)
    }
    return(out)
  }

  # ---- date filter (inclusive) ----
  if ("RETRIEVAL_DATE" %in% names(dat)) {
    dat[, RETRIEVAL_DATE__DATE := parse_dt_to_date(RETRIEVAL_DATE)]
    dat <- dat[!is.na(RETRIEVAL_DATE__DATE) & RETRIEVAL_DATE__DATE >= dmin]
    if (!is.null(dmax)) dat <- dat[RETRIEVAL_DATE__DATE <= dmax]
    dat[, RETRIEVAL_DATE__DATE := NULL]
  } else {
    warning("RETRIEVAL_DATE not found; skipping date filtering.")
  }

  # ---- gear filter ----
  dat[, GEAR := recode_gear(GEAR_TYPE)]
  if (!is.null(gear)) {
    dat <- dat[GEAR %in% gear]
  }

  # ---- length-frequency summary ----
  dat[, LENGTH := suppressWarnings(as.numeric(LENGTH))]
  dat[, FREQUENCY := suppressWarnings(as.numeric(FREQUENCY))]

  lf <- dat[
    is.finite(LENGTH) & is.finite(FREQUENCY),
    .(FREQUENCY = sum(FREQUENCY, na.rm = TRUE)),
    by = .(SPECIES, NMFS_AREA, GEAR, LENGTH)
  ][order(SPECIES, NMFS_AREA, GEAR, LENGTH)]

  lf <- as.data.frame(lf)

  if (isTRUE(return_sql)) {
    return(list(
      raw = data.frame(dat),
      lf  = lf,
      sql = list(sql = sql_len, first_year_used = first_year)
    ))
  }

  list(raw = dat, lf = lf)
}
