#' Pull Observer + EM catch data for a species and optional date range
#'
#' Uses SQL templates with `-- insert species` and `-- insert year` placeholders.
#' If `date_min` is provided, the year from `date_min` is used for the SQL `>= year`
#' filter (first_year). Date filtering is also applied post-pull (inclusive).
#'
#' @param afsc DBI connection to AFSC database.
#' @param akfin DBI connection to AKFIN database.
#' @param fsh_sp_label Numeric/integer species code(s) for the SQL IN clause.
#' @param date_min Optional start date (inclusive) as "mm/dd/yyyy".
#' @param date_max Optional end date (inclusive) as "mm/dd/yyyy".
#' @param tz Timezone used when parsing datetime strings (default "UTC").
#' @param return_sql If TRUE, return the final SQL text used (default TRUE).
#'
#' @return list(data_o=..., data_em=..., sql=list(...) if return_sql=TRUE)
#' @export
get_catch_data_date <- function(
  afsc,
  akfin,
  species = 202,
  date_min = NULL,
  date_max = NULL,
  tz = "UTC",
  return_sql = FALSE
) {

  # ---- helpers ----
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

  parse_dt_to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
    y <- suppressWarnings(as.POSIXct(x, tz = tz))
    if (!all(is.na(y))) return(as.Date(y))
    suppressWarnings(as.Date(x))
  }

  # ---- parse date inputs ----
  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)

  # ---- infer first_year from date_min 
    first_year <- as.integer(format(dmin, "%Y"))
  
  # ---- read SQL templates ----

  sql_file <- system.file("sql", "GET_CURRENT.sql", package = "inseasonDashboard")
  Ocatch  <- readLines(sql_file)
  
  sql_file <- system.file("sql", "GET_EM_CATCH.sql", package = "inseasonDashboard")
  EMcatch <- readLines(sql_file)

  # ---- inject species ----
  Ocatch  <- sql_filter(sql_precode = "IN", x = species, sql_code = Ocatch,  flag = "-- insert species")
  EMcatch <- sql_filter(sql_precode = "IN", x = species, sql_code = EMcatch, flag = "-- insert species")

  # ---- inject year (only if we have one) ----
  if (!is.null(first_year)) {
    Ocatch  <- sql_filter_num(sql_precode = ">=", x = first_year, sql_code = Ocatch,  flag = "-- insert year")
    EMcatch <- sql_filter_num(sql_precode = ">=", x = first_year, sql_code = EMcatch, flag = "-- insert year")
  }

  # ---- run queries ----
  data_o <- sql_run(afsc, Ocatch) |>
    dplyr::rename_all(toupper) |>
    data.table::data.table()

  data_em <- sql_run(akfin, EMcatch) |>
    dplyr::rename_all(toupper) |>
    data.table::data.table()

  # ---- post-pull date filtering (inclusive) ----
  if (!is.null(dmin) || !is.null(dmax)) {

    # observer
    if ("RETRIEVAL_DATE" %in% names(data_o)) {
      data_o[, RETRIEVAL_DATE__DATE := parse_dt_to_date(RETRIEVAL_DATE)]
      if (!is.null(dmin)) data_o <- data_o[!is.na(RETRIEVAL_DATE__DATE) & RETRIEVAL_DATE__DATE >= dmin]
      if (!is.null(dmax)) data_o <- data_o[!is.na(RETRIEVAL_DATE__DATE) & RETRIEVAL_DATE__DATE <= dmax]
      data_o[, RETRIEVAL_DATE__DATE := NULL]
    } else {
      warning("data_o does not contain RETRIEVAL_DATE; skipping date filtering for observer pull.")
    }

    # EM
    if ("RETRIEVAL_END_DATE" %in% names(data_em)) {
      data_em[, RETRIEVAL_END_DATE__DATE := parse_dt_to_date(RETRIEVAL_END_DATE)]
      if (!is.null(dmin)) data_em <- data_em[!is.na(RETRIEVAL_END_DATE__DATE) & RETRIEVAL_END_DATE__DATE >= dmin]
      if (!is.null(dmax)) data_em <- data_em[!is.na(RETRIEVAL_END_DATE__DATE) & RETRIEVAL_END_DATE__DATE <= dmax]
      data_em[, RETRIEVAL_END_DATE__DATE := NULL]
    } else {
      warning("data_em does not contain RETRIEVAL_END_DATE; skipping date filtering for EM pull.")
    }
  }

  if (isTRUE(return_sql)) {
    return(list(
      data_o = data_o,
      data_em = data_em,
      sql = list(Ocatch = Ocatch, EMcatch = EMcatch),
      first_year_used = first_year
    ))
  } else {
    return(list(data_o = data_o, data_em = data_em, first_year_used = first_year))
  }
}
