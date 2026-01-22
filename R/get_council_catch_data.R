#' Retrieve catch data by species and minimum year
#'
#' Pulls catch data from the AFSC database using GET_ALL_CATCH.sql,
#' filtering by agency species code and year >= input year.
#'
#' @param afsc DBI connection object afsc
#' @param akfin DBI connection object akfin
#' @param species Integer observer program species code
#' @param year_min Integer minimum year to include (inclusive)
#' @param sql_dir Directory containing SQL files (default "sql")
#'
#' @return A data.frame with catch data
#' @export
get_council_catch_data <- function(
  afsc,akfin,
  species,
  year_min
) {

  stopifnot(
    !missing(afsc),
    !missing(akfin),
    is.numeric(species), length(species) == 1,
    is.numeric(year_min), length(year_min) == 1
  )

  # ---- convert species code
  sql_file <- system.file("sql", "GET_CODES.sql", package = "inseasonDashR")
  sql_code <- readLines(sql_file)

  # ---- inject species ----
  sql_code <- sql_filter(
    sql_precode = "IN",
    x = species,
    sql_code = sql_code,
    flag = "-- insert species"
  )

  code <- sql_run(afsc, sql_code)
  species=code$AKR_PROGRAM_CODE

  # ---- read SQL template ----
  sql_file <- system.file("sql", "GET_ALL_CATCH.sql", package = "inseasonDashR")
  sql_code <- readLines(sql_file)

  # ---- inject species ----
  sql_code <- sql_filter(
    sql_precode = "IN",
    x = species,
    sql_code = sql_code,
    flag = "-- insert species"
  )

  # ---- inject year filter ----
  sql_code <- sql_filter(
    sql_precode = ">=",
    x = year_min,
    sql_code = sql_code,
    flag = "-- insert year"
  )

  # ---- run query ----
  out <- sql_run(akfin, sql_code)

  # ---- standard cleanup ----
  out <- dplyr::rename_all(out, toupper)

  out
}
