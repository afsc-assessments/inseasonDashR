#' Retrieve code table rows for one or more species
#'
#' Reads a SQL template file (GET_CODES.sql), injects one or more agency species
#' codes, and runs the query against a provided database connection using the
#' `sql_filter()` and `sql_run()` utilities.
#'
#' @param con A DBI connection object (e.g., your AFSC connection).
#' @param spec Integer or numeric vector of agency species codes to filter on.
#' @param sql_dir Character path to the directory containing GET_CODES.sql.
#'   Default is "sql".
#'
#' @return A data.frame containing the code table rows returned by the query.
#'
#' @examples
#' \dontrun{
#' afsc <- db_connect("afsc")
#' codes <- get_codes(afsc, spec = 202)
#' codes2 <- get_codes(afsc, spec = c(202, 203))
#' }
#'
#' @export
get_codes <- function(con, spec, sql_dir = "sql") {

  stopifnot(!missing(con))
  if (missing(spec) || length(spec) < 1) stop("`spec` must be provided.")
  if (!is.numeric(spec)) stop("`spec` must be numeric/integer (agency species code).")

  sql_file <- file.path(sql_dir, "GET_CODES.sql")
  if (!file.exists(sql_file)) {
    stop("SQL file not found: ", sql_file)
  }

  sql_code <- readLines(sql_file)

  # Inject species codes
  sql_code <- sql_filter(
    sql_precode = "IN",
    x = spec,
    sql_code = sql_code,
    flag = "-- insert species"
  )

  code <- sql_run(con, sql_code)

  # Standardize column names (consistent with your other pulls)
  code <- dplyr::rename_all(code, toupper)

  code
}

  