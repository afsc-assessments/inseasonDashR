## ---------------------------------------------------------------------
## Utility functions for database access and SQL templating
##
## This file contains internal helper functions used throughout the
## inseasonDashboard package, including:
##   - SQL template filtering and injection
##   - Database query execution wrappers
##   - Common safety checks for SQL construction
##
## These functions are designed to be lightweight, reusable, and
## compatible with DBI/odbc connections.
## ---------------------------------------------------------------------

#' Execute a SQL Query and Return Results
#'
#' Executes a SQL query against a DBI connection and returns the
#' results as a data.frame.
#'
#' @param con A DBI connection object.
#'
#' @param sql_code Character vector.
#'   SQL query (typically read via \code{readLines()} and processed
#'   with \code{sql_filter()}).
#'
#' @return data.frame containing query results.
#'
#' @details
#' This is a lightweight wrapper around \code{DBI::dbGetQuery()} that
#' ensures SQL code is collapsed into a single string before execution.
#'
#' @importFrom DBI dbGetQuery
#'
#' @keywords internal
sql_run <- function(database, query) {
  query = paste(query, collapse = "\n")
  DBI::dbGetQuery(database, query, as.is=TRUE, believeNRows=FALSE)
}


#' Inject Values into a SQL Template
#'
#' Safely inserts values into a SQL script read as character lines,
#' replacing a flagged placeholder with an appropriate SQL expression
#' (e.g., \code{IN (...)}, \code{>= value}).
#'
#' This function is designed for use with SQL scripts stored as files
#' and read via \code{readLines()}, and is compatible with Oracle SQL.
#'
#' @param sql_precode Character.
#'   SQL operator or prefix (e.g., \code{"IN"}, \code{">="}).
#'
#' @param x Vector.
#'   Values to insert into the SQL template.
#'
#' @param sql_code Character vector.
#'   SQL script read via \code{readLines()}.
#'
#' @param flag Character.
#'   Comment flag in the SQL script indicating where substitution occurs.
#'
#' @return Character vector of modified SQL code.
#'
#' @details
#' The placeholder line containing \code{flag} is replaced with a valid
#' SQL expression constructed from \code{x}. Missing or empty values
#' will result in an error.
#'
#' @keywords internal
sql_filter <- function(sql_precode = "=", x, sql_code, flag = "-- insert species") {
  
  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(x), ")"
  )
  sql_code
}


#' Inject Numeric Values into a SQL Template
#'
#' Replaces a flagged line in a SQL script with a numeric filter expression,
#' typically used for year- or value-based constraints (e.g. \code{>= (2007)}).
#'
#' @param sql_precode Character.
#'   SQL comparison operator or prefix (e.g. \code{">="}, \code{"<="}, \code{"IN"}).
#'
#' @param x Numeric vector.
#'   Values to be inserted into the SQL expression.
#'
#' @param sql_code Character vector.
#'   SQL script read via \code{readLines()}.
#'
#' @param flag Character.
#'   Comment flag in the SQL script indicating where substitution should occur.
#'
#' @return Character vector containing the modified SQL code.
#'
#' @details
#' This function expects exactly one occurrence of \code{flag} in the SQL
#' template. Numeric values are formatted using
#' \code{\link{collapse_filters_num}} and injected directly into the query.
#'
#' @keywords internal
sql_filter_num <- function(sql_precode = ">=", x, sql_code, flag = "-- insert year") {
  i <- suppressWarnings(grep(flag, sql_code))
  if (length(i) != 1) stop("Expected exactly one match for flag: ", flag)
  sql_code[i] <- paste0(sql_precode, " (", collapse_filters_num(x), ")")
  sql_code
}

#' Collapse Values for SQL IN-Clause Filtering
#'
#' Formats a vector of values as a single quoted, comma-separated string
#' suitable for use inside a SQL \code{IN (...)} clause.
#'
#' @param x Vector of values (character or coercible to character).
#'
#' @return Character scalar containing quoted, comma-separated values.
#'
#' @details
#' This function is intended for internal SQL templating utilities and
#' assumes values are safe for direct insertion into SQL text.
#' Missing values are not filtered and should be handled upstream.
#'
#' @examples
#' collapse_filters(c("A", "B", "C"))
#' # returns "'A','B','C'"
#'
#' @keywords internal
collapse_filters <- function(x) {
  sprintf("'%s'", paste(x, collapse = "','"))
}

#' Collapse Numeric Values for SQL Filtering
#'
#' Formats a numeric vector as a comma-separated string suitable for
#' insertion into SQL numeric filter expressions (e.g. \code{IN (1,2,3)}).
#'
#' @param x Numeric vector.
#'
#' @return Character scalar containing comma-separated numeric values.
#'
#' @details
#' This function performs no quoting or validation and assumes values
#' are safe for direct insertion into SQL text. Input validation should
#' be handled by the calling function.
#'
#' @examples
#' collapse_filters_num(c(2005, 2006, 2007))
#' # returns "2005,2006,2007"
#'
#' @keywords internal
collapse_filters_num<-function(x) {
  paste(x, collapse = ",")
}


#' function to format numbers to text with 1,000s comma
#'
#' @param number value to format
#'
#'
comma <- function( number ) {
  format(round(as.numeric( number ), digits = 0), big.mark = ",")
}





