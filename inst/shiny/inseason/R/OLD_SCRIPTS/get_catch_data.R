#' Pull Observer + EM catch data for a species and start year
#' @export
get_catch_data <- function(afsc,akfin, fsh_sp_label = 202, first_year = 2007) {

  # read SQL templates (using plain readLines like your script)
  Ocatch  <- readLines("sql/GET_CURRENT.sql")
  EMcatch <- readLines("sql/GET_EM_CATCH.sql")

  # species filter (keep your original sql_filter)
  Ocatch  <- sql_filter(sql_precode = "IN", x = fsh_sp_label, sql_code = Ocatch,  flag = "-- insert species")
  EMcatch <- sql_filter(sql_precode = "IN", x = fsh_sp_label, sql_code = EMcatch, flag = "-- insert species")

  # year filter (use numeric-safe version)
  Ocatch  <- sql_filter_num(sql_precode = ">=", x = first_year, sql_code = Ocatch,  flag = "-- insert year")
  EMcatch <- sql_filter_num(sql_precode = ">=", x = first_year, sql_code = EMcatch, flag = "-- insert year")

  # run
  data_o <- sql_run(afsc, Ocatch) |>
    dplyr::rename_all(toupper) |>
    data.table::as.data.table()

  data_em <- sql_run(akfin, EMcatch) |>
    dplyr::rename_all(toupper) |>
    data.table::as.data.table()

  list(data_o = data_o, data_em = data_em, sql = list(Ocatch = Ocatch, EMcatch = EMcatch))
}
