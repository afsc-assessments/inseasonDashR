#' Connect to AFSC and AKFIN databases
#'
#' Uses keyring to securely retrieve credentials.
#'
#' @return A list with AFSC and AKFIN DBI connections
#' @export
db_connect <- function() {

  afsc_user  <- keyring::key_list("afsc")$username
  afsc_pwd   <- keyring::key_get("afsc", afsc_user)

  akfin_user <- keyring::key_list("akfin")$username
  akfin_pwd  <- keyring::key_get("akfin", akfin_user)

  afsc <- DBI::dbConnect(
    odbc::odbc(), "afsc",
    UID = afsc_user, PWD = afsc_pwd
  )

  akfin <- DBI::dbConnect(
    odbc::odbc(), "akfin",
    UID = akfin_user, PWD = akfin_pwd
  )

  list(afsc = afsc, akfin = akfin)
}
