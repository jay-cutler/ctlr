#' Shortcut code to authorize Box.com API from API information
#'
#' Pulls API information from .Renviron without recall of specific code from the `boxr` package.
#'
#' @export
authorize_box <- function(){

  boxr::box_auth(client_id = Sys.getenv("BOX_CLIENT_ID"),
         client_secret = Sys.getenv("BOX_CLIENT_SECRET"))

}
