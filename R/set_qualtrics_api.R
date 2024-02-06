#' Choose from loaded MFF Qualtrics APIs
#'
#' Loads specified API information to pull down data from Qualtrics.
#'
#' @param account Name of MFF Qualtrics account to use. Can be one of "snap-ed", "ntae", or "other". Must ensure that API keys are named in lowercase and underscores with these names, and base URLs are named either with "mff" or "ntae" and underscores in the same format (e.g., "ntae_qualtrics_api_key", "mff_qualtrics_base_url").
#'
#' @export

set_qualtrics_api <- function(account){

  if (!account %in% c("snap-ed", "ntae", "other")) {
    stop("'account' argument must be one of: 'snap-ed', 'ntae', 'other'")
  }

  if(account == "snap-ed"){
    qualtRics::qualtrics_api_credentials(api_key = Sys.getenv("snap_ed_qualtrics_api_key"),
                                         base_url = Sys.getenv("mff_qualtrics_base_url"),
                                         install = FALSE)
  }

  if(account == "ntae"){
    qualtRics::qualtrics_api_credentials(api_key = Sys.getenv("ntae_qualtrics_api_key"),
                                         base_url = Sys.getenv("ntae_qualtrics_base_url"),
                                         install = FALSE)
  }

  if(account == "other"){
    qualtRics::qualtrics_api_credentials(api_key = Sys.getenv("other_qualtrics_api_key"),
                                         base_url = Sys.getenv("mff_qualtrics_base_url"),
                                         install = FALSE)
  }

}
