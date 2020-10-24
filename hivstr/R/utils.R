#' @importFrom httr user_agent
#' @noRd
set_agent <- function(agent = NULL){
  if (is.null(agent)){
    agent <- "https://github.com/psi-mis/HIVST"
    httr::user_agent(agent)
  }

  if (!is.null(agent)){
    httr::user_agent(agent)
  }

}

#' @importFrom curl has_internet
#' @noRd
check_internet <- function(){
  if (!curl::has_internet()){
    stop(
      "Please check your internet connection"
    )
  }
}

#' Check the status of API request
#'  @importFrom httr http_error status_code
#'  @param resp API response.
check_status <- function(resp){
  if (httr::http_error(resp)){
    stop(
      sprintf(
        "PSI - MIS API request failed [%s]\n<%s>",
        status_code(resp),
        "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"
      ),
      call. = FALSE
    )
  }
}

#' Set the web api version
#' @noRd
api_version <- function(version = NULL){
  if (is.null(version)){
    32
  }
}


#' Set endpoint
#' @noRd
api_resource <- function(resource = NULL, push = FALSE){

  if (!is.null(resource) && push == FALSE){
    paste0(
      paste("api", api_version(), "analytics", resource, sep = "/")
    )
  } else if(!is.null(resource) && push == TRUE){
    paste0(
      paste("api", api_version(), resource, sep = "/")
    )
  } else{
    stop("API Resource must be specified!", call. = F)
  }
}

colon <- function(...){
  paste0(..., collapse = ":")
}

semi_colon <- function(...){
  paste0(..., collapse = ";")
}


