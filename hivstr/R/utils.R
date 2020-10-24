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




colon <- function(...){
  paste0(..., collapse = ":")
}

semi_colon <- function(...){
  paste0(..., collapse = ";")
}


