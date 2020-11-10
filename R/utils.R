#' Set user agent
#'
#' Set a user agent.
#'
#' @param agent A string.
#' @importFrom httr user_agent
#' @noRd
set_agent <- function(agent = NULL){
  if (is.null(agent)){
    agent <- "https://github.com/psi-mis/hivstr"
    httr::user_agent(agent)
  }

  if (!is.null(agent)){
    httr::user_agent(agent)
  }

}

#' Check for internet access
#'
#' Check if internet access exists.
#'
#'  @importFrom curl has_internet
#' @noRd
check_internet <- function(){
  if (!curl::has_internet()){
    stop(
      "Please check your internet connection"
    )
  }
}




#' Colon
#'
#' Collapse everything in vector of characters by a full colon.
#'
#' @param ... A character vector.
#'
#' @return A string separated by full colon.
#'
#' @noRd
colon <- function(...){
  paste0(..., collapse = ":")
}

#' Semi colon
#'
#' Collapse everything in vector of characters by a semi colon.
#' @inheritParams colon
#'
#' @return A string separated by full colon.
#' @noRd
semi_colon <- function(...){
  paste0(..., collapse = ";")
}

#' Print Note
#'
#' @importFrom crayon cyan %+%
#' @noRd
note <- function(...){
  cyan(...)
  #bold$cyan$inverse("NOTE ") %+% " : " %+%  bold(...)
}



#' Print main
#'
#' @importFrom crayon green inverse
#' @noRd
main <- function(...){
  green$inverse(...)
}


