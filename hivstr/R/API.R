#' API version
#'
#' Set an API version
#'
#' @param version Numeric. The current supported Web API version.
#'
#' @return The supported API version.
api_version <- function(version = NULL){
  if (is.null(version)){
    32
  }
}


#' API resource
#'
#' Point an API resource.
#'
#' \code{api_resource} sets an API resource.
#'
#' @param resource A string, A PSI - MIS resource.
#' @param push Logical. Default is FALSE, which returns the analytics resource.
#'
#' @return A string, an API resource
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
    stop("API resource must be specified!", call. = F)
  }
}



#' Generate an API Endpoint
#'
#' Create an API endpoint to query kits from the analytics.
#'
#' \code{api_endpoint} constructs an API endpoint to query HIV
#' self testing kits from the analytics resource. The default endpoint will
#' query insti and oraq kits distributed via hotpots.
#'
#' @param baseurl A string, the base URL of a PSI - MIS. Default is PSI - MIS clone server.
#' @param dimension_dx A string or character vector, data elements or indicators
#'   uid. The defaults are Oraq (id: mOGarPwHuFc) and Insti (id: dQTWxMDtAiW)
#'   kits bought data elements uids.
#' @param category A string, category UID. Default; KE HIVST - Distribution
#'   channels (id: n5ODfcdD1YQ)
#' @param category_option A string, the distribution model. Default is hotspot
#'   (id: YF8v2OSxWKl)
#' @param dimension_ou A string or character vector, the orgUnit UID. Default is
#'   Kenya (id: rP1W74RpNWF).
#' @param dimension_pe A string or character vector, DHIS2 period. The default
#'   is THIS_MONTH
#' @param completed_only Logical, whether to pull only the completed events. The
#'   default is false.
#'
#' @return A string, An API endpoint.
api_endpoint <- function(baseurl = "https://clone.psi-mis.org/", dimension_dx = c("dQTWxMDtAiW","mOGarPwHuFc"), category = "n5ODfcdD1YQ",
                          category_option = "YF8v2OSxWKl", dimension_ou = "rP1W74RpNWF", dimension_pe = "THIS_MONTH",
                          completed_only = FALSE){

  resource <- api_resource("dataValueSet")

  query <- paste0("?",
    paste(
      paste0("dimension=dx:", semi_colon(dimension_dx)),
      paste0("dimension=", colon(c(category, category_option))),
      paste0("dimension=ou:", semi_colon(dimension_ou)),
      paste0("dimension=pe:", semi_colon(dimension_pe)),
      paste0("completedOnly=", completed_only),
          sep = "&"
          )
  )

  endpoint <- paste0(baseurl, resource, query)

  endpoint


}













kits_endpoint <- function(baseurl){

  structure(
    list(
      hotspots = api_endpoint(baseurl),
      workplace = api_endpoint(baseurl, category_option = "dXBNFfxQ6O8"),
      pharmacies = api_endpoint(baseurl, category_option = "oHwb9OjSdkr")
    ),
    class = "kit_endpoints"
  )


}



