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
                          completed_only = "false"){

  resource <- api_resource("dataValueSet.json")

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

#' Check an API response
#'
#' Did an API request return error?
#'
#' @param resp An API response to check.
#'
#' @return An error message
#'
#' @importFrom httr http_error status_code
api_status_check <- function(resp = NULL){

  if (is.null(resp)){
    stop(
      "API response must be specified",
      call. = FALSE
    )
  }

  if (http_error(resp)){
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

#' Check an API response is JSON
#'
#' Check that an API request returned a JSON object.
#'
#' @param resp A response
#' @return An error message
#' @importFrom httr http_type
api_json_check <- function(resp = NULL){

  if (!is.null(resp)){

    if (http_type(resp) != "application/json"){
      stop(
        "API did not return JSON",
        call. = FALSE
      )
    }

  }

}





#' Pull a Resource at PSI-MIS
#'
#' @param endpoint A string, the API endpoint
#'
#' @return An S3 object.
#'
#' @importFrom httr GET timeout
#' @importFrom utils URLencode str
#' @export
api_get <- function(endpoint = NULL){

  if (is.null(endpoint)){
    stop(
      "Endpoint must be specified!",
      call. = FALSE
    )
  }

  url <- URLencode(endpoint)

  resp <- GET(url, set_agent(), timeout(60))

  api_json_check(resp)

  api_status_check(resp)

  structure(
    list(
      endpoint = endpoint,
      response = resp
    ),
    class = "api_get"
  )

}

print.api_get <- function(x, ...){
  cat("[PSI - MIS ", x$endpoint, "]\n", sep = "")
  str(x)
  invisible(x)
}


#' Update Data Values at PSI MIS
#'
#' Create or update dataValueSet resource at PSI MIS.
#'
#' @importFrom httr modify_url POST timeout content_type_json
#' @importFrom jsonlite toJSON
#' @importFrom utils URLencode str
#'
#' @param endpoint A string, API endpoint.
#' @param data_values A data.frame, filtered kits.
#' @param import_strategy A string. Default is CREATE_UPDATE.
#'
#' @return An S3 object, reponse
api_update_data <- function(endpoint = NULL, data_values = NULL, import_strategy = "CREATE_AND_UPDATE"){

  if (is.null(endpoint)){
    stop(
      "Endpoint must be specified!",
      call. = FALSE
    )
  }

  if (is.null(data_values)){
    stop(
      "Data values must be specified!",
      call. = FALSE
    )
  }

  url <- modify_url(endpoint, query = list(importStrategy = import_strategy))

  url <- URLencode(url)

  resp <- POST(url, set_agent(), timeout(60),
               body = toJSON(list(dataValues = data_values), auto_unbox = T),
               content_type_json()
               )

  api_json_check(resp)

  api_status_check(resp)

  structure(
    list(
      endpoint = endpoint,
      data_values = data_values,
      response = resp
    ),
    class = "api_update_data"
  )


}

print.api_update_data <- function(x, ...){
  cat("[PSI - MIS ", x$endpoint, "]\n", sep = "")
  str(x)
  invisible(x)
}


#' Basic Auth
#'
#' @importFrom httr GET authenticate status_code
#' @param baseurl A string, the base URL of a PSI - MIS. Default is PSI - MIS clone server.
#' @param username A string. PSI - MIS username.
#' @param password A string. PSI - MIS user password.
#'
#' @return logical
#' @export
api_basic_auth <- function(baseurl = "https://clone.psi-mis.org/", username, password){

  endpoint <- api_resource("me", push = T)

  url <- URLencode(paste0(baseurl, endpoint))

  resp <- GET(url,set_agent(), authenticate(username, password))

  api_json_check(resp)


  if (status_code(resp) == 200L)
    TRUE
  else
    FALSE

}



