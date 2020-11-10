#' Bypass KE HIVST Distribution channels
#'
#' Bypass KE Distribution channels, a category combination filter.
#' @param baseurl A string, the base URL of a PSI - MIS. Default is PSI - MIS clone server.
#' @importFrom rlang .data
#' @importFrom cli cli_h1 cli_alert_success cli_alert_danger cli_alert_info
#' @importFrom crayon %+%
#' @export
bypass_ke_distr <- function(baseurl){

  cli_h1("Bypass KE distribution channels")

  cli_alert_success("Pulling kits distribution data")

  # pull kit distribution data
  kits_distr <- purrr::map(kits_endpoint(baseurl),api_get)

  # default kits
  default_kits_distr <- purrr::map(kits_endpoint(baseurl, dimension_pe = "LAST_12_MONTHS"),api_get)

  # parse kits
  kits_distr_kits <- purrr::map(kits_distr, function(x){

    if (!is.null(x$response)){
      # parse the response
      data_values <- httr::content(x$response, "text")

      x$response <- jsonlite::fromJSON(data_values)

      # if (length(x$response) == 0){
      #   stop(
      #     paste("Function", sQuote("api_get"), "did not return a data value set"),
      #     call. = FALSE
      #   )
      # }

      x

    }
  })

  defaults_kits_distr_kits <- purrr::map(default_kits_distr, function(x){

    if (!is.null(x$response)){
      # parse the response
      data_values <- httr::content(x$response, "text")

      x$response <- jsonlite::fromJSON(data_values)

      if (length(x$response) == 0){
        cli_alert_danger(
          paste("Function", main(sQuote("api_get")), "did not return a data value set for the fall back period", main(sQuote("LAST_12_MONTH")), ", please try again later")
        )

        cli_alert_info("Exitting .... ")
        stop("Terminating the script", call. = FALSE)
      }

      x

    }
  })



  if (length(kits_distr_kits$hotspots$response) == 0) {
    # updating to default kits for the last 12 months
    cli_alert_success("Updating hotspots kits to the default period" %+% note(" LAST_12_MONTHS"))
    kits_distr_kits$hotspots <- defaults_kits_distr_kits$hotspots

  }

  if (length(kits_distr_kits$workplace$response) == 0){
    # updating to default workplace kits for the last 12 months
    cli_alert_success("Updating workplace kits to the default period" %+% note(" LAST_12_MONTHS"))
    kits_distr_kits$workplace <- defaults_kits_distr_kits$workplace
  }


  if (length(kits_distr_kits$pharmacies$response) == 0){
    # updating to default pahramcies kits for the last 12 months
    cli_alert_success("Updating pharmacy kits to the default period" %+% note(" LAST_12_MONTHS"))
    kits_distr_kits$pharmacies <- defaults_kits_distr_kits$pharmacies
  }


  # transform kits
  cli_alert_success("Bypassing KE distribution channels")

  hotspots <- kits_data_elements("hotspots")

  pharmacies <- kits_data_elements("pharmacies")

  workplace <- kits_data_elements("workplace")

  main <- kits_data_elements("main")

  kits_distr_kits_hotspots <- dplyr::mutate(kits_distr_kits$hotspots$response$dataValues,
                                            dataElement = ifelse(.data$dataElement == main$insti,
                                                                 hotspots$insti,
                                                                 hotspots$oraq))

  kits_distr_kits_pharmacies <- dplyr::mutate(kits_distr_kits$pharmacies$response$dataValues,
                                              dataElement = ifelse(.data$dataElement == main$insti,
                                                                   pharmacies$insti,
                                                                   pharmacies$oraq))

  kits_distr_kits_workplace <- dplyr::mutate(kits_distr_kits$workplace$response$dataValues,
                                             dataElement = ifelse(.data$dataElement == main$insti,
                                                                  workplace$insti,
                                                                  workplace$oraq))
  transformed_kits <- list(
    hotspots = kits_distr_kits_hotspots,
    pharmacies = kits_distr_kits_pharmacies,
    workplace = kits_distr_kits_workplace
  )

  transformed_kits <- purrr::map(transformed_kits, function(x){
    dplyr::mutate(x, storedBy = "[srp]")
  })

  #transformed_kits


  # upload / update transformed kits

  upload_endpoint <- paste0(baseurl, api_resource(resource = "dataValueSets", push = T))

  cli_alert_success("Updating kits distribution data")

  kits_distr_d <- purrr::map(transformed_kits, ~api_update_data(upload_endpoint, data_values = .x))

  # Did any API update fail?

  cli_alert_success("checking if there are any fails")

  any_fails <- purrr::map_lgl(kits_distr_d, function(x){
    httr::http_error(x$response)
  })

  if (all(any_fails)){
    cli_alert_danger("Failed to update one or all the kits distribution data")
    stop(
      sprintf(
        "PSI - MIS API Upload request failed \n<%s>",
        "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"
      ),
      call. = FALSE
    )
  } else{
    cli_alert_success("Done" %+% note(" Update completed successively!"))
    #TRUE
  }


}

#' Kits endpoints
#'
#' @param baseurl A string, the base URL of a PSI - MIS. Default is PSI - MIS clone server.
#' @param ... Additional prams passed to the `api_endpoint`.
#' @return An S3 object.
#' @export
kits_endpoint <- function(baseurl, ...){

  structure(
    list(
      hotspots = api_endpoint(baseurl, ...),
      workplace = api_endpoint(baseurl, ...,  category_option = "dXBNFfxQ6O8"),
      pharmacies = api_endpoint(baseurl, ..., category_option = "oHwb9OjSdkr")
    ),
    class = "kit_endpoints"
  )


}

#' Kits data elements
#'
#' Mapping of new kits data elements.
#'
#' @param type A string.
#' @return A list.
#' @export
kits_data_elements <- function(type){
  switch (type,
    hotspots = list(insti = "aKR4wgYNYpb",
                    oraq = "BqezUphpPgV"),
    pharmacies = list(insti = "y95zrS5bSiz",
                      oraq = "pKtc41yNqKu"),
    workplace = list(insti = "rhvlc7qPUKR",
                     oraq = "fLbkGrbVImO"),
    main = list(insti = "dQTWxMDtAiW",
                oraq = "mOGarPwHuFc")
  )
}

