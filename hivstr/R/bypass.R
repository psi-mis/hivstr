#' Bypass KE HIVST Distribution channels
#'
#' Bypass KE Distribution channels, a category combination filter.
#' @param baseurl A string, the base URL of a PSI - MIS. Default is PSI - MIS clone server.
#' @export
bypass_ke_distr <- function(baseurl){

  # pull kit distribution data
  kits_distr <- purrr::map(kits_endpoint(baseurl),api_get)

  # parse kits
  kits_distr_kits <- purrr::map(kits_distr, function(x){

    if (!is.null(x$response)){
      # parse the response
      data_values <- httr::content(x$response, "text")

      x$response <- jsonlite::fromJSON(data_values)

      if (length(x$response) == 0){
        stop(
          paste("Function", sQuote("api_get"), "did not return a data value set"),
          call. = FALSE
        )
      }

      x

    }
  })

  # transform kits

  hotspots <- kits_data_elements("hotspots")

  pharmacies <- kits_data_elements("pharmacies")

  workplace <- kits_data_elements("workplace")

  main <- kits_data_elements("main")

  kits_distr_kits_hotspots <- dplyr::mutate(kits_distr_kits$hotspots$response$dataValues,
                                            dataElement = ifelse(dataElement == main$insti,
                                                                 hotspots$insti,
                                                                 hotspots$oraq))

  kits_distr_kits_pharmacies <- dplyr::mutate(kits_distr_kits$pharmacies$response$dataValues,
                                              dataElement = ifelse(dataElement == main$insti,
                                                                   pharmacies$insti,
                                                                   pharmacies$oraq))

  kits_distr_kits_workplace <- dplyr::mutate(kits_distr_kits$workplace$response$dataValues,
                                             dataElement = ifelse(dataElement == main$insti,
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

  kits_distr_d <- purrr::map(transformed_kits, ~api_update_data(upload_endpoint, data_values = .x))

  # Did any API update fail?

  any_fails <- purrr::map_lgl(kits_distr_d, function(x){
    httr::http_error(x$response)
  })

  if (all(any_fails)){
    stop(
      sprintf(
        "PSI - MIS API Upload request failed \n<%s>",
        "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"
      ),
      call. = FALSE
    )
  } else{
    TRUE
  }


}

#' Kits endpoints
#'
#' @param baseurl A string, the base URL of a PSI - MIS. Default is PSI - MIS clone server.
#' @export
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

#' Kits data elements
#'
#' Mapping of new kits data elements.
#'
#' @param type A string.
#' @return A list
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

