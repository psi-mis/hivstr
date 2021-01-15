baseurl <- "https://clone.psi-mis.org/"
hivstr::api_basic_auth(username = "ke_tunza", password = "#")

# Pull Kits data
kits_distr <- purrr::map(kits_endpoint(baseurl),api_get)

# Test 1 ------------------------------------------------------------------

testthat::test_that("Kits distributed are well pulled",{
  purrr::walk(kits_distr, function(x){
    if (!is.null(x$response)){
      testthat::expect_true(identical(x$response$status_code, 200L))
    }
  })
})

# Pull default kits data
default_kits_distr <- purrr::map(kits_endpoint(baseurl, dimension_pe = "LAST_12_MONTHS"),api_get)


# Test 2 ------------------------------------------------------------------


testthat::test_that("Default kits are well pulled", {

  purrr::walk(default_kits_distr, function(x){
    if (!is.null(x$response)){
      testthat::expect_true(identical(x$response$status_code, 200L))
    }
  })
})

## ------ Data Transformation ------------------------


# Parse normal kits
kits_distr_kits <- purrr::map(kits_distr, function(x){

  if (!is.null(x$response)){
    # parse the response
    data_values <- httr::content(x$response, "text")

    x$response <- jsonlite::fromJSON(data_values)
    x

  }
})

# Parse the default kits
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

# update missing data with the default fallback data of Last 12 Months
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
  # updating to default pharmacies kits for the last 12 months
  cli_alert_success("Updating pharmacy kits to the default period" %+% note(" LAST_12_MONTHS"))
  kits_distr_kits$pharmacies <- defaults_kits_distr_kits$pharmacies
}

# Transform Kits
hotspots <- kits_data_elements("hotspots")

pharmacies <- kits_data_elements("pharmacies")

workplace <- kits_data_elements("workplace")

main <- kits_data_elements("main")

# ==> Hotspots kits
kits_distr_kits_hotspots <- dplyr::mutate(kits_distr_kits$hotspots$response$dataValues,
                                          dataElement = ifelse(.data$dataElement == main$insti,
                                                               hotspots$insti,
                                                               ifelse(.data$dataElement == main$sure_check,
                                                                      hotspots$sure_check,
                                                                      hotspots$oraq
                                                               )
                                          ))
# ===> Pharmacy
kits_distr_kits_pharmacies <- dplyr::mutate(kits_distr_kits$pharmacies$response$dataValues,
                                            dataElement = ifelse(.data$dataElement == main$insti,
                                                                 pharmacies$insti,
                                                                 ifelse(.data$dataElement == main$sure_check,
                                                                        pharmacies$sure_check,
                                                                        pharmacies$oraq
                                                                 )
                                            ))
# ===> Workplace
kits_distr_kits_workplace <- dplyr::mutate(kits_distr_kits$workplace$response$dataValues,
                                           dataElement = ifelse(.data$dataElement == main$insti,
                                                                workplace$insti,
                                                                ifelse(.data$dataElement == main$sure_check,
                                                                       workplace$sure_check,
                                                                       workplace$oraq
                                                                )
                                           ))



# Test 3 ------------------------------------------------------------------


testthat::test_that("Kits distributed are well transformed", {

  testthat::expect_true(all(kits_distr_kits_hotspots$dataElement %in% unlist(hotspots)))
  testthat::expect_true(all(kits_distr_kits_pharmacies$dataElement %in% unlist(pharmacies)))
  testthat::expect_true(all(kits_distr_kits_workplace$dataElement %in% unlist(workplace)))

})

## Compile
transformed_kits <- list(
  hotspots = kits_distr_kits_hotspots,
  pharmacies = kits_distr_kits_pharmacies,
  workplace = kits_distr_kits_workplace
)

## Document the compiled transformation
transformed_kits <- purrr::map(transformed_kits, function(x){
  dplyr::mutate(x, storedBy = "[srp]")
})



# Data Upload / update ----------------------------------------------------


# upload / update transformed kits
# prepare upload endpoint
upload_endpoint <- paste0(baseurl, api_resource(resource = "dataValueSets", push = T))

# update
kits_distr_d <- purrr::map(transformed_kits, ~api_update_data(upload_endpoint, data_values = .x))


# Test 4  -----------------------------------------------------------------

testthat::test_that("Data is re-uploaded well", {
  purrr::walk(kits_distr_d, function(x){
    # Parse
    r <- httr::content(x$response, "text")
    d <- jsonlite::fromJSON(r)
    testthat::expect_true(identical(d$status, "SUCCESS"))
  })
})

