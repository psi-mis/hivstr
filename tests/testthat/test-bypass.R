# kits_distr_test <- function(baseurl){
#   invisible(api_basic_auth(baseurl, username = "ke_hiv", password = "Temp@123"))
#   kits_distr <- purrr::map(kits_endpoint(baseurl),api_get)
#
#   # default kits
#   default_kits_distr <- purrr::map(kits_endpoint(baseurl, dimension_pe = "LAST_12_MONTHS"),api_get)
#
#   # parse kits
#   kits_distr_kits <- purrr::map(kits_distr, function(x){
#
#     if (!is.null(x$response)){
#       # parse the response
#       data_values <- httr::content(x$response, "text")
#
#       x$response <- jsonlite::fromJSON(data_values)
#
#       # if (length(x$response) == 0){
#       #   stop(
#       #     paste("Function", sQuote("api_get"), "did not return a data value set for the period", sQuote("THIS_MONTH")),
#       #     call. = FALSE
#       #   )
#       # }
#
#       x
#
#     }
#   })
#
#   defaults_kits_distr_kits <- purrr::map(default_kits_distr, function(x){
#
#     if (!is.null(x$response)){
#       # parse the response
#       data_values <- httr::content(x$response, "text")
#
#       x$response <- jsonlite::fromJSON(data_values)
#
#       if (length(x$response) == 0){
#         message(
#           paste("Function", sQuote("api_get"), "did not return a data value set for the fall back period", sQuote("LAST_12_MONTH"), ", please try again later")
#         )
#
#         message("Exitting .... ")
#         exit()
#       }
#
#       x
#
#     }
#   })
#
#
#
#   if (length(kits_distr_kits$hotspots$response) == 0) {
#     # updating to default kits for the last 12 months
#     kits_distr_kits$hotspots <- defaults_kits_distr_kits$hotspots
#
#   }
#
#   if (length(kits_distr_kits$workplace$response) == 0){
#     # updating to default workplace kits for the last 12 months
#     kits_distr_kits$workplace <- defaults_kits_distr_kits$workplace
#   }
#
#
#   if (length(kits_distr_kits$pharmacies$response) == 0){
#     # updating to default pahramcies kits for the last 12 months
#     kits_distr_kits$pharmacies <- defaults_kits_distr_kits$pharmacies
#   }
#
#   kits_distr_kits
# }
#
# test_that("An error is displayed with the period & distribution type for kits without data ", {
#  expect_error(kits_distr_error, "API")
# })
