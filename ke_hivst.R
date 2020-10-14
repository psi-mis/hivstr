# Code to bypass use of KE Distribution channels, a category combination filter, directly from the analytic views. 
# Making it easier to combine/visualize HIV Self Testing data with other sources on single chart/table.
# Source: KE HIV CIFF - HIV Self Testing
# Project: KE HIV Self Testing 
# Author: Isaiah Nyabuto, Contact details: inyabuto@psi.org


# Load required packages ------------------------------------------------------------------

library(httr)
library(curl)
library(jsonlite)
library(keyringr)
library(magrittr)
library(dplyr)
library(purrr)




# Set up baseurl & login ------------------------------------------------------------------

baseurl <- "https://data.psi-mis.org/"
base <- substr(baseurl, 9, 24)


#' @importFrom httr GET authenticate
#' log in 
login_dhis2 <- function(baseurl, usr, pwd){
  GET(paste0(baseurl, "api/me"), authenticate(usr, pwd)) -> r
  assertthat::assert_that(r$status_code == 200L)
}

login_dhis2(baseurl,
            usr = keyringr::get_kc_account(base, type = "internet"),
            pwd = keyringr::decrypt_kc_pw(base, type = "internet"))

# check internet
check_internet <- function(){
  if (!curl::has_internet()){
    stop(
      "Please check your internet connection"
    )
  }
}

# check status of a response
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



# Pull kits distribution data from the main source ------------------------------------------------------------

endpoints <- list(hotspots = "api/32/analytics/dataValueSet.json?dimension=dx%3AdQTWxMDtAiW%3BmOGarPwHuFc&dimension=n5ODfcdD1YQ%3AYF8v2OSxWKl&dimension=ou%3ArP1W74RpNWF&dimension=pe%3A202004%3B202005%3B202006%3B202007%3B202008%3B202009%3B202010%3B202011%3B202012%3B202101%3B202102%3B202103&completedOnly=false",
                  pharmacies = "api/32/analytics/dataValueSet.json?dimension=dx%3AdQTWxMDtAiW%3BmOGarPwHuFc&dimension=n5ODfcdD1YQ%3AoHwb9OjSdkr&dimension=ou%3ArP1W74RpNWF&dimension=pe%3A202004%3B202005%3B202006%3B202007%3B202008%3B202009%3B202010%3B202011%3B202012%3B202101%3B202102%3B202103&completedOnly=false",
                  workplace = "api/32/analytics/dataValueSet.json?dimension=dx%3AdQTWxMDtAiW%3BmOGarPwHuFc&dimension=n5ODfcdD1YQ%3AdXBNFfxQ6O8&dimension=ou%3ArP1W74RpNWF&dimension=pe%3A202004%3B202005%3B202006%3B202007%3B202008%3B202009%3B202010%3B202011%3B202012%3B202101%3B202102%3B202103&completedOnly=false")




check_internet()


kits_distribution <- purrr::map(endpoints, 
                                function(x){
                                  httr::GET(paste0(baseurl, x)) %>%
                                    httr::content(., "text") %>%
                                    jsonlite::fromJSON(.)
                                })




# Transform  --------------------------------------------------------------

# set uids of kits distributed via hotspots
kits_distr_hotspots <- list(insti = "aKR4wgYNYpb", 
                            oraq = "BqezUphpPgV")

# set uids of kits distributed via pharmacies
kits_distr_pharmacies <- list(insti = "y95zrS5bSiz", 
                              oraq = "pKtc41yNqKu")

# set uids of kits distributed via workplace
kits_distr_workplace <- list(insti = "rhvlc7qPUKR", 
                             oraq = "fLbkGrbVImO")

# Kits uids from the main source
kits <- list(insti = "dQTWxMDtAiW", 
             oraq = "mOGarPwHuFc")


# Remap kits distributed via hotspots
kits_distr_hotspots_tr <- 
kits_distribution$hotspots$dataValues %>%
  dplyr::mutate(., dataElement = ifelse(dataElement == kits$insti,
                                   kits_distr_hotspots$insti, 
                                   kits_distr_hotspots$oraq)) %>%
  dplyr::mutate(., storedBy = "[srp]")


# Remap kits distributed via pharmacies
kits_distr_pharmacies_tr <- 
kits_distribution$pharmacies$dataValues %>%
  dplyr::mutate(., dataElement = ifelse(dataElement == kits$insti, 
                                        kits_distr_pharmacies$insti, 
                                        kits_distr_pharmacies$oraq)) %>%
  dplyr::mutate(., storedBy = "[srp]")


# Remap kit distributed via workplace
kits_distr_workplace_tr <- 
kits_distribution$workplace$dataValues %>%
  dplyr::mutate(., dataElement = ifelse(dataElement == kits$insti, 
                                        kits_distr_workplace$insti, 
                                        kits_distr_workplace$oraq)) %>%
  dplyr::mutate(., storedBy = "[srp]")

# Transformed kits
kits_distribution_tr <- list(hotspots = kits_distr_hotspots_tr, 
                             pharmacies = kits_distr_pharmacies_tr, 
                             workplace = kits_distr_workplace_tr)

# Re-upload transformed kits ------------------------------------------------------------------

kits_distribution_d <- purrr::map(kits_distribution_tr,function(x){
                                    httr::POST(paste0(baseurl, "api/dataValueSets?importStrategy=CREATE_AND_UPDATE"), 
                                               body = jsonlite::toJSON(list(dataValues = x), auto_unbox = T), 
                                               httr::content_type_json())
                                  })



# Did any of the API request fail? ------------------------------------------

any_fails <- 
  purrr::map_lgl(kits_distribution_d, function(x){
  httr::http_error(x)
}) %>% 
  all(.)


if (any_fails){
  stop(
    sprintf(
      "PSI - MIS API Upload request failed \n<%s>",
      "https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual.html"
    ),
    call. = FALSE
  )
}















