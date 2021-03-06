
# install devtools & hivstr if not installed ------------------------------------------------------------------

if (!require("devtools")) install.packages("devtools")

#if (!require("hivstr")) install_github("psi-mis/hivstr")

#install the new version
devtools::install_github("psi-mis/hivstr")


# set up ------------------------------------------------------------------

library(hivstr)

# server url, default is clone
baseurl <- "https://clone.psi-mis.org/"

# username
usr <- "ke_tunza"

# password
pwd <- "Test@2020!"

# login to a PSI MIS
api_basic_auth(baseurl = baseurl, username = usr, password = pwd)
#> [1] TRUE

# run script, expect done
bypass_ke_distr(baseurl)
