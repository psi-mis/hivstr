
# install devtools & hivstr if not installed ------------------------------------------------------------------

if (!require("devtools")) install.packages("devtools")

if (!require("hivstr")) install_github("psi-mis/hivstr")


# set up ------------------------------------------------------------------

# server url, default is clone
baseurl <- "https://clone.psi-mis.org/" 

# username
usr <- "ke_hiv" 

# password
pwd <- "Temp@123" 

# login to a PSI MIS
api_basic_auth(baseurl = baseurl, username = usr, password = pwd)
#> [1] TRUE

# run script, expect TRUE
bypass_ke_distr(baseurl)
