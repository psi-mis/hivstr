
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hivstr

<!-- badges: start -->

<!-- badges: end -->

The goal of hivstr is to bypass using the Kenya HIV self-testing
distribution channels, a category combination, directly from the
analytic views, making it easier to relate the HIV self-testing data on
a single chart/table.

## Installation

hivstr is on its way to [CRAN](https://CRAN.R-project.org). You can
install the released version of hivstr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hivstr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("psi-mis/hivstr")
```

## Usage

Load the package with:

``` r
library(hivstr)
```

Then set and pass credentials to a PSI MIS.

``` r
# server url, default is clone
baseurl <- "https://clone.psi-mis.org/"

# enter username & password
usr <- "ke_hiv"
pwd <- "Temp@123"

# login to a PSI MIS
api_basic_auth(baseurl = baseurl, username = usr, password = pwd)
#> [1] TRUE
```

Run this function to bypass HIVST KE distribution channels:

``` r

bypass_ke_distr(baseurl)
```

Expect a TRUE for a successive update, otherwise there will be errors.

Alternatively, please download the scripted file
[here](https://github.com/psi-mis/hivstr/blob/main/ke_hivst-script.R)
and source it from your R CMD like this:

``` r
source("~ the location of the script")
```
