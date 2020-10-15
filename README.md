# ke_hivst.R

## overview 

Category combinations a.k.a cat combos are one of the techniques used to disaggregate data in DHIS2. They allow multiple categories to be combined into a related set, making it easier to collect and analyze data based on the categories. 

However, when used at the programs or data sets directly, cat combos are applied at the top level of the analytic views. This makes it technically challenging to relate data with other sources, particularly those with and without related dimensions. 

This R script is meant to bypass the use of the Kenya HIV self-testing distribution channels, a category combination, directly from the analytic views, making it easier to relate the HIV self-testing data on a single chart/table.

## Usage 

To run the script, you will need to have `R` and the following packages installed. 

-	**[httr](https://cran.r-project.org/web/packages/httr/index.html)** – for working with URLs and HTTP. 
-	**[jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)** – A json parser.
- **[magritrr](https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html)** – for forward piping.
-	**[dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)** – for data manipulation.
-	**[purrr](https://purrr.tidyverse.org/)** –  An enhanced functional programming tool. 

I am the using **[keyringr](https://cran.r-project.org/web/packages/keyringr/index.html)** currently to store and manage the my credentials. You will neeed to have this installed and set up your credentials appropriately. 

Then download and source the script from your directory like this.

``` r

Source( ~/YOUR DIRECTORY/HIVST/ke_hivst.R)

```






