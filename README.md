<!-- README.md is generated from README.Rmd. Please edit that file -->
gordon01
========

CHANGE!!! zenodo DOI badge [![DOI](https://zenodo.org/badge/6271972.svg)](https://zenodo.org/badge/latestdoi/6271972)

### Authors

-   Benjamin R. Gordon
-   Check if I should add others? I may not need to if it's a thesis repo.

### Contributors

-   Other "authors" may be able to go here instead of into the authors section.

### Overview

This repositry contains the data and analysis of our research into the effects of ocean acidification and warming on the metabolome of the coral *Acopora aspera*. It has been permanantly archived at the DOI indicated by the above linked badge.

### R Package

This repositry is organised as an R package to ensure reproducibility of our analysis. It provides funcions for all steps of the analysis including **preprocessing**, **modelling** as well as **figure and table generation**. Please note that this package and its functions have been written explicitly for this project and may have limited use outside of the package.

[![Travis-CI Build Status](https://travis-ci.org/%3CUSERNAME%3E/%3CREPO%3E.svg?branch=master)](https://travis-ci.org/%3CUSERNAME%3E/%3CREPO%3E)

### Package Structure

-   `data/`: Contains the preprocessed data as R data files.
-   `data-raw/`: Contains the raw data and a small subset of our `.mzXML` data
-   `inst/`: Contains the `extdata/` folder that holds `.csv` files of the preprocessed data.
-   `man/`: Contains the function and dataset documentation.
-   `R/`: Contains the custom functions used in the analysis.

### Pacakage Installation

To install this package within R, ensure you have `devtools::` installed and enter the code below at the R prompt:

``` r
devtools::install_github("brgordon17/gordon01")
```

### Citation

Add citation

### R Session Information

``` r
devtools::session_info()
#> Session info -------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.5.0 (2018-04-23)
#>  system   x86_64, darwin15.6.0        
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_AU.UTF-8                 
#>  tz       Australia/Brisbane          
#>  date     2018-05-24
#> Packages -----------------------------------------------------------------
#>  package   * version date       source        
#>  backports   1.1.2   2017-12-13 CRAN (R 3.5.0)
#>  base      * 3.5.0   2018-04-24 local         
#>  compiler    3.5.0   2018-04-24 local         
#>  datasets  * 3.5.0   2018-04-24 local         
#>  devtools    1.13.5  2018-02-18 CRAN (R 3.5.0)
#>  digest      0.6.15  2018-01-28 CRAN (R 3.5.0)
#>  evaluate    0.10.1  2017-06-24 CRAN (R 3.5.0)
#>  graphics  * 3.5.0   2018-04-24 local         
#>  grDevices * 3.5.0   2018-04-24 local         
#>  htmltools   0.3.6   2017-04-28 CRAN (R 3.5.0)
#>  knitr       1.20    2018-02-20 CRAN (R 3.5.0)
#>  magrittr    1.5     2014-11-22 CRAN (R 3.5.0)
#>  memoise     1.1.0   2017-04-21 CRAN (R 3.5.0)
#>  methods   * 3.5.0   2018-04-24 local         
#>  Rcpp        0.12.16 2018-03-13 CRAN (R 3.5.0)
#>  rmarkdown   1.9     2018-03-01 CRAN (R 3.5.0)
#>  rprojroot   1.3-2   2018-01-03 CRAN (R 3.5.0)
#>  stats     * 3.5.0   2018-04-24 local         
#>  stringi     1.2.2   2018-05-02 CRAN (R 3.5.0)
#>  stringr     1.3.1   2018-05-10 CRAN (R 3.5.0)
#>  tools       3.5.0   2018-04-24 local         
#>  utils     * 3.5.0   2018-04-24 local         
#>  withr       2.1.2   2018-03-15 CRAN (R 3.5.0)
#>  yaml        2.1.19  2018-05-01 CRAN (R 3.5.0)
```
