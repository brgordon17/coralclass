
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Coral Metabolomics: Classification of Corals Exposed to Elevated Temperature and Carbon Dioxide

<!-- badges: start -->

[![License: CC
BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
<!-- badges: end -->

This repositry contains the data and analysis of my research into the
effects of ocean acidification and warming on the metabolome of *Acopora
aspera*. It is provided as an R package to ensure reproducibility while
adopting a common standard project structure, and to explore the merits
of doing so. It provides funcions for all steps of the analysis
including preprocessing, modelling as well as figure and table
generation.

Please note that this package and its functions have been written
explicitly for this project and may have limited use outside of the
package.

### Package Structure

  - [data](data/): Contains the preprocessed data as R data files.
  - [data-raw](data-raw/) : Contains the raw data and a small subset of
    the raw LCMS data
  - [inst](inst/) : Contains the `extdata/` folder that holds `.csv`
    files of the preprocessed data.
  - `man/`: Contains the function and dataset documentation.
  - `R/`: Contains the custom functions used in the analysis.

### Pacakage Installation

To install this package within R, ensure you have `devtools::` installed
and enter the code below at the R prompt:

``` r
devtools::install_github("brgordon17/gordon01")
```
