
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Coral Metabolomics: Classification of Discrete *Acropora aspera* Phenotypes Associated with a Simulated Bleaching Event and Elevated Carbon Dioxide

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3894132.svg)](https://doi.org/10.5281/zenodo.3894132)
<!-- badges: end -->

This repositry contains the data and analysis of chapter 4 of BGs
thesis, which examines the effects of ocean acidification and warming on
the metabolome of *Acropora aspera*. It is provided as an R package to
ensure reproducibility while adopting a common standard project
structure; and to explore the merits of doing so. It provides functions
to reproduce all steps of the analysis including preprocessing,
modelling, figure and table generation.

Please note that this package and its functions have been written
explicitly for this project and may have limited use outside of the
package.

### Package Structure

  - [data](data/): Contains the preprocessed data as R data files.
  - [data-raw](data-raw/) : Contains the raw data and a small subset of
    the raw LCMS data.
  - [inst/extdata](inst/extdata) : Contains installed data.
  - [man](man/): Contains the function and dataset documentation files.
  - [R](R/): Contains the function scripts used in the analysis.

### Usage

You can install coralclass in R using the devtools package:

``` r
devtools::install_github("brgordon17/coralclass")
```

### License

MIT License. Copyright (c) 2020 Benjamin R. Gordon

For more information, please see the [LICENSE](LICENSE.md) file.
