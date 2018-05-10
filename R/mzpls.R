#' Function to perform PLS-DA for LCMS data.
#'
#' \code{mzpls()} was used to perform the PLS-DA of the
#' LCMS data (\code{./data/mzdata.rda})
#'
#' \code{mzpls()} zero-centres the data and performs a PCA using singular value
#' decomposition according to \code{\link[stats]{prcomp}}. It outputs a
#' \code{prcomp} object and a plot, which can be saved as either a \code{.pdf}
#' or a \code{.png} file, or both.
#'
#' @param scale Logical indicating if variables should be scaled to unit
#' variance.
#' @param center Logical indicating if variables should be zero centered.
#' @param savepdf Logical indicating if plot should be saved to \code{.pdf} in
#' the \code{./figs} directory.
#' @param savepng Logical indicating if plot should be saved to \code{.png} in
#' the \code{./figs} directory.
#' @param plotname Name of plot
#' @param ... Other arguments passed on to individual methods.
#'
#' @return returns a list with class \code{"prcomp"}.
#'
#' @note \code{mzpca()} was not intended to be used outside of this package
#' without modification to the underlying code.
#'
#' @seealso
#' \code{\link[stats]{prcomp()}}
#' \code{\link[ggplot2]{ggplot()}}
#'
#' @examples
#' x <- gordon01:::mzpca()
#'
mzpls <- function(scale = FALSE,
                  center = TRUE,
                  savepdf = TRUE,
                  savepng = TRUE,
                  plotname = "mzpca_plot",
                  ...) {

  library(caret)
  library(doMC)
  library(tidyverse)

  # Remove PBQCs
  data <- mzdata[-which(mzdata$class == "PBQC") ,]
  data <- droplevels(data)


}
















