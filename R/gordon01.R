#' gordon01: A  package style research compendium for Gordon et. al. 2018
#'
#' The package provides all the functions and data to reproduce the analysis
#' performed by Gordon et. al. This experiment examined the synergistic effects
#' of ocean warming and acidification on the coral Acropora aspera at Heron
#' Island in 2011.
#'
#' @section Functions:
#' \itemize{
#' \item create_litmz()
#' \item create_mzdata()
#' \item create_nmrdata()
#' \item figure_pca()
#' \item figure_tuning()
#' \item mzdata_raw()
#' \item mzpca()
#' \item mzpls()
#' \item mzrf()
#' \item nmrpca()
#' \item nmrpls()
#' \item nmrrf()
#' \item plot_base()
#' \item qual_colours()
#' \item sd_colours()
#' \item seq_colours()
#' \item table_cv_performance()
#' \item theme_brg_grid()
#'}
#'
#' @author Benjamin R. Gordon
#'
#' @docType package
#' @name gordon01
#' @import caret
#' @import ggplot2
#' @import dplyr
NULL

# Quiets R CMD CHECK notes where there is no visible binding for some global
# variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("Accuracy",
                           "Endnote Reference",
                           "Reported m/z",
                           "Reported ion",
                           "Monoisotopic Mass (Da)",
                           "Molecular Formula",
                           "Reported Compound",
                           "mtry",
                           "mzdata",
                           "scores",
                           "PC1",
                           "PC2",
                           "x_lab",
                           "y_lab",
                           "Genus",
                           "Reported compound name",
                           "ncomp",
                           "nmrdata",
                           "ref",
                           "save.mzpls",
                           "obs",
                           "obs_day",
                           "n",
                           "Day",
                           "Proportion",
                           "qual_colours",
                           "mz",
                           "control",
                           "eT",
                           "eCO2",
                           "eCO2eT",
                           "importance",
                           "mz_neutral",
                           "model",
                           "mz_low",
                           "mz_high",
                           "monoiso_mass",
                           "litmz",
                           "dummy",
                           "endnote_ref",
                           "vip"))

}
