#' coralclass: A  package style research compendium for the coral
#' classification chapter of B. R. Gordon's PhD thesis.
#'
#' The package provides all the functions and data to reproduce the analysis
#' performed by Gordon et. al. This experiment examined the synergistic effects
#' of ocean warming and acidification on the coral Acropora aspera at Heron
#' Island in 2011.
#'
#' @section Functions:
#' \itemize{
#' \item create_mzdata()
#' \item create_nmrdata()
#' \item figure_pca()
#' \item figure_tuning()
#' \item figure_vip()
#' \item mzdata_raw()
#' \item mzpls()
#' \item mzrf()
#' \item nmrpls()
#' \item nmrrf()
#' \item table_annotate()
#' \item table_cv_conmat()
#' \item table_cv_performance()
#' \item table_model_test()
#'}
#'
#' @author Benjamin R. Gordon
#'
#' @docType package
#' @name coralclass
#' @import caret
#' @import ggplot2
#' @import dplyr
NULL

# Quiets R CMD CHECK notes where there is no visible binding for some global
# variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("Accuracy",
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
                           "save.mzpls",
                           "obs",
                           "obs_day",
                           "n",
                           "Day",
                           "Proportion",
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
                           "dummy",
                           "vip",
                           "reference",
                           "density",
                           "Observation2",
                           "mz_raw",
                           "pcgroup",
                           "adduct"))

}
