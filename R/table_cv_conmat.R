#' Cross validation confusion matrices.
#'
#' \code{table_cv_conmat()} creates cross validation confusion matrices used in
#' table x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{table_cv_conmat()} loads the saved models in \code{./inst/extdata/};
#' reproduces then predicts the test data; creates a confusion matrix for each
#' model and returns them as a list of dataframes.
#'
#' @return returns a list of confusion matrices for each model.
#'
#' @note Although this function is exported, \code{table_cv_conmat()} was not
#' intended to be used outside of this package.
#'
#' @seealso
#' \code{\link[caret]{confusionMatrix}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
table_cv_conmat <- function() {

  # Load models ----------------------------------------------------------------
  mzpls_mod <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf_mod <- readRDS("./inst/extdata/mzrf_model.rds")
  nmrpls_mod <- readRDS("./inst/extdata/nmrpls_model.rds")
  nmrrf_mod <- readRDS("./inst/extdata/nmrrf_model.rds")

  # Create a list of matrices --------------------------------------------------
  mzpls_conmat <- confusionMatrix(mzpls_mod, norm = "none")
  mzpls_conmat <- as.data.frame.matrix(mzpls_conmat$table)

  nmrpls_conmat <- confusionMatrix(nmrpls_mod, norm = "none")
  nmrpls_conmat <- as.data.frame.matrix(nmrpls_conmat$table)

  mzrf_conmat <- confusionMatrix(mzrf_mod, norm = "none")
  mzrf_conmat <- as.data.frame.matrix(mzrf_conmat$table)

  nmrrf_conmat <- confusionMatrix(nmrrf_mod, norm = "none")
  nmrrf_conmat <- as.data.frame.matrix(nmrrf_conmat$table)

  conmats <- list(mzpls = mzpls_conmat,
                  nmrpls = nmrpls_conmat,
                  mzrf = mzrf_conmat,
                  nmrrf = nmrrf_conmat)

  conmats

}
