#' Important variables in the LCMS PLS-DA model.
#'
#' \code{vip_mzpls()} was used to identify the important variables in the
#' LCMS PLS-DA model at (\code{./inst/extdata/mzpls_model.rds})
#'
#' \code{vip_mzpls()} loads \code{mzpls_model.rds} and
#'
#' @param
#'
#' @return returns a list with class \code{train}.
#'
#' @note Although this function is exported, \code{mzpls()} was not intended to
#' be used outside of this package.
#'
#' @seealso
#' \code{\link[caret]{train}}
#' \code{\link[ggplot2]{ggplot}}
#' \href{http://topepo.github.io/caret/index.html}{The caret Package} by Max Kuhn (2017)
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
vip_mzpls <- function(parallel = TRUE,
                  save.model = TRUE,
                  view.plot = TRUE,
                  save.plot = FALSE,
                  save.gg = FALSE,
                  plot.name = "mzpls_cv_plot",
                  model.name = "mzpls_model",
                  seed = 1978,
                  pred.results = TRUE,
                  ...) {
