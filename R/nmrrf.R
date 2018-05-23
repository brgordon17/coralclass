#' Random Forests Analysis of 1H-NMR data.
#'
#' \code{nmrrf()} was used to perform the random forests analysis of the LCMS
#' data (\code{./data/nmrdata.rda})
#'
#' \code{nmrrf()} loads \code{nmrdata} and performs a RF analysis of the data
#' using \code{nmrdata$class} as outcomes. The process is outlined as follows:
#' \enumerate{
#' \item The data is split into training and test sets using an 80:20 stratified
#' split according to class and day \code{nmrdata$class_day}.
#' \item A list of random seeds is produced for each iteration of the CV
#' process. For the 10-fold, repeated (3 times) CV used here, we require 10 * 3
#' seeds for each mtry value assesed (tune grid length).
#' \item Define a tuning grid of mtry values. In this case we assess mtry values
#' \code{c(25, 75, 100, seq(from = 100, to = 500, by = 50))} giving a tunegrid
#' length of 12.
#' \item Define the CV parameters. 10 folds, 3 repeats, default summary. We also
#' define the method for selecting the best tune. In this case, the best tune is
#' the simplest model within one standard error of the empirically
#' optimal model. This rule, as described by Breiman et al. (1984), may avoid
#' overfitting the model. Note that k-fold CV as performed using
#' \code{trainControl(method = "repeatedcv")} stratifies sampling according to
#' class.
#' \item The data is centred by subtracting the mean of the predictor's data
#' from the predictor values
#' \item The data is scaled by dividing the predictor's by the standard
#' deviation.
#' \item The model is run.
#' }
#'
#' @param parallel Logical indicating if parallel processing should be used.
#' @param save.model Logical indicating if the model should be saved.
#' @param view.plot Logical indicating if a plot of accuracy vs mtry should
#' be printed to the plot viewer.
#' @param save.plot Logical indicating if plot should be saved to a \code{.pdf}
#' in the \code{./figs} directory.
#' @param plot.name Name of plot if \code{save.plot = TRUE}.
#' @param model.name Name of model if \code{save.model = TRUE}.
#' @param seed An integer for setting the RNG state.
#' @param pred.results Logical indicating if the results of predicting the test
#' data should be printed to the console.
#' @param ... Other arguments passed on to individual methods.
#'
#' @return returns a list with class \code{train}.
#'
#' @note Although this function is exported, \code{nmrrf()} was not intended to
#' be used outside of this package. Run this function with default values to
#' reproduce the results in this package.
#'
#' @seealso
#' \code{\link[caret]{train}}
#' \code{\link[ggplot2]{ggplot}}
#' \href{http://topepo.github.io/caret/index.html}{The caret Package} by Max Kuhn (2017)
#'
#' @author Benjamin R. Gordon
#'
#' @import caret
#'
#' @export
#'
