#' PLS-DA analysis of LCMS data.
#'
#' \code{mzpls()} was used to perform the partial least squares discriminant
#' analysis of the LCMS data (\code{./data/mzdata.rda})
#'
#' \code{mzpls()} loads \code{mzdata} and performs a PLS-DA of the data using
#' \code{mzdata$class} as outcomes. The process is outlined as follows:
#' \enumerate{
#' \item The data is split into training and test sets using an 80:20 stratified
#' split according to class and day \code{mzdata$class_day}.
#' \item A list of random seeds is produced for each iteration of the CV
#' process. For the 10-fold, repeated (3 times) CV used here, we require 10 * 3
#' seeds for each of the 50 principal components assesed.
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
#' @param view.plot Logical indicating if a plot of accuracy vs prcomp should
#' be printed to the plot viewer.
#' @param save.plot Logical indicating if plot should be saved to a \code{.pdf}
#' in the \code{./figs} directory.
#' @param save.gg Logical indicating if ggplot object should be saved to
#' \code{./inst/extdata/}
#' @param plot.name Name of plot if \code{save.plot = TRUE}.
#' @param model.name Name of model if \code{save.model = TRUE}.
#' @param seed An integer for setting the RNG state.
#' @param pred.results Logical indicating if the results of predicting the test
#' data should be printed to the console.
#' @param ... Other arguments passed on to individual methods.
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
mzpls <- function(parallel = TRUE,
                  save.model = TRUE,
                  view.plot = TRUE,
                  save.plot = FALSE,
                  save.gg = FALSE,
                  plot.name = "mzpls_cv_plot",
                  model.name = "mzpls_model",
                  seed = 1978,
                  pred.results = TRUE,
                  ...) {

  # Modelling setup -----------------------------------------------------------
  plsdata <- data.frame(mzdata[-which(mzdata$class == "PBQC"), ])
  plsdata <- droplevels(plsdata)

  set.seed(seed)
  index <- caret::createDataPartition(plsdata$class_day,
                                      p = .8,
                                      list = FALSE,
                                      times = 1
                                      )
  train <- plsdata[index, ]
  test  <- plsdata[-index, ]

  set.seed(seed)
  seeds <- vector(mode = "list", length = 31)
  for(i in 1:30) seeds[[i]] <- sample.int(1000, 50)
  seeds[[31]] <- sample.int(1000, 1)

  ## Set up k-fold CV using trainControl. Note that "repeatedcv" automatically
  ## performs stratified k-fold CV with its call to createMultiFolds()
  ctrl <- caret::trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              summaryFunction = defaultSummary,
                              seeds = seeds,
                              savePredictions = "all",
                              selectionFunction = "oneSE")

  ## Create model --------------------------------------------------------------
  if(parallel) {
    doMC::registerDoMC()
    set.seed(seed)
    mzpls <- caret::train(x = train[, -1:-6],
                          y = train$class,
                          method = "pls",
                          tuneLength = 50,
                          trControl = ctrl,
                          preProc = c("center", "scale"),
                          allowParallel = TRUE)
  }

  else {
    set.seed(seed)
    mzpls <- caret::train(x = train[, -1:-6],
                          y = train$class,
                          method = "pls",
                          tuneLength = 50,
                          trControl = ctrl,
                          preProc = c("center", "scale"),
                          allowParallel = FALSE)
  }

  preds <- caret::confusionMatrix(stats::predict(mzpls,
                                                 newdata = test),
                                  test$class)

  # Plotting -------------------------------------------------------------------

  custom_colours <- seq_colours[4] # red

  train_plot <- ggplot(mzpls$results,
                       aes(x = ncomp, y = Accuracy)) +
    geom_line(colour = custom_colours,
              size = 1) +
    geom_point(aes(x = ncomp[ncomp == 16],
                   y = Accuracy[ncomp == 16]),
                colour = custom_colours,
                size = 3) +
    scale_x_continuous(limits = c(1, 50),
                       expand = waiver(),
                       name = "Number of Components") +
    scale_y_continuous(limits = c(0, 1),
                       expand = waiver()) +
    theme_brg_grid() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.6),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(colour = "grey90",
                                      size = 0.6),
          axis.line.x = element_blank()) +
    annotate("text",
             x = 16,
             y = 0.8428571 + 0.15,
             label = "LCMS PLS-DA") +
    annotate("text",
              x = 16,
              y = 0.8428571 + 0.08,
              label = "Best Tune (16, 0.84)")

  if(view.plot) {
    print(train_plot)
  }

  if(save.plot) {
    grDevices::pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
                   width = 10,
                   height = 8,
                   useDingbats = FALSE)
    print(train_plot)
    grDevices::dev.off()
  }

  if(save.model) {
    saveRDS(mzpls, paste(c("./inst/extdata/", model.name, ".rds"), collapse = ""))
  }

  if (save.gg) {
    saveRDS(train_plot, "./inst/extdata/mzpls_tune_ggobject.rds")
  }

  if(pred.results) {
    print(preds)
  }

  mzpls

}
