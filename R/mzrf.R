#' Random Forests Analysis of LCMS data.
#'
#' \code{mzrf()} was used to perform the random forests analysis of the LCMS
#' data (\code{./data/mzdata.rda})
#'
#' \code{mzrf()} loads \code{mzdata} and performs a RF analysis of the data
#' using \code{mzdata$class} as outcomes. The process is outlined as follows:
#' \enumerate{
#' \item The data is split into training and test sets using an 80:20 stratified
#' split according to class and day \code{mzdata$class_day}.
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
#' @note Although this function is exported, \code{mzrf()} was not intended to
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
mzrf <- function(parallel = TRUE,
                 save.model = FALSE,
                 view.plot = TRUE,
                 save.plot = FALSE,
                 plot.name = "mzrf_cv_plot",
                 model.name = "mzrf_model",
                 seed = 1978,
                 pred.results = TRUE,
                 ...) {

  # Modelling setup -----------------------------------------------------------
  rfdata <- data.frame(mzdata[-which(mzdata$class == "PBQC"), ])
  rfdata <- droplevels(rfdata)

  set.seed(seed)
  index <- caret::createDataPartition(rfdata$class_day,
                                      p = .8,
                                      list = FALSE,
                                      times = 1
                                      )
  train_data <- rfdata[index, ]
  test_data  <- rfdata[-index, ]

  tunegrid <- expand.grid(.mtry = c(25, 50, 75,
                                    seq(from = 100, to = 500, by = 50)
                                    ))

  set.seed(seed)
  seeds <- vector(mode = "list", length = 31)
  for(i in 1:30) seeds[[i]] <- sample.int(1000, length(tunegrid[,1]))
  seeds[[31]] <- sample.int(1000, 1)

  ctrl <- caret::trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              summaryFunction = defaultSummary,
                              seeds = seeds,
                              savePredictions = "all",
                              selectionFunction = "oneSE"
                              )

  ## Create model --------------------------------------------------------------
  if(parallel) {
    doMC::registerDoMC()
    set.seed(seed)
    mzrf <- caret::train(x = train_data[, -1:-6],
                         y = train_data$class,
                         method = "rf",
                         trControl = ctrl,
                         preProc = c("center", "scale"),
                         allowParallel = TRUE,
                         importance = TRUE,
                         tuneGrid = tunegrid
                         )
  }

  else {
    set.seed(seed)
    mzrf <- caret::train(x = train_data[, -1:-6],
                         y = train_data$class,
                         method = "rf",
                         trControl = ctrl,
                         preProc = c("center", "scale"),
                         allowParallel = FALSE,
                         importance = TRUE,
                         tuneGrid = tunegrid
                         )
  }

  preds <- caret::confusionMatrix(stats::predict(mzrf,
                                                 newdata = test_data),
                                  test_data$class)

  # Plotting -------------------------------------------------------------------
  custom_colours <- phdhelpr::warm_colours[4] # red

  train_plot <- ggplot(mzrf$results, aes(x = mtry,
                                         y = Accuracy)) +
    geom_line(colour = custom_colours,
              size = 1) +
    geom_point(aes(x = mtry[mtry == 350],
                   y = Accuracy[mtry == 350]),
               colour = custom_colours,
               size = 3) +
    scale_x_continuous(limits = c(10, 550),
                       expand = c(0, 0),
                       name = "mtry") +
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
             x = 350,
             y = 0.8428571 + 0.15,
             label = "LCMS RF") +
    annotate("text",
             x = 350,
             y = 0.8426587 + 0.08,
             label = "Best Tune (350, 0.84)")

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
    saveRDS(mzrf, paste(c("./inst/extdata/", model.name, ".rds"), collapse = ""))
  }

  if(pred.results) {
    print(preds)
  }

  mzrf

}
