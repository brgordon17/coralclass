#' Function to perform PLS-DA for LCMS data.
#'
#' \code{mzpls()} was used to perform the PLS-DA of the LCMS data
#' (\code{./data/mzdata.rda})
#'
#' \code{mzpls()} loads \code{mzdata} then partions the data into training and
#' test sets (80:20 train/test split). It then sets up unique seeds for each
#' iteration of the model.
#'
#' @param parallel Logical indicating if parallel processing should be used.
#' @param save.model Logical indicating if the model should be saved.
#' @param view.plot Logical indicating if a plot of accuracy vs prcomp should
#' be printed to the plot viewer.
#' @param save.plot Logical indicating if plot should be saved to a \code{.pdf}
#' in the \code{./figs} directory.
#' @param plot.name Name of plot if \code{save.plot = TRUE}
#' @param model.name Name of model if \code{save.model = TRUE}
#' @param seed The seed used for random number generation
#' @param ... Other arguments passed on to individual methods.
#'
#' @return returns a list with class \code{"prcomp"}.
#'
#' @note Although this function is exported, \code{mzpca()} was not intended to
#' be used outside of this package. Run this function with default values to
#' reproduce the results in this package.
#'
#' @seealso
#' \code{\link[caret]{train()}}
#' \code{\link[ggplot2]{ggplot()}}
#' \href{http://topepo.github.io/caret/index.html}{The caret Package by Max Kuhn (2017)}
#'
#' @examples
#' x <- gordon01:::mzpls()
#'
#' @author Benjamin R. Gordon
#'
#' @import caret
#'
#' @export
#'
mzpls <- function(parallel = TRUE,
                  save.model = TRUE,
                  view.plot = TRUE,
                  save.plot = FALSE,
                  plot.name = "mzpls_cv_plot",
                  model.name = "mzpls_model",
                  seed = 1978,
                  ...) {

  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package \"caret\" needed for this function to work. Please install
         it.",
         call. = FALSE)
  }

  # Modelling setup -----------------------------------------------------------
  load("./data/mzdata.rda")
  plsdata <- data.frame(mzdata[-which(mzdata$class == "PBQC"), ])
  plsdata <- droplevels(plsdata)
  ## Create test and training sets.
  set.seed(seed)
  index <- caret::createDataPartition(plsdata$class_day,
                                      p = .8,
                                      list = FALSE,
                                      times = 1
                                      )
  train <- plsdata[index, ]
  test  <- plsdata[-index, ]

  ## set seeds for each iteration of the model. I need 10 * 3 seeds for CV plus
  ## one for the final model (31). Since tunelength = 50 (50 prcomps), I need 50
  ## seeds for each of the 30 iterations of CV.
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
                              selectionFunction = "oneSE"
                              )

  ## Create model --------------------------------------------------------------
  if(parallel) {
    doMC::registerDoMC()
    set.seed(seed)
    mzpls = caret::train(x = train[, -1:-6],
                         y = train$class,
                         method = "pls",
                         tuneLength = 50,
                         trControl = ctrl,
                         preProc = c("center", "scale"),
                         allowParallel = TRUE
                         )
  }

  else {
    set.seed(seed)
    mzpls = caret::train(x = train[, -1:-6],
                         y = train$class,
                         method = "pls",
                         tuneLength = 50,
                         trControl = ctrl,
                         preProc = c("center", "scale"),
                         allowParallel = FALSE
                         )
  }

  custom_shapes <- gordon01::plot_shapes("triangle")
  custom_colours <- gordon01::seq_colours("red")

  train_plot <- ggplot(mzpls$results, aes(x = ncomp,
                                            y = Accuracy)
                         ) +
    geom_line(colour = custom_colours,
              size = 1) +
    geom_point(aes(x = ncomp[mzpls$bestTune$ncomp],
                    y = Accuracy[mzpls$bestTune$ncomp]),
                colour = custom_colours,
                size = 3) +
    scale_x_continuous(limits = c(1, 50),
                        expand = c(0, 0),
                        name = "Number of Components") +
    scale_y_continuous(limits = c(0, 1),
                        expand = c(0, 0)) +
    gordon01:::theme_brg_grid() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.6),
          axis.ticks.x = element_line(colour = "grey90",
                                      size = 0.6),
          axis.ticks.y = element_line(colour = "grey90",
                                      size = 0.6),
          axis.line.x = element_line(colour = "grey90",
                                     size = 0.6)) +
    annotate("text",
              x = mzpls$bestTune$ncomp,
              y = 0.91,
              label = paste("Best Tune (ncomp = ", mzpls$bestTune$ncomp, ")",
                            sep = ""))

  if(view.plot) {
    print(train_plot)
  }

  if(save.plot) {
    pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
        width = 10,
        height = 8,
        useDingbats = FALSE)
    print(train_plot)
    dev.off()
  }

  if(save.model) {
    saveRDS(mzpls, paste(c("./data/", model.name, ".rds"), collapse = ""))
  }

  mzpls

}
