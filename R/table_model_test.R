#' Model testing performance table.
#'
#' \code{table_model_test()} reproduces the model testing performance table in
#' section x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{table_model_test()} loads the saved models in
#' \code{./inst/extdata/}; reproduces then predicts the test data; calculates
#' the confusion matrices and statistics for the test data; creates a table
#' containing the accuracy and the class specific sensitivity and specificity
#' for each model; saves the table to \code{./tables/} as a \code{.csv} file
#' with a \code{.txt} file extension.
#'
#' @param table.name Name of table.
#' @param save.table Logical indicating if the plot should be saved to
#' \code{./tables/}.
#'
#' @return returns a tibble with class \code{tbl_df}.
#'
#' @note The \code{.txt} file extension makes it easy to open the file in a
#' text editor and copy/paste the contents to Word, where the text can be easily
#' converted to a table. Although this function is exported,
#' \code{table_model_test()} was not intended to be used outside of this
#' package.
#'
#' @seealso
#' \code{\link[caret]{confusionMatrix}}
#' \code{\link[tibble]{tibble}}
#' \code{\link[gordon01]{table_cv_performance}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
table_model_test <- function(table.name = "model_testing_table",
                             save.table = FALSE) {

  # Load models ----------------------------------------------------------------
  mzpls_mod <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf_mod <- readRDS("./inst/extdata/mzrf_model.rds")
  nmrpls_mod <- readRDS("./inst/extdata/nmrpls_model.rds")
  nmrrf_mod <- readRDS("./inst/extdata/nmrrf_model.rds")

  # Reproduce test data --------------------------------------------------------
  lcmsdata <- data.frame(mzdata[-which(mzdata$class == "PBQC"), ])
  lcmsdata <- droplevels(lcmsdata)

  set.seed(1978)
  mzindex <- createDataPartition(lcmsdata$class_day,
                                 p = .8,
                                 list = FALSE,
                                 times = 1)
  set.seed(1978)
  nmrindex <- createDataPartition(nmrdata$class_day,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)

  mztest  <- lcmsdata[-mzindex, ]
  nmrtest  <- data.frame(nmrdata[-nmrindex, ], check.names = FALSE)

  # Predict test data and get stats --------------------------------------------
  mzpls_preds <- stats::predict(mzpls_mod, newdata = mztest)
  mzrf_preds <- stats::predict(mzrf_mod, newdata = mztest)
  nmrpls_preds <- stats::predict(nmrpls_mod, newdata = nmrtest)
  nmrrf_preds <- stats::predict(nmrrf_mod, newdata = nmrtest)

  mzpls_conmat <- confusionMatrix(mzpls_preds, mztest$class)
  mzrf_conmat <- confusionMatrix(mzrf_preds, mztest$class)
  nmrpls_conmat <- confusionMatrix(nmrpls_preds, nmrtest$class)
  nmrrf_conmat <- confusionMatrix(nmrrf_preds, nmrtest$class)

  # Build table ----------------------------------------------------------------
  table <-
    tibble::tibble(Method = c("LCMS_PLSDA", "LCMS_RF", "NMR_PLSDA", "NMR_RF"))

  table$Accuracy <- round(c(mzpls_conmat$overall[["Accuracy"]],
                            mzrf_conmat$overall[["Accuracy"]],
                            nmrpls_conmat$overall[["Accuracy"]],
                            nmrrf_conmat$overall[["Accuracy"]]),
                          2)

  sens <- tibble::as_tibble(rbind(round(mzpls_conmat$byClass[, 1], 2),
                                  round(mzrf_conmat$byClass[, 1], 2),
                                  round(nmrpls_conmat$byClass[, 1], 2),
                                  round(nmrrf_conmat$byClass[, 1], 2)))
  colnames(sens) <- gsub("Class: ", "Sens_", colnames(sens))

  spec <- tibble::as_tibble(rbind(round(mzpls_conmat$byClass[, 2], 2),
                                  round(mzrf_conmat$byClass[, 2], 2),
                                  round(nmrpls_conmat$byClass[, 2], 2),
                                  round(nmrrf_conmat$byClass[, 2], 2)))
  colnames(spec) <- gsub("Class: ", "Spec_", colnames(spec))

  table <- tibble::as_tibble(cbind(table, sens, spec))

  if (save.table) {
    readr::write_csv(table,
                     paste(c("./tables/", table.name, ".txt"),
                           collapse = ""))
  }

  table

}
