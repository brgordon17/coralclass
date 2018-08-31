#' Cross validation performance table.
#'
#' \code{table_cv_performance()} reproduces the cross validation performance
#' table in section x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{table_cv_performance()} loads the saved models in
#' \code{./inst/extdata/}; calculates the cross validation confusion matrices
#' and statistics for each model; creates a table containing the accuracy and
#' the class specific sensitivity and specificity for each model; saves the
#' table to \code{./tables/} as a \code{.csv} file with a \code{.txt} file
#' extension.
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
#' \code{table_cv_performance()} was not intended to be used outside of this
#' package.
#'
#' @seealso
#' \code{\link[caret]{confusionMatrix}}
#' \code{\link[tibble]{tibble}}
#' \code{\link[gordon01]{table_model_test}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
table_cv_performance <- function(table.name = "CV_perform_table",
                                 save.table = FALSE) {
  # load models ----------------------------------------------------------------
  mzpls_mod <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf_mod <- readRDS("./inst/extdata/mzrf_model.rds")
  nmrpls_mod <- readRDS("./inst/extdata/nmrpls_model.rds")
  nmrrf_mod <- readRDS("./inst/extdata/nmrrf_model.rds")

  # resampling predictions -----------------------------------------------------
  mzpls_preds <-
    confusionMatrix(mzpls_mod$pred$pred[mzpls_mod$pred$ncomp ==
                                          mzpls_mod$bestTune$ncomp],
                    mzpls_mod$pred$obs[mzpls_mod$pred$ncomp ==
                                         mzpls_mod$bestTune$ncomp])
  mzrf_preds <-
    confusionMatrix(mzrf_mod$pred$pred[mzrf_mod$pred$mtry ==
                                         mzrf_mod$bestTune$mtry],
                    mzrf_mod$pred$obs[mzrf_mod$pred$mtry ==
                                        mzrf_mod$bestTune$mtry])
  nmrpls_preds <-
    confusionMatrix(nmrpls_mod$pred$pred[nmrpls_mod$pred$ncomp ==
                                           nmrpls_mod$bestTune$ncomp],
                    nmrpls_mod$pred$obs[nmrpls_mod$pred$ncomp ==
                                          nmrpls_mod$bestTune$ncomp])
  nmrrf_preds <-
    confusionMatrix(nmrrf_mod$pred$pred[nmrrf_mod$pred$mtry ==
                                          nmrrf_mod$bestTune$mtry],
                    nmrrf_mod$pred$obs[nmrrf_mod$pred$mtry ==
                                         nmrrf_mod$bestTune$mtry])

  # Build table ----------------------------------------------------------------
  table <-
    tibble::tibble(Method = c("LCMS_PLSDA", "LCMS_RF", "NMR_PLSDA", "NMR_RF"))

  table$Accuracy <- round(c(mzpls_preds$overall[["Accuracy"]],
                            mzrf_preds$overall[["Accuracy"]],
                            nmrpls_preds$overall[["Accuracy"]],
                            nmrrf_preds$overall[["Accuracy"]]),
                          2)

  sens <- tibble::as_tibble(rbind(round(mzpls_preds$byClass[, 1], 2),
                                  round(mzrf_preds$byClass[, 1], 2),
                                  round(nmrpls_preds$byClass[, 1], 2),
                                  round(nmrrf_preds$byClass[, 1], 2)))
  colnames(sens) <- gsub("Class: ", "Sens_", colnames(sens))

  spec <- tibble::as_tibble(rbind(round(mzpls_preds$byClass[, 2], 2),
                                  round(mzrf_preds$byClass[, 2], 2),
                                  round(nmrpls_preds$byClass[, 2], 2),
                                  round(nmrrf_preds$byClass[, 2], 2)))
  colnames(spec) <- gsub("Class: ", "Spec_", colnames(spec))

  table <- tibble::as_tibble(cbind(table, sens, spec))

  if (save.table) {
    readr::write_csv(table,
                     paste(c("./tables/", table.name, ".txt"),
                           collapse = ""))
  }

  table

}

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

#' Cross reference LCMS features
#'
#' \code{table_crossref()} compares the 20 most important variables
#' identified in the PLS-DA and Random Forests modelling of the LCMS data with
#' the monoisotopic mass of those in \code{\link{litmz}}.
#'
#' \code{table_crossref()} Loads the LCMS PLS-DA and LCMS RF models and
#' creates a table (\code{list$imp_vars}) of the 20 most important variables
#' according to \code{\link[caret]{varImp}}. The cross referencing relies on the
#' user defined mass error. Three variables are created to define the upper and
#' lower bounds of the mass error:
#' \describe{
#' \item{$mz_neutral}{The neutral mass of the ion calculated by subtracting the
#' monoisotopic mass of the hydrogen ion (1.007276 Da)}
#' \item{$mz_low}{Equal to mz_neutral - the user defined ppm error}
#' \item{$mz_high}{Equal to mz_neutral + the user defined ppm error}
#' }
#' The table of the 20 most important variables and their mass error can be
#' optionally saved to \code{./inst/extdata/} as a \code{.rds}
#' file. Matches with compounds in \code{litmz}, if any, are retrieved and can
#' be optionally saved to \code{./tables/} as a comma separated \code{.txt}
#' file.
#'
#' @param ppm Numeric mass error to use for cross referencing
#' @param ref.type The format of the references column in the matched variables
#' table. Can be one of either "generic" or "endnote". Defaults to
#' \code{"generic"} as the endnote format, and its associated record number, is
#' relevant only to the author.
#' @param save.impvars Logical indicating if the table of the 20 most important
#' variables for each model should be saved to \code{./inst/extdata/}
#' @param save.matches Logical indication if the table of cross referencing
#' matches should be save to \code{./tables/}
#'
#' @return Returns a list containing the important and matched variables
#'
#' @note Commas in the references variable are replaced with semi colons to
#' avoid conflicts with downstream use of the \code{.csv} file. Although this
#' function is exported, \code{table_crossref()} was not intended to be used
#' outside of this package.
#'
#' @seealso
#' \code{\link[caret]{varImp}}
#' \code{\link{litmz}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
table_crossref <- function(ppm = 50,
                           ref.type = c("generic", "endnote"),
                           save.impvars = FALSE,
                           save.matches = FALSE) {

  ref.type <- match.arg(ref.type)

  # retrieve important variables -----------------------------------------------
  mzpls_mod <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf_mod <- readRDS("./inst/extdata/mzrf_model.rds")

  mzpls_impvars <- caret::varImp(mzpls_mod, scale = TRUE)
  mzpls_impvars <- tibble::as_tibble(mzpls_impvars$importance)
  mzpls_impvars <-
    mzpls_impvars %>%
    mutate(mz = gsub("mz_", "", rownames(mzpls_impvars))) %>%
    rowwise() %>%
    transmute(
      mz = as.numeric(mz),# warnings arise from features with two decimal points
      importance = max(control, eT, eCO2, eCO2eT)) %>%
    arrange(desc(importance)) %>%
    slice(1:20)

  mzrf_impvars <- caret::varImp(mzrf_mod, scale = TRUE)
  mzrf_impvars <- tibble::as_tibble(mzrf_impvars$importance)
  mzrf_impvars <-
    mzrf_impvars %>%
    mutate(mz = gsub("mz_", "", rownames(mzrf_impvars))) %>%
    rowwise() %>%
    transmute(
      mz = as.numeric(mz),# warnings arise from features with two decimal points
      importance = max(control, eT, eCO2, eCO2eT)) %>%
    arrange(desc(importance)) %>%
    slice(1:20)

  # Add variables for ppm error ranges -----------------------------------------
  mzpls_impvars <-
    mzpls_impvars %>%
    rowwise() %>%
    mutate(
      model = "pls",
      mz_neutral = mz - 1.007276,
      mz_low = mz_neutral - (mz * ppm/10^6),
      mz_high = mz_neutral + (mz * ppm/10^6)) %>%
    select(model, everything()) %>%
    ungroup()

  mzrf_impvars <-
    mzrf_impvars %>%
    rowwise() %>%
    mutate(
      model = "rf",
      mz_neutral = mz - 1.007276,
      mz_low = mz_neutral - (mz * ppm/10^6),
      mz_high = mz_neutral + (mz * ppm/10^6)) %>%
    select(model, everything()) %>%
    ungroup()

  pls_rf_impvars <-
    bind_rows(mzpls_impvars, mzrf_impvars)

  # Cross reference with litmz -------------------------------------------------
  pls_rf_matches <-
    pls_rf_impvars %>%
    mutate(dummy = TRUE) %>%
    left_join(litmz %>% mutate(dummy = TRUE))  %>%
    filter(monoiso_mass <= mz_high, monoiso_mass >= mz_low) %>%
    select(-dummy,
           -mz_neutral,
           -mz_low,
           -mz_high)

  # References -----------------------------------------------------------------
  if (ref.type == "generic") {
    pls_rf_matches <-
      pls_rf_matches %>%
      mutate(ref = stringr::str_replace(endnote_ref, ",", ";")) %>%
      select(-endnote_ref)
  }

  else if (ref.type == "endnote") {
    pls_rf_matches <-
      pls_rf_matches %>%
      mutate(endnote_ref = stringr::str_replace(endnote_ref, ",", ";")) %>%
      select(-ref)
  }

  # Output and saves -----------------------------------------------------------
  if (save.impvars) {
    saveRDS(pls_rf_impvars, "./inst/extdata/pls_rf_impvars.rds")
  }

  if (save.matches) {
    readr::write_csv(pls_rf_matches, "./tables/important_variable_matches.txt")
  }

  impvars_matches <- list(imp_vars = pls_rf_impvars,
                          lit_matches = pls_rf_matches)

  message("duplicate mz values have .1 appended to the value and may produce
          some warnings")

  impvars_matches

}
