#' Annotate LCMS features
#'
#' \code{table_annotate()} compares the 20 most important variables
#' identified in the PLS-DA and Random Forests modelling of the LCMS data with
#' the monoisotopic mass of those in \link[coralmz]{coralmz}.
#'
#' \code{table_annotate()} Loads the LCMS PLS-DA and LCMS RF models and
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
#' file. Matches with compounds in the \code{coralmz} package, if any,
#' are retrieved and can be optionally saved to \code{./tables/} as a comma
#' separated \code{.txt} file.
#'
#' @param ppm Numeric mass error to use for cross referencing
#' @param save.impvars Logical indicating if the table of the 20 most important
#' variables for each model should be saved to \code{./inst/extdata/}
#' @param save.matches Logical indicating if the table of cross referencing
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
#' \code{\link[coralmz]{coralmz}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
table_annotate <- function(ppm = 50,
                           save.impvars = FALSE,
                           save.matches = FALSE) {

  # retrieve important variables -----------------------------------------------
  mzpls_mod <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf_mod <- readRDS("./inst/extdata/mzrf_model.rds")

  mzpls_impvars <- caret::varImp(mzpls_mod, scale = TRUE)
  mzpls_impvars <- tibble::as_tibble(mzpls_impvars$importance, rownames = "mz")
  mzpls_impvars <-
    mzpls_impvars %>%
    mutate(mz = gsub("mz_", "", mzpls_impvars$mz)) %>%
    rowwise() %>%
    transmute(
      mz = as.numeric(mz),# warnings arise from features with two decimal points
      importance = max(control, eT, eCO2, eCO2eT)) %>%
    arrange(desc(importance)) %>%
    slice(1:20)

  mzrf_impvars <- caret::varImp(mzrf_mod, scale = TRUE)
  mzrf_impvars <- tibble::as_tibble(mzrf_impvars$importance, rownames = "mz")
  mzrf_impvars <-
    mzrf_impvars %>%
    mutate(mz = gsub("mz_", "", mzrf_impvars$mz)) %>%
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

  # Cross reference with coralmz -----------------------------------------------
  pls_rf_matches <-
    pls_rf_impvars %>%
    mutate(dummy = TRUE,
           adduct = NA) %>%
    left_join(coralmz::coralmz %>% mutate(dummy = TRUE))  %>%
    filter(monoiso_mass <= mz_high, monoiso_mass >= mz_low) %>%
    select(-dummy,
           -mz_neutral,
           -mz_low,
           -mz_high,
           -importance)

  # cross reference any adducts ------------------------------------------------
  mzdata_raw <- read_csv("./data-raw/mzdata-raw.csv", na = "0")
  mzdata_raw[1] <- NULL
  colnames(mzdata_raw)[1] <- "mz_raw"

  # identify adducts from pls_rf_impvars
  adduct_matches <-
    pls_rf_impvars %>%
    mutate(dummy = TRUE) %>%
    left_join(mzdata_raw %>% mutate(dummy = TRUE))  %>%
    filter(near(mz, mz_raw, tol = .0001)) %>%
    select(-dummy,
           -7:-133,
           -pcgroup) %>%
    mutate(mz_neutral = as.numeric(str_sub(adduct, str_length(adduct)-6, -1))) %>%
    mutate(adduct = str_sub(adduct, 1, str_length(adduct)-8)) %>%
    filter(!is.na(mz_neutral))

  # add variables for 50ppm error based on adduct mz (mz_neutral)
  adduct_matches <-
    adduct_matches %>%
    rowwise() %>%
    mutate(mz_low = mz_neutral - (mz_neutral * ppm/10^6),
           mz_high = mz_neutral + (mz_neutral * ppm/10^6)) %>%
    ungroup()

  # Cross reference with coralmz
  adduct_matches <-
    adduct_matches %>%
    mutate(dummy = TRUE) %>%
    left_join(coralmz::coralmz %>% mutate(dummy = TRUE))  %>%
    filter(monoiso_mass <= mz_high, monoiso_mass >= mz_low) %>%
    select(-dummy,
           -mz_neutral,
           -mz_low,
           -mz_high,
           -importance)

  # Tidy up references ---------------------------------------------------------
    pls_rf_annotated <-
      bind_rows(pls_rf_matches, adduct_matches) %>%
      mutate(reference = stringr::str_replace(reference, ",", ";"))

  # Output and saves -----------------------------------------------------------
  if (save.impvars) {
    saveRDS(pls_rf_impvars, "./inst/extdata/pls_rf_impvars.rds")
  }

  if (save.matches) {
    readr::write_csv(pls_rf_annotated, "./tables/annotated_features.txt")
  }

  message("duplicate mz values have .1 appended to the value and may produce
          some warnings")

  pls_rf_annotated

}
