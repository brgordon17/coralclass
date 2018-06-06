#' create litmz.
#'
#' \code{create_litmz()} cleans \code{./data-raw/metabs_literature.csv} for
#' future analysis
#'
#' \code{create_litmz()} cleans up the headers and removes Endnote referencing
#' in \code{metabs_literature.csv}.
#'
#' @param save.rda Logical indicating if a .rda file should be saved to
#' \code{/data}
#'
#' @return Returns a dataframe of class \code{tbl_df}
#'
#' @note \code{create_litmz()} was not intended to be used outside of this
#' package.
#'
#' @author Benjamin R. Gordon
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
# Load literature variables and clean up ---------------------------------------
create_litmz <- function(save.rda = TRUE) {

  litmz <- readr::read_csv("./data-raw/metabs_literature.csv", col_names = TRUE)
  litmz <- litmz %>%
    dplyr::select(-`Endnote Reference`) %>%
    dplyr::rename(reported_mz = `Reported m/z`,
                  reported_ion = `Reported ion`,
                  moniso_mass = `Monoisotopic Mass (Da)`,
                  molec_formula = `Molecular Formula`,
                  reported_name = `Reported compound name`,
                  taxon = `Genus`,
                  ref = `ref`
                  )

  if(save.rda) {
    devtools::use_data(litmz)
  }

  litmz

}

## Document data ---------------------------------------------------------------

#' litmz
#'
#' A dataset of published and/or known mz ions identified in either coral,
#' Symbiodinium or other closely related species.
#'
#' @format A tibble with 230 rows and 7 variables:
#' \describe{
#'   \item{reported_mz}{the m/z reported}
#'   \item{reported ion}{the ion reported}
#'   \item{monoiso_mass}{the monoisotopic mass}
#'   \item{molec_formula}{the molecular formula}
#'   \item{reported_name}{the reported name}
#'   \item{taxon}{the taxonomic information}
#'   \item{ref}{the reference where the information was reported. This is
#'   currently in custom endnote format and will be updated in the future.}
#' }
#' @source Benjamin R. Gordon
"litmz"
