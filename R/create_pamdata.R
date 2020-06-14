#' Create pamdata.
#'
#' \code{create_pamdata()} reproduces the mean FvFm data.
#'
#' This function reproduces the PAM data that was collected in this experiment.
#'
#' @param path The path where the .csv is located
#' @param saverda Logical indicating if a .rda file should be saved to /data
#'
#' @return Returns a dataframe of class tbl_df
#'
#' @author Benjamin R. Gordon
#'
#' @seealso
#' \code{\link[tibble]{tibble}}
#'
#' @export
#'
create_pamdata <- function(path = "./data-raw/pamdata.csv",
                           saverda = TRUE) {

  # read and sort data
  pamdata  <-  readr::read_csv(path, na = "0")
  pamdata <- dplyr::select(pamdata, day, class, FvFm)
  pamdata$class <- factor(pamdata$class,
                          levels = c("control", "eCO2", "eT", "eCO2eT"))

  # write data
  if(saverda) {
    save(pamdata, file = "./data/pamdata.rda", compress = "bzip2")
  }

  pamdata
}

## Data documentation ----------------------------------------------------------

#' Mean daily PAM data
#'
#' A dataset containing the mean PAM measurement for each class on days recorded
#'
#' @format A tibble with 28 rows and 3 variables:
#' \describe{
#'   \item{day}{day of measurement}
#'   \item{FvFm}{Photosynthetic yield}
#'   \item{class}{treatment class label}
#' }
#' @source Benjamin R. Gordon
"pamdata"
