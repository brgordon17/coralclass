#' Function to preprocess raw 1H-NMR binned data.
#'
#' \code{nmrdata()} was used to tidy up \code{./data-raw/nmrdata-raw-0.02.csv}
#' and output \code{./data/nmrdata.rda}.
#'
#' \code{nmrdata()} takes the binned data from Bruker's Amix and creates new
#' categorical variables based on the sample ID's. It then removes any unwanted
#' variables as defined by the user (e.g. residual methanol or water peaks)
#' before saving the results to \code{./data}
#'
#' @param savecsv Logical indicating if output should be saved as a \code{.csv}
#' file to the current working directory
#' @param saverda Logical indicating if a .rda file should be saved to /data
#' @param csvname The name of the output .csv file to be saved if TRUE
#' @param remove.vars Logical indicating if user-defined variables should be
#' removed
#' @param vars A character vector of variables to be removed
#' @param ... Other arguments passed on to individual methods
#'
#' @return Returns a dataframe of class tbl_df
#'
#' @note \code{nmrdata()} was not intended to be used outside of this package.
#' Please use \code{gordon01:::nmrdata()} to run this function after installing
#' the package.
#'
#' @examples
#' gordon01:::nmrdata(savecsv = FALSE,
#'                    saverda = TRUE,
#'                    csvname = "nmrdata",
#'                    remove.vars = TRUE,
#'                    vars = c("3.31"))
#'
nmrdata <- function(savecsv = FALSE,
                    saverda = TRUE,
                    csvname = "nmrdata",
                    remove.vars = TRUE,
                    vars = c("3.31"),
                    ...) {

  library(devtools)
  library(tidyverse)

  nmrdata  <-  read_csv("./data-raw/nmrdata-raw-0.02.csv", na = "0")
  colnames(nmrdata)[1] <- "sample_ids"
  sample_ids <- nmrdata[1]
  var_ids <- round(as.numeric(colnames(nmrdata[-1])),2)
  colnames(nmrdata)[2:ncol(nmrdata)] <- var_ids

  # create categorical variables -----------------------------------------------
  class <- c(rep("eCO2", 6), # 1000-0902-C
             rep("eCO2eT", 6), # 1000-0902-H
             rep("eCO2", 6), # 1000-1202-C
             rep("eCO2eT", 6), # 1000-1202-H
             rep("eCO2", 6), # 1000-1402-C
             rep("eCO2eT", 6), # 1000-1402-H
             rep("eCO2", 6), #1000-d14-C
             rep("eCO2eT", 6), # 1000-d14-H
             rep("control", 6), # 380-0902-C
             rep("eT", 6), # 380-0902-H
             rep("control", 6), # 380-1202-C
             rep("eT", 3), # 380-1202-H
             rep("control", 6), # 380-1402-C
             rep("eT", 6),# 380-1402-H
             rep("control", 6), # 380-d14-C
             rep("eT", 6)) # 380-d14-H
  class <- factor(class, levels = c("control", "eT", "eCO2", "eCO2eT"))

  day <- c(rep("day1", 12),
           rep("day4", 12),
           rep("day6", 12),
           rep("day14", 12),
           rep("day1", 12),
           rep("day4", 9),
           rep("day6", 12),
           rep("day14", 12))
  day <- factor(day, levels = c("day1", "day4", "day6", "day14"))

  tank <- c(rep(c("L","L","L", "R","R","R"), 11),
            c("L","L","L"),
            rep(c("L","L","L", "R","R","R"), 4))
  tank <- factor(tank, levels = c("L", "R"))

  rep <- c(rep(c(1, 2, 3), 31))
  rep <- factor(rep, levels = (c(1:3)))

  nmrdata <- as_tibble(data.frame(sample_ids,
                                  class,
                                  day,
                                  tank,
                                  rep,
                                  nmrdata[2:ncol(nmrdata)],
                                  check.names = FALSE))
  nmrdata <- arrange(nmrdata, class, day)

  if(remove.vars) {
    nmrdata <- nmrdata %>%
      select(-one_of(vars))
  }

  # write data -----------------------------------------------------------------
  if(saverda) {
    use_data(nmrdata)
  }

  if(savecsv) {
    write_csv(nmrdata, paste(c("./data/", csvname, ".csv"), collapse = "")
    )
  }

  nmrdata
}




