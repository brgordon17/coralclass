#' Create mzdata.
#'
#' \code{create_mzdata()} is the pre-processed LCMS data used for modelling in
#' gordon01.
#'
#' Initially, the function takes the raw output from xcms and removes unwanted
#' data (e.g. retention times, isotopes, peak counts etc.). Then, it creates
#' new categorical variables based on the sample information. Finally, it
#' replaces true non-detects with noise, removes poorly resolved mass features
#' and then replaces the small number of remining missing values using random
#' forest imputaion. The list below details the logic behind the missing values
#' imputation:
#' \itemize{
#' \item \strong{TRUE NON-DETECTS:}
#'  Replace values missing in one class, but not others, with a random number
#'  between zero and the minimum of the matrix (i.e. noise). To be considered a
#'  true non-detect, a class should be missing at least 60 precent of its
#'  values. Achieved with,
#'  \code{metabolomics::MissingValues(group.cutoff = 0.6)}
#'  \item \strong{POORLY RESOLVED MASS FEATURES:}
#'  Remove mass features with more than 90 percent missing values. Achieved
#'  with, \code{metabolomics::MissingValues(column.cutoff = 0.9)}
#'  \item \strong{FALSE NON DETECTS:}
#'  Remaining missing values will be computed using
#'  \code{missForest::missForest()}. Achieved with,
#'  \code{metabolomics::MissingValues(complete.matrix = FALSE)}
#' }
#'
#' @param parallel Logical indicating if missing values imputation should be run
#' in parallel. If \code{TRUE}, the default number of cores is equal to half the
#' available number of cores
#' @param seed An integer used for setting seeds of random number generation
#' @param savecsv Logical indicating if output should be saved as a \code{.csv}
#' file to the current working directory
#' @param saverda Logical indicating if a .rda file should be saved to /data
#' @param csvname The name of the output .csv file to be saved if TRUE
#'
#' @return Returns a dataframe of class tbl_df
#'
#' @note Using \code{parallel = TRUE} is not reproducible. Future versions of
#' this function may include support for reproducible RNG seeds when using
#' parallel processing. Although this function is exported,
#' \code{create_mzdata()} was not intended to be used outside of this package.
#' Run this function with default values to reproduce the data used in this
#' package.
#'
#' @author Benjamin R. Gordon
#'
#' @seealso
#' \code{\link[metabolomics]{MissingValues}}
#' \code{\link[doMC]{registerDoMC}}
#' \code{\link[missForest]{missForest}}
#'
#' @import utils
#'
#' @export
#'
create_mzdata <- function(parallel = FALSE,
                          seed = 100,
                          savecsv = FALSE,
                          saverda = TRUE,
                          csvname = "mzdata"
                          ) {

  if (!requireNamespace("metabolomics", quietly = TRUE)) {
    stop("Package \"metabolomics\" needed for this function to work. Please
         install it.",
         call. = FALSE)
  }

  if (!requireNamespace("missForest", quietly = TRUE)) {
    stop("Package \"missForest\" needed for this function to work. Please
         install it.",
         call. = FALSE)
  }

  data  <-  readr::read_csv("./data-raw/mzdata-raw.csv", na = "0")

  # remove unwanted data -------------------------------------------------------
  data <- data[-1]
  data <- data[-2:-24]
  data <- data[-grep("[M+1]", data$isotopes, fixed = TRUE),]
  data <- data[-grep("[M+2]", data$isotopes, fixed = TRUE),]
  data <- data[-grep("[M+3]", data$isotopes, fixed = TRUE),]
  data <- data[-grep("[M+4]", data$isotopes, fixed = TRUE),]
  data <- data[-103:-105]
  mz_names <- round(data[1], 4)
  data <- data[-1]
  mz_names <- paste("mz", mz_names$mz, sep = "_")
  mz_names <- make.names(mz_names, unique = TRUE)
  data <- tibble::as_tibble(t(data), rownames = "sample_ids")
  colnames(data)[2:ncol(data)] <- mz_names
  sample_ids <- data[1]

  # Create categorical variables -----------------------------------------------
  class <- c(rep("eCO2", 6),
             rep("eCO2eT", 6),
             rep("eCO2", 5),
             rep("eCO2eT", 5),
             rep("eCO2", 6),
             rep("eCO2eT", 6),
             rep("eCO2", 6),
             rep("eCO2eT", 6),
             rep("control", 6),
             rep("eT", 6),
             rep("control", 6),
             rep("eT", 6),
             rep("control", 6),
             rep("eT", 3),
             rep("control", 6),
             rep("eT", 6),
             rep("PBQC", 10)
             )
  class <- factor(class,
                  levels = c("control", "eT", "eCO2", "eCO2eT", "PBQC")
                  )
  day <- c(rep("day1", 12),
           rep("day14", 10),
           rep("day4", 12),
           rep("day6", 12),
           rep("day1", 12),
           rep("day14", 12),
           rep("day4", 9),
           rep("day6", 12),
           rep("PBQC", 10)
           )
  day <- factor(day,
                levels = c("day1", "day4", "day6", "day14", "PBQC")
                )
  tank <- c(rep(c("L","L","L", "R","R","R"), 2),
            rep(c("L","L","L","R","R","L","L"), 1),
            rep(c("R","R","R","L","L","L"), 10),
            rep(c("L","L","L", "R","R","R"), 2),
            rep("LR", 10)
            )
  tank <- factor(tank,
                 levels = c("L", "R", "LR")
                 )
  rep <- c(rep(c(1, 2, 3), 5),
           rep(c(1, 3, 1, 2), 1),
           rep(c(1, 2, 3), 24),
           c(1, 10, 2:9))
  rep <- factor(rep,
                levels = (c(1:10))
                )
  class_day <- interaction(class,
                           day,
                           drop = TRUE,
                           sep = "."
                           )

  # Impute noise and remove unreliable mass features ---------------------------
  data <- tibble::as_tibble(cbind(class_day, data[-1]))
  data_filt <- metabolomics::MissingValues(data,
                                             column.cutoff = 0.9,
                                             group.cutoff = 0.6,
                                             complete.matrix = FALSE,
                                             seed = seed
                                             )
  data <- data.frame(sample_ids,
                       class,
                       day,
                       tank,
                       rep,
                       data_filt$output
                       )
  percent_na <- round(sum(is.na(data))/prod(dim(data[-1:-6]))*100, 2)

  # Impute remaining missing values --------------------------------------------
  if(parallel) {
    doMC::registerDoMC()
    set.seed(seed)
    data.imp <- missForest::missForest(data[-1], parallelize = "variables")
  }

  else {
    set.seed(seed)
    data.imp <- missForest::missForest(data[-1], parallelize = "no")
  }

  data <- tibble::as_tibble(cbind(sample_ids, data.imp$ximp))
  data <- dplyr::arrange(data, class, day)

  # write data -----------------------------------------------------------------
  if(saverda) {
    devtools::use_data(data)
  }

  if(savecsv) {
    readr::write_csv(data, paste(c("./data/", csvname, ".csv"), collapse = ""))
  }

  message("The data contained ", percent_na, "% NAs")########
  message("MissForest NRMSE: ", round(data.imp$OOBerror, 4))
  data
}
