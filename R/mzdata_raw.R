#' Feature detection using xcms.
#'
#' Method used by Gordon et. al. to extract LCMS features from
#' \code{.mzXML} files using \code{xcms}.
#'
#' This function was used to extract features from \code{.mzXML} data. The
#' function creates an \code{xcmsSet} object, groups peaks, corrects for
#' retention time drift, re-groups the peaks, fills missing peaks, annotates
#' adducts and isotopes, before saving the output to a \code{.csv} file in
#' \code{/data-raw}.
#'
#' @param saveoutput Logical indicating if output should be saved
#' @param outputname The name of the output file to be saved if \code{TRUE}
#' @param ... Other arguments passed on to individual methods
#'
#' @return Returns a dataframe of class \code{tbl_df}
#'
#' @note \code{mzdata_raw()} was not intended to be used outside of this
#' package. Please use \code{gordon01:::mzdata_raw()} to run this function after
#' installing the package. The function utilises as small subset of out
#' \code{.mzXML} data only. The full \code{.mzXML} data set is available from
#' the package author.
#'
#' @examples
#' gordon01:::mzdata_raw(saveoutput = FALSE, outputname = "example-mzdata-raw")
#'
#' @seealso \code{\link[xcms]{xcmsSet}}
#'
mzdata_raw <- function(saveoutput = FALSE,
                       outputname = "example-mzdata-raw",
                       ...) {

  library(tidyverse)
  library(xcms)
  library(CAMERA)

  files <- list.files(path = "./data-raw/example_mzxml_data",
                      full.names = TRUE,
                      recursive = TRUE)

  xset <- xcmsSet(files,
                  method = "centWave",
                  ppm = 30,
                  snthr = 10,
                  peakwidth = c(20,60),
                  mzdiff = 0.01,
                  integrate = 2,
                  prefilter = c(3,1100)
                  )

  # Group peaks
  xsetgr1 <- group(xset,
                   bw = 5,
                   minfrac = 0.5,
                   minsamp = 1,
                   mzwid = 0.05,
                   max = 100
                   )

  # Correct RT
  xsetcor <- retcor(xsetgr1,
                    method = "obiwarp",
                    plottype = "none",
                    profStep = 0.5
                    )

  # Regroup peaks
  xsetgr2 <- group(xsetcor,
                   bw = 5,
                   minfrac = 0.5,
                   minsamp = 1,
                   mzwid= 0.05,
                   max = 100
                   )

  # Fill missing peaks
  xsetmiss <- fillPeaks(xsetgr2)

  # Identify and annotate adducts
  xsetadd <- annotate(xsetmiss,
                      perfwhm = 0.7,
                      cor_eic_th = 0.75,
                      ppm = 10,
                      polarity = "positive"
                      )

  # Get features
  features <- as_tibble(getPeaklist(xsetadd))

  # Save output
  if (saveoutput) {
    write_csv(features, paste(c("./data-raw/",outputname, ".csv"),
                              collapse = ""))
  }

  features
}
