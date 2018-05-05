#' Feature detection using xcms.
#'
#' Method used by Gordon et. al. to extract LCMS features from
#' .mzXML files using XCMS.
#'
#' This function was used to extract features from .mzXML data. Values used
#' within the function were optimised to suit our instrument and R environment.
#' The function creates an xcmsSet object, groups peaks, corrects for retention
#' time drift, re-groups the peaks, fills missing peaks, annotates adducts and
#' isotopes, before saving the output to a .csv file in /data-raw.
#'
#' @param saveoutput Logical indicating if output should be saved
#' @param outputname The name of the output file to be saved if TRUE
#' @param ... Other arguments passed on to individual methods
#'
#' @return Returns a dataframe of class tbl_df
#'
#' @note This function was not intended to be used for any other data without
#' modification to the underlying code.
#'
#' @examples
#' mzdata_raw()
#'
#' @seealso \code{\link[xcms]{xcmsPreprocess}}
#'
mzdata_raw <- function(saveoutput = FALSE,
                       outputname = "mzdata-raw",
                       ...) {
  files <- list.files(recursive = TRUE)
  xset <- xcms::xcmsSet(files,
                        method = "centWave",
                        ppm = 30,
                        snthr = 10,
                        peakwidth = c(20,60),
                        mzdiff = 0.01,
                        integrate = 2,
                        prefilter = c(3,1100)
                        )
  # Group peaks
  xsetgr1 <- xcms::group(xset,
                         bw = 5,
                         minfrac = 0.5,
                         minsamp = 1,
                         mzwid = 0.05,
                         max = 100
                         )
  # Correct RT
  xsetcor <- xcms::retcor(xsetgr1,
                          method = "obiwarp",
                          plottype = "none",
                          profStep = 0.5
                          )
  # Regroup peaks
  xsetgr2 <- xcms::group(xsetcor,
                         bw = 5,
                         minfrac = 0.5,
                         minsamp = 1,
                         mzwid= 0.05,
                         max = 100
                         )
  # Fill missing peaks
  xsetmiss <- xcms::fillPeaks(xsetgr2)
  # Identify and annotate adducts
  xsetadd <- CAMERA::annotate(xsetmiss,
                              perfwhm = 0.7,
                              cor_eic_th = 0.75,
                              ppm = 10,
                              polarity = "positive"
                              )
  # Get features
  features <- tibble::as_tibble(CAMERA::getPeaklist(xsetadd))
  # Save output
  if (saveoutput) {
    readr::write_csv(features,
                     paste(c(outputname, ".csv"),
                           collapse = ""))
  }
features
}
