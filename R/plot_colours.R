#' Qualitative Colours
#'
#' Custom qualitative, colour blind safe colours used by plots in the
#' gordon01 package.
#'
#' The function \code{qual_colours()} returns the colour blind safe Okabe Ito
#' qualitative colour scale for distinguishing discrete groups.
#'
#' @param colour The colour required e.g. \code{"orange"}. \code{NULL} returns
#' all colours.
#'
#' @return The default \code{NULL} returns a named character vector of all
#' colours available.
#'
#' @note Although this function is exported, \code{qual_colours()} was not
#' intended to be used outside of this package.
#'
#' @examples
#' qual_colours()
#'
#' @seealso \href{http://serialmentor.com/dataviz/color-pitfalls.html#not-designing-for-color-vision-deficiency}{serialmentor.com}
#' and \href{http://jfly.iam.u-tokyo.ac.jp/color/}{Okabe and Ito (2008)}
#'
#' @export
#'

qual_colours <- function(colour = NULL) {
  col_vector <- c("orange" = "#E69F00",
                  "sky blue" = "#56B4E9",
                  "blue green" = "#009E73",
                  "yellow" = "#F0E442",
                  "blue" = "#0072B2",
                  "vermillion" = "#D55E00",
                  "reddish purple" = "#CC79A7",
                  "grey" = "#999999"
                  )
  if(is.null(colour)) {
    col_vector
  }

  else {
    col_vector[colour]
  }
}

#' Sarah and Duck Colours
#'
#' Colours from Sarah and Duck.
#'
#' The function \code{sd_colours()} returns a named vector of colours from the
#' Sarah and Duck cartoon.
#'
#' @param colour The colour required e.g. \code{"pink"}. \code{NULL} returns
#' all colours.
#'
#' @return The default \code{NULL} returns a named vector of all colours in the
#' palette.
#'
#' @note Although this function is exported, \code{sd_colours()} was not
#' intended to be used outside of this package.
#'
#' @examples
#' sd_colours()
#'
#' @seealso
#' \href{https://www.sarahandduck.com}{sarahandduck.com}
#'
#' @export
#'
sd_colours <- function(colour = NULL) {
  col_vector <- c("pastel green" = "#73BE97",
                  "brown" = "#595653",
                  "green" = "#3CB54C",
                  "pink" = "#E9898D",
                  "sand" = "#D2CA65"
                  )
  if(is.null(colour)) {
    col_vector
  }

  else {
    col_vector[colour]
  }
}

#' Yellow Orange Red Sequential colours
#'
#' Five sequential colours from the YlOrRd palette at colorbrewer.org.
#'
#' The function \code{()} returns a named vector of sequential colours from the
#' YlOrRd palette at colorbrewer.org.
#'
#' @param colour The colour required e.g. \code{"red"}. \code{NULL} returns
#' all colours.
#'
#' @return The default \code{NULL} returns a named vector of all colours in the
#' palette.
#'
#' @note Although this function is exported, \code{seq_colours()} was not
#' intended to be used outside of this package.
#'
#' @examples
#' seq_colours()
#'
#' @seealso
#' \href{http://colorbrewer2.org/#type=sequential&scheme=YlOrRd&n=5}{colorbrewer.org}
#'
#' @export
#'
seq_colours <- function(colour = NULL) {
  col_vector <- c("yellow" = "#ffffb2",
                  "light orange" = "#fecc5c",
                  "orange" = "#fd8d3c",
                  "red" = "#f03b20",
                  "crimson" = "#bd0026"
  )
  if(is.null(colour)) {
    col_vector
  }

  else {
    col_vector[colour]
  }
}
