#' Plot Shapes
#'
#' Shapes used by plots in the gordon01 package.
#'
#' The function \code{plot_shapes()} returns a vector of shape IDs for use in
#' \code{ggplot}. The shapes used are shapes 21:25 that have a \code{fill}
#' option
#'
#' @param shape The shape required e.g. \code{"triangle"}. \code{NULL} returns
#' all shapes.
#'
#' @return The default \code{NULL} returns a named numeric vector of
#' shapes 21:25.
#'
#' @note Although this function is exported, \code{plot_shapes()} was not
#' intended to be used outside of this package.
#'
#' @seealso
#' \href{http://sape.inf.usi.ch/quick-reference/ggplot2/shape}{Sape research group)}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'

plot_shapes <- function(shape = NULL) {
  shape_vector <- c("circle" = 21,
                    "square" = 22,
                    "diamond" = 23,
                    "triangle" = 24,
                    "nabla" = 25)
  if(is.null(shape)) {
    shape_vector
  }

  else {
    shape_vector[shape]
  }
}
