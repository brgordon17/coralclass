#' Custom theme for ggplot2
#'
#' A custom \code{ggplot2} theme that includes major grid lines on both axes.
#'
#' This theme is based on \code{\link[ggplot2]{theme_light()}} with the
#' following major differences:
#' \itemize{
#' \item axis text to \code{size = 11}
#' \item axis title to \code{size = 14}
#' \item no panel border
#' \item no axis ticks
#' \item plot \code{margin = 11}
#' }
#'
#' @param base_size Base font size
#' @param base_family Base font family
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg))
#' p + theme_brg_grid()
#'
#' @seealso \code{\link[ggplot2]{theme_light()}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
theme_brg_grid <- function(base_size = 12,
                           base_family = ""
                           ) {
  half_line <- base_size / 2
  theme_grey() +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      panel.border = element_blank(),
      panel.grid = element_line(colour = "grey90"),
      panel.grid.major = element_line(colour = "grey90", size = rel(0.75)),
      panel.grid.minor = element_blank(),

      axis.ticks = element_blank(),
      axis.text = element_text(size = 10,
                               colour = "grey50"),
      axis.title.y = element_text(size = 12,
                                  angle = 90,
                                  vjust = 0,
                                  margin = margin(0, half_line, 0, 0)),
      axis.title.x = element_text(size = 12,
                                  vjust = 0,
                                  margin = margin(half_line, 0, 0, 0)),

      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.text = element_text(size = 10),

      strip.background = element_rect(fill = "grey70", colour = NA),
      strip.text = element_text(colour = "white",
                                size = rel(0.8),
                                margin = margin(0.8 * half_line,
                                                0.8 * half_line,
                                                0.8 * half_line,
                                                0.8 * half_line)
                                ),

      plot.margin = margin(2 * half_line, 2 * half_line, half_line, half_line)
    )
}
