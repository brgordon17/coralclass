#' Plot of Photosynthetic Yield (FvFm).
#'
#' \code{figure_fvfm()} produces the photosynthetic yield plot in section 4 of
#' Benjamin Gordon's PhD thesis.
#'
#' \code{figure_fvfm()} plots the PAM data located in \code{./data}.
#' The figure can be saved to \code{./figs/} if required.
#'
#' @param save.plot Logical indicating if the plot should be saved to
#' \code{./figs/}.
#' @param plot.name Name of plot.
#'
#' @note Although this function is exported, \code{figure_fvfm()}
#' was not intended to be used outside of this package.
#'
#' @seealso
#' \code{\link[ggplot2]{ggplot}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
figure_fvfm <- function(save.plot = FALSE,
                        plot.name = "fvfm_plot") {

  # load data
  load("./data/pamdata.rda")

  # plot
  fvfm_plot <- ggplot(pamdata, aes(x = day,
                                   y = FvFm,
                                   colour = class,
                                   shape = class,
                                   fill = class)) +
    geom_point(size = 3, show.legend = TRUE) +
    geom_path() +
    scale_x_continuous(name = "Time (days)",
                       labels = c(1:13),
                       breaks = c(1:13)) +
    scale_y_continuous(name = expression(Quantum~Yield~PSII~(F[v]/F[m]))) +
    scale_colour_manual(values = phdhelpr::qual_colours[c(3, 2, 1, 6)],
                        name = NULL,
                        labels = c("control", expression(eCO[2]), "eT",
                                   expression(eCO[2]*eT))) +
    scale_shape_manual(values = c(21:24),
                       name = NULL,
                       labels = c("control", expression(eCO[2]), "eT",
                                  expression(eCO[2]*eT))) +
    scale_fill_manual(values = grDevices::adjustcolor(phdhelpr::qual_colours[c(3, 2, 1, 6)],
                                                      alpha.f = 0.5),
                      name = NULL,
                      labels = c("control", expression(eCO[2]), "eT",
                                 expression(eCO[2]*eT))) +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.6),
          axis.text = element_text(size = 10, colour = "grey65"),
          axis.title = element_text(size = 14),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 14),
          legend.text.align = 0
    )

  # View or save plot ----------------------------------------------------------
  if (save.plot) {
    grDevices::pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
                   width = 10,
                   height = 5,
                   useDingbats = FALSE)
    fvfm_plot
    grDevices::dev.off()
  }

  fvfm_plot
}
