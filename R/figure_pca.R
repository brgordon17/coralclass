#' PCA compound figure.
#'
#' \code{figure_pca()} reproduces the LCMS and NMR PCA scores compound figure
#' in Benjamin Gordon's PhD thesis.
#'
#' \code{figure_pca()} loads the saved gg objects in \code{./inst/extdata/};
#' extracts the legend from the mzpca plot; adds plot labels (a and b) and saves
#' the plot to \code{./figs/} as a \code{.pdf} file.
#'
#' @param scale Logical indicating if scaling should be applied in PCA
#' @param center Logical indicatinf if the data should be centered
#' @param view.plot Logical indicating if the plot should be printed to the plot
#' viewer.
#' @param save.plot Logical indicating if the plot should be saved to
#' \code{./figs/}.
#' @param plot.name Name of plot.
#' @param seed Integer the sets the state of the random number generator
#'
#' @note Although this function is exported, \code{figure_pca()} was not
#' intended to be used outside of this package.
#'
#' @seealso
#' \code{\link[gridExtra]{gridExtra-package}}
#' \code{\link[ggplot2]{ggplot}}
#' \code{\link[grid]{grid-package}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
figure_pca <- function(scale = FALSE,
                       center = TRUE,
                       view.plot = TRUE,
                       save.plot = FALSE,
                       plot.name = "pca_figure",
                       seed = 1978
                       ) {

  # perform PCAs ---------------------------------------------------------------
  set.seed(seed)
  mzpca <- stats::prcomp(mzdata[-1:-6], scale = scale, center = center)
  set.seed(seed)
  nmrpca <- stats::prcomp(nmrdata[-1:-6], scale = scale, center = center)

  # Define variables for mz PCA plot -------------------------------------------
  mz_exp_var <- summary(mzpca)$importance[2 ,]
  mz_scores <- data.frame(mzdata[, 2:6], mzpca$x)
  mz_x_lab <- paste("PC1", " (", round(mz_exp_var[1] * 100, 2), "%)", sep =  "")
  mz_y_lab <- paste("PC2", " (", round(mz_exp_var[2] * 100, 2), "%)", sep =  "")
  custom_colors <- phdhelpr::qual_colours[c(1:3, 6, 8)]

  # Define variables for nmr PCA plot ------------------------------------------
  nmr_exp_var <- summary(nmrpca)$importance[2 ,]
  nmr_scores <- data.frame(nmrdata[, 2:6], nmrpca$x)
  nmr_x_lab <- paste("PC1", " (", round(nmr_exp_var[1] * 100, 2), "%)", sep =  "")
  nmr_y_lab <- paste("PC2", " (", round(nmr_exp_var[2] * 100, 2), "%)", sep =  "")

  # create mzpca plot ----------------------------------------------------------
  mzpcaplot <-
    ggplot(data = mz_scores,
           aes(x = PC1,
               y = PC2,
               color = class,
               fill = class,
               shape = class)) +
    geom_point(size = 2.5,
               stroke = 0.7,
               position = position_jitter(width = 0.01 * diff(range(mz_scores$PC1)),
                                          height = 0.01 * diff(range(mz_scores$PC2))
               )) +
    labs(x = mz_x_lab, y = mz_y_lab) +
    scale_shape_manual(name = NULL,
                       values = c(21:25)) +
    scale_color_manual(name = NULL,
                       values = grDevices::adjustcolor(custom_colors,
                                                       alpha.f = 0.9)) +
    scale_fill_manual(name = NULL,
                      values = grDevices::adjustcolor(custom_colors,
                                                      alpha.f = 0.5)) +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          axis.text = element_text(colour = "grey70"),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.key = element_blank(),
          legend.position = "bottom")

  # create nmrpca plot ----------------------------------------------------------
  nmrpcaplot <-
    ggplot(data = nmr_scores,
           aes(x = PC1,
               y = PC2,
               color = class,
               fill = class,
               shape = class)) +
    geom_point(size = 2.5,
               stroke = 0.7,
               position = position_jitter(width = 0.01 * diff(range(nmr_scores$PC1)),
                                          height = 0.01 * diff(range(nmr_scores$PC2))
               )) +
    labs(x = nmr_x_lab, y = nmr_y_lab) +
    scale_shape_manual(name = NULL,
                       values = c(21:25)) +
    scale_color_manual(name = NULL,
                       values = grDevices::adjustcolor(custom_colors,
                                                       alpha.f = 0.9)) +
    scale_fill_manual(name = NULL,
                      values = grDevices::adjustcolor(custom_colors,
                                                      alpha.f = 0.5)) +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          axis.text = element_text(colour = "grey70"),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.key = element_blank(),
          legend.position = "bottom")

  # Extract Legend -------------------------------------------------------------
  g_tab <- ggplot_gtable(ggplot_build(mzpcaplot))
  leg <- which(sapply(g_tab$grobs, function(x) x$name) == "guide-box")
  legend <- g_tab$grobs[[leg]]

  # Set corner labels ----------------------------------------------------------
  pca_plot_a <-
    gridExtra::arrangeGrob(mzpcaplot +
                             theme(legend.position = "none"),
                           top = grid::textGrob("a",
                                                x = grid::unit(0.017, "npc"),
                                                y = grid::unit(0.5, "npc"),
                                                just = c("left", "top"),
                                                gp = grid::gpar(fontsize = 16)
                           ))
  pca_plot_b <-
    gridExtra::arrangeGrob(nmrpcaplot +
                             theme(legend.position = "none"),
                           top = grid::textGrob("b",
                                                x = grid::unit(0.014, "npc"),
                                                y = grid::unit(0.5, "npc"),
                                                just = c("left", "top"),
                                                gp = grid::gpar(fontsize = 16)
                           ))

  # Create, view, save plot ----------------------------------------------------
  if (view.plot) {
    gridExtra::grid.arrange(gridExtra::arrangeGrob(pca_plot_a,
                                                   pca_plot_b,
                                                   nrow = 1),
                            legend,
                            nrow = 2,
                            heights = c(12, 1))
  }

  if (save.plot) {
    grDevices::pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
                   width = 10,
                   height = 5,
                   useDingbats = FALSE)
    gridExtra::grid.arrange(gridExtra::arrangeGrob(pca_plot_a,
                                                   pca_plot_b,
                                                   nrow = 1),
                            legend,
                            nrow = 2,
                            heights = c(12, 1))
    grDevices::dev.off()
  }

}
