#' PCA compound figure.
#'
#' \code{figure_pca()} reproduces the LCMS and NMR PCA scores compound figure
#' in section x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{figure_pca()} loads the saved gg objects in \code{./inst/extdata/};
#' extracts the legend from the mzpca plot; adds plot labels (a and b) and saves
#' the plot to \code{./figs/} as a \code{.pdf} file.
#'
#' @param view.plot Logical indicating if the plot should be printed to the plot
#' viewer.
#' @param save.plot Logical indicating if the plot should be saved to
#' \code{./figs/}.
#' @param plot.name Name of plot.
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
#' @import ggplot2
#'
#' @export
#'
#'
figure_pca <- function(view.plot = TRUE,
                       save.plot = FALSE,
                       plot.name = "pca_figure") {

  # Load pca gg objects and modify ---------------------------------------------
  mzpca_plot <- readRDS("./inst/extdata/mzpca_ggobject.rds")
  nmrpca_plot <- readRDS("./inst/extdata/nmrpca_ggobject.rds")
  mzpca_plot <- mzpca_plot +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 10,
                                   colour = "grey70"))
  nmrpca_plot <- nmrpca_plot +
    theme(axis.text = element_text(size = 10,
                                   colour = "grey70"))

  # Extract Legend -------------------------------------------------------------
  g_tab <- ggplot_gtable(ggplot_build(mzpca_plot))
  leg <- which(sapply(g_tab$grobs, function(x) x$name) == "guide-box")
  legend <- g_tab$grobs[[leg]]

  # Set corner labels ----------------------------------------------------------
  pca_plot_a <-
    gridExtra::arrangeGrob(mzpca_plot +
                             theme(legend.position = "none"),
                             top = grid::textGrob("a",
                                                x = grid::unit(0.017, "npc"),
                                                y = grid::unit(0.5, "npc"),
                                                just = c("left", "top"),
                                                gp = grid::gpar(fontsize = 16)
                                                ))
  pca_plot_b <-
    gridExtra::arrangeGrob(nmrpca_plot +
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
    pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
        width = 10,
        height = 5,
        useDingbats = FALSE)
    gridExtra::grid.arrange(gridExtra::arrangeGrob(pca_plot_a,
                                                   pca_plot_b,
                                                   nrow = 1),
                            legend,
                            nrow = 2,
                            heights = c(12, 1))
    dev.off()
  }

}

#' Model tuning figure.
#'
#' \code{figure_tuning()} reproduces the compound figure of the LCMS and NMR
#' model tuning results in section x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{figure_tuning()} loads gg objects that were saved to
#' \code{./inst/extdata/} by the four modelling functions; adds plot labels
#' (a, b, c and d) and saves the plot to \code{./figs/} as a \code{.pdf} file.
#'
#' @param view.plot Logical indicating if the plot should be printed to the plot
#' viewer.
#' @param save.plot Logical indicating if the plot should be saved to
#' \code{./figs/}.
#' @param plot.name Name of plot.
#'
#' @note Although this function is exported, \code{figure_tuning()} was not
#' intended to be used outside of this package.
#'
#' @seealso
#' \code{\link[gridExtra]{gridExtra-package}}
#' \code{\link[ggplot2]{ggplot}}
#' \code{\link[grid]{grid-package}}
#'
#' @author Benjamin R. Gordon
#'
#' @import ggplot2
#'
#' @export
#'
figure_tuning <- function(view.plot = TRUE,
                          save.plot = FALSE,
                          plot.name = "tuning_figure") {

  # Load gg objects ------------------------------------------------------------
  mzpls_plot <- readRDS("./inst/extdata/mzpls_tune_ggobject.rds")
  mzrf_plot <- readRDS("./inst/extdata/mzrf_tune_ggobject.rds")
  nmrpls_plot <- readRDS("./inst/extdata/nmrpls_tune_ggobject.rds")
  nmrrf_plot <- readRDS("./inst/extdata/nmrrf_tune_ggobject.rds")

  # Set corner labels ----------------------------------------------------------
  mzpls_plot_a <-
    gridExtra::arrangeGrob(
      mzpls_plot,
      top = grid::textGrob("a", x = unit(0.017, "npc"),
                           y = grid::unit(0.5, "npc"),
                           just = c("left", "top"),
                           gp = grid::gpar(fontsize = 16)
                           ))

  nmrpls_plot_b <-
    gridExtra::arrangeGrob(
      nmrpls_plot,
      top = grid::textGrob("b", x = unit(0.017, "npc"),
                           y = grid::unit(0.5, "npc"),
                           just = c("left", "top"),
                           gp = grid::gpar(fontsize = 16)
      ))

  mzrf_plot_c <-
    gridExtra::arrangeGrob(
      mzrf_plot,
      top = grid::textGrob("c", x = unit(0.017, "npc"),
                           y = grid::unit(0.5, "npc"),
                           just = c("left", "top"),
                           gp = grid::gpar(fontsize = 16)
      ))

  nmrrf_plot_d <-
    gridExtra::arrangeGrob(
      nmrrf_plot,
      top = grid::textGrob("d", x = unit(0.017, "npc"),
                           y = grid::unit(0.5, "npc"),
                           just = c("left", "top"),
                           gp = grid::gpar(fontsize = 16)
      ))

  # Create, view, save plot ----------------------------------------------------

  if (view.plot) {
    gridExtra::grid.arrange(mzpls_plot_a,
                            nmrpls_plot_b,
                            mzrf_plot_c,
                            nmrrf_plot_d,
                            nrow = 2)
  }

  if (save.plot) {
    pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
        width = 10,
        height = 8,
        useDingbats = FALSE)
    gridExtra::grid.arrange(mzpls_plot_a,
                            nmrpls_plot_b,
                            mzrf_plot_c,
                            nmrrf_plot_d,
                            nrow = 2)
    dev.off()
  }

}
