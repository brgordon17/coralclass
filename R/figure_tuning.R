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
      top = grid::textGrob("a",
                           x = grid::unit(0.017, "npc"),
                           y = grid::unit(0.5, "npc"),
                           just = c("left", "top"),
                           gp = grid::gpar(fontsize = 16)
      ))

  nmrpls_plot_b <-
    gridExtra::arrangeGrob(
      nmrpls_plot,
      top = grid::textGrob("b",
                           x = grid::unit(0.017, "npc"),
                           y = grid::unit(0.5, "npc"),
                           just = c("left", "top"),
                           gp = grid::gpar(fontsize = 16)
      ))

  mzrf_plot_c <-
    gridExtra::arrangeGrob(
      mzrf_plot,
      top = grid::textGrob("c",
                           x = grid::unit(0.017, "npc"),
                           y = grid::unit(0.5, "npc"),
                           just = c("left", "top"),
                           gp = grid::gpar(fontsize = 16)
      ))

  nmrrf_plot_d <-
    gridExtra::arrangeGrob(
      nmrrf_plot,
      top = grid::textGrob("d",
                           x = grid::unit(0.017, "npc"),
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
    grDevices::pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
                   width = 10,
                   height = 8,
                   useDingbats = FALSE)
    gridExtra::grid.arrange(mzpls_plot_a,
                            nmrpls_plot_b,
                            mzrf_plot_c,
                            nmrrf_plot_d,
                            nrow = 2)
    grDevices::dev.off()
  }

}
