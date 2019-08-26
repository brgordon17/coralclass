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

  # Load models ----------------------------------------------------------------
  mzpls <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf <- readRDS("./inst/extdata/mzrf_model.rds")
  nmrpls <- readRDS("./inst/extdata/nmrpls_model.rds")
  nmrrf <- readRDS("./inst/extdata/nmrrf_model.rds")

  # Plotting -------------------------------------------------------------------
  custom_colours <- phdhelpr::warm_colours[4] # red

  mzpls_plot <- ggplot(mzpls$results,
                       aes(x = ncomp, y = Accuracy)) +
    geom_line(colour = custom_colours,
              size = 1) +
    geom_point(aes(x = ncomp[ncomp == 16],
                   y = Accuracy[ncomp == 16]),
               colour = custom_colours,
               size = 3) +
    annotate("text",
             x = 16,
             y = 0.8428571 + 0.15,
             label = "LCMS PLS-DA") +
    annotate("text",
             x = 16,
             y = 0.8428571 + 0.08,
             label = "Best Tune (16, 0.84)") +
    scale_x_continuous(limits = c(1, 50),
                       expand = waiver(),
                       name = "Latent Variables") +
    scale_y_continuous(limits = c(0, 1),
                       expand = waiver()) +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.6),
          axis.title = element_text(size = 14),
          axis.text = element_text(colour = "grey65"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(colour = "grey90",
                                      size = 0.6),
          axis.line.x = element_blank())

  nmrpls_plot <- ggplot(nmrpls$results,
                       aes(x = ncomp, y = Accuracy)) +
    geom_line(colour = custom_colours,
              size = 1) +
    geom_point(aes(x = ncomp[ncomp == 21],
                   y = Accuracy[ncomp == 21]),
               colour = custom_colours,
               size = 3) +
    annotate("text",
             x = 21,
             y = 0.7708333 + 0.15,
             label = "NMR PLS-DA") +
    annotate("text",
             x = 21,
             y = 0.7708333 + 0.08,
             label = "Best Tune (21, 0.77)") +
    scale_x_continuous(limits = c(1, 50),
                       expand = waiver(),
                       name = "Latent Variables") +
    scale_y_continuous(limits = c(0, 1),
                       expand = waiver()) +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.6),
          axis.title = element_text(size = 14),
          axis.text = element_text(colour = "grey65"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(colour = "grey90",
                                      size = 0.6),
          axis.line.x = element_blank())

  mzrf_plot <- ggplot(mzrf$results,
                      aes(x = mtry,
                          y = Accuracy)) +
    geom_line(colour = custom_colours,
              size = 1) +
    geom_point(aes(x = mtry[mtry == 350],
                   y = Accuracy[mtry == 350]),
               colour = custom_colours,
               size = 3) +
    annotate("text",
             x = 350,
             y = 0.8428571 + 0.15,
             label = "LCMS RF") +
    annotate("text",
             x = 350,
             y = 0.8426587 + 0.08,
             label = "Best Tune (350, 0.84)") +
    scale_x_continuous(limits = c(10, 550),
                       expand = c(0, 0),
                       name = "mtry") +
    scale_y_continuous(limits = c(0, 1),
                       expand = waiver()) +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.6),
          axis.title = element_text(size = 14),
          axis.text = element_text(colour = "grey65"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(colour = "grey90",
                                      size = 0.6),
          axis.line.x = element_blank())

  nmrrf_plot <- ggplot(nmrrf$results,
                      aes(x = mtry,
                          y = Accuracy)) +
    geom_line(colour = custom_colours,
              size = 1) +
    geom_point(aes(x = mtry[mtry == 25],
                   y = Accuracy[mtry == 25]),
               colour = custom_colours,
               size = 3) +
    annotate("text",
             x = 25 + 75,
             y = 0.6125000 + 0.15,
             label = "NMR RF") +
    annotate("text",
             x = 25 + 75,
             y = 0.6125000 + 0.08,
             label = "Best Tune (25, 0.61)") +
    scale_x_continuous(limits = c(10, 550),
                       expand = c(0, 0),
                       name = "mtry") +
    scale_y_continuous(limits = c(0, 1),
                       expand = waiver()) +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.6),
          axis.title = element_text(size = 14),
          axis.text = element_text(colour = "grey65"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(colour = "grey90",
                                      size = 0.6),
          axis.line.x = element_blank())

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
