#' Important variables figure.
#'
#' \code{figure_vip()} was used to create a composite figure of the important
#' variables for each model
#'
#' \code{figure_vip()} loads each model, determines the important variables
#' using \code{caret::varimp()} and creates a composite figure.
#'
#' @param number The number of important variables to plot from each model
#' @param view.plot Logical indicating if the plot should be printed to the
#' viewer
#' @param save.plot Logical indicating if the plot should be saved to
#' \code{./figs/}.
#' @param plot.name Name of plot.
#'
#' @note Although this function is exported, \code{figure_vip()} was not
#' intended to be used outside of this package.
#'
#' @seealso
#' \code{\link[caret]{varImp}}
#' \code{\link[ggplot2]{ggplot}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
figure_vip <- function(number = 20,
                       view.plot = TRUE,
                       save.plot = FALSE,
                       plot.name = "vip_plot") {

  # Load models ----------------------------------------------------------------
  mzpls_mod <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf_mod <- readRDS("./inst/extdata/mzrf_model.rds")

  # Identify important variables -----------------------------------------------
  # for mzpls
  mzpls_impvars <- varImp(mzpls_mod, scale = TRUE)
  mzpls_impvars <- mzpls_impvars$importance
  mzpls_impvars <- cbind(vip = apply(mzpls_impvars, 1, max), mzpls_impvars)
  mzpls_impvars <- mzpls_impvars[order(-mzpls_impvars$vip), ,drop = FALSE]

  # for mzrf
  mzrf_impvars <- varImp(mzrf_mod, scale = TRUE)
  mzrf_impvars <- mzrf_impvars$importance
  mzrf_impvars <- cbind(vip = apply(mzrf_impvars, 1, max), mzrf_impvars)
  mzrf_impvars <- mzrf_impvars[order(-mzrf_impvars$vip), ,drop = FALSE]

  # clean up dfs and convert to tibbles ----------------------------------------
  # For mzpls
  rownames(mzpls_impvars) <- gsub("mz_", "", rownames(mzpls_impvars))
  mzpls_impvars <- cbind(mz = factor(rownames(mzpls_impvars),
                                     levels = rev(rownames(mzpls_impvars))
  ), mzpls_impvars)
  mzpls_impvars <- tibble::as_tibble(mzpls_impvars[1:number, ], rownames = NULL)

  # For mzrf
  rownames(mzrf_impvars) <- gsub("mz_", "", rownames(mzrf_impvars))
  mzrf_impvars <- cbind(mz = factor(rownames(mzrf_impvars),
                                    levels = rev(rownames(mzrf_impvars))
  ), mzrf_impvars)
  mzrf_impvars <- tibble::as_tibble(mzrf_impvars[1:number, ], rownames = NULL)

  # create plots ---------------------------------------------------------------
  mzpls_plot <- ggplot(mzpls_impvars,
                       aes(x = vip, y = mz)) +
    geom_point(shape = 16,
               colour = phdhelpr::warm_colours[4],
               size = 3) +
    theme(axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 10,
                                     colour = "grey80"),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.4),
          legend.position = "none")

  mzrf_plot <- ggplot(mzrf_impvars,
                      aes(x = vip, y = mz)) +
    geom_point(shape = 16,
               colour = phdhelpr::warm_colours[4],
               size = 3) +
    theme(axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 10,
                                     colour = "grey80"),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90",
                                            size = 0.4),
          legend.position = "none")

  # Create composite plot and axis text grobs ----------------------------------
  mzpls_plot <-
    gridExtra::arrangeGrob(mzpls_plot,
                           top = grid::textGrob("PLS-DA",
                                                x = grid::unit(0.5, "npc"),
                                                y = grid::unit(0.5, "npc"),
                                                gp = grid::gpar(fontsize = 12)))

  mzrf_plot <-
    gridExtra::arrangeGrob(
      mzrf_plot,
      top = grid::textGrob("Random Forests",
                           x = grid::unit(0.5, "npc"),
                           y = grid::unit(0.5, "npc"),
                           gp = grid::gpar(fontsize = 12)))

  xgrob <- grid::textGrob("Variable Importance",
                          x = grid::unit(0.5, "npc"),
                          y = grid::unit(0.5, "npc"),
                          just = c("centre"),
                          gp = grid::gpar(fontsize = 12))

  ygrob <- grid::textGrob("m/z",
                          x = grid::unit(0.5, "npc"),
                          y = grid::unit(0.5, "npc"),
                          just = c("centre"),
                          rot = 90,
                          gp = grid::gpar(fontsize = 12))

  # Create, view, save plot ----------------------------------------------------

  if (view.plot) {
    gridExtra::grid.arrange(mzpls_plot,
                            mzrf_plot,
                            nrow = 1,
                            left = ygrob,
                            bottom = xgrob)
  }

  if (save.plot) {
    grDevices::pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
                   width = 10,
                   height = 5,
                   useDingbats = FALSE)
    gridExtra::grid.arrange(mzpls_plot,
                            mzrf_plot,
                            nrow = 1,
                            left = ygrob,
                            bottom = xgrob)
    grDevices::dev.off()
  }

}
