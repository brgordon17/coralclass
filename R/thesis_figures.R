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
#' @export
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

#' Temporal misclassifications figure.
#'
#' \code{figure_misclass_temporal()} reproduces the compound figure of the LCMS
#' and NMR model temporal misclassification results in section x.xx of Benjamin
#' Gordon's PhD thesis.
#'
#' \code{figure_misclass_temporal()} loads the models that were saved to
#' \code{./inst/extdata/}; reproduces the training data; ascertains the temporal
#' information for each misclassification; summarises the misclassifications
#' according to the temporal data; creates side-by-side bar plots and places
#' them, with an appropriate plot title, into a single compound figure. The
#' figure can be saved to \code{./figs/} if required.
#'
#' @param view.plot Logical indicating if the plot should be printed to the plot
#' viewer.
#' @param save.plot Logical indicating if the plot should be saved to
#' \code{./figs/}.
#' @param plot.name Name of plot.
#'
#' @note Although this function is exported, \code{figure_misclass_temporal()}
#' was not intended to be used outside of this package.
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
figure_misclass_temporal <- function(view.plot = TRUE,
                                     save.plot = FALSE,
                                     plot.name = "temporal_misclass_figure") {

  # load models ----------------------------------------------------------------
  mzpls_mod <- readRDS("./inst/extdata/mzpls_model.rds")
  mzrf_mod <- readRDS("./inst/extdata/mzrf_model.rds")
  nmrpls_mod <- readRDS("./inst/extdata/nmrpls_model.rds")
  nmrrf_mod <- readRDS("./inst/extdata/nmrrf_model.rds")

  # Reproduce training data ----------------------------------------------------
  lcmsdata <- data.frame(mzdata[-which(mzdata$class == "PBQC"), ])
  lcmsdata <- droplevels(lcmsdata)

  set.seed(1978)
  mzindex <- caret::createDataPartition(lcmsdata$class_day,
                                        p = .8,
                                        list = FALSE,
                                        times = 1
  )
  set.seed(1978)
  nmrindex <- caret::createDataPartition(nmrdata$class_day,
                                         p = .8,
                                         list = FALSE,
                                         times = 1)

  mztrain  <- lcmsdata[mzindex, ]
  nmrtrain  <- data.frame(nmrdata[nmrindex, ], check.names = FALSE)

  # Temporal information for each misclassification ----------------------------
  mzpls_false_preds <- mzpls_mod$pred
  mzpls_false_preds$obs_day <- mztrain["day"][mzpls_false_preds$rowIndex ,]
  mzpls_false_preds <-
    mzpls_false_preds[mzpls_false_preds$ncomp == mzpls_mod$bestTune$ncomp ,]
  mzpls_false_preds <-
    mzpls_false_preds[mzpls_false_preds$pred != mzpls_false_preds$obs ,]

  mzrf_false_preds <- mzrf_mod$pred
  mzrf_false_preds$obs_day <- mztrain["day"][mzrf_false_preds$rowIndex ,]
  mzrf_false_preds <-
    mzrf_false_preds[mzrf_false_preds$mtry == mzrf_mod$bestTune$mtry ,]
  mzrf_false_preds <-
    mzrf_false_preds[mzrf_false_preds$pred != mzrf_false_preds$obs ,]

  nmrpls_false_preds <- nmrpls_mod$pred
  nmrpls_false_preds$obs_day <- nmrtrain["day"][nmrpls_false_preds$rowIndex ,]
  nmrpls_false_preds <-
    nmrpls_false_preds[nmrpls_false_preds$ncomp == nmrpls_mod$bestTune$ncomp ,]
  nmrpls_false_preds <-
    nmrpls_false_preds[nmrpls_false_preds$pred != nmrpls_false_preds$obs ,]

  nmrrf_false_preds <- nmrrf_mod$pred
  nmrrf_false_preds$obs_day <- nmrtrain["day"][nmrrf_false_preds$rowIndex ,]
  nmrrf_false_preds <-
    nmrrf_false_preds[nmrrf_false_preds$mtry == nmrrf_mod$bestTune$mtry ,]
  nmrrf_false_preds <-
    nmrrf_false_preds[nmrrf_false_preds$pred != nmrrf_false_preds$obs ,]

  # Summarise the proportion of all misclassification according to day ---------
  mzpls_prop <-
    group_by(mzpls_false_preds,
             obs,
             obs_day) %>%
    summarize(n = length(obs)) %>%
    mutate(Proportion = round(n / sum(n), 2)) %>%
    ungroup()
  colnames(mzpls_prop)[1] <- "Observation"
  colnames(mzpls_prop)[2] <- "Day"
  mzpls_prop$Observation2 <- # factor with plotmath labels for subscripts
    factor(mzpls_prop$Observation,
           labels = c("control",
                      "eT",
                      "eCO[2]",
                      "eCO[2]*eT"))

  mzrf_prop <-
    group_by(mzrf_false_preds,
             obs,
             obs_day) %>%
    summarize(n = length(obs)) %>%
    mutate(Proportion = round(n / sum(n), 2)) %>%
    ungroup()
  colnames(mzrf_prop)[1] <- "Observation"
  colnames(mzrf_prop)[2] <- "Day"
  mzrf_prop$Observation2 <- # factor with plotmath labels for subscripts
    factor(mzrf_prop$Observation,
           labels = c("control",
                      "eT",
                      "eCO[2]",
                      "eCO[2]*eT"))

  nmrpls_prop <-
    group_by(nmrpls_false_preds,
             obs,
             obs_day) %>%
    summarize(n = length(obs)) %>%
    mutate(Proportion = round(n / sum(n), 2)) %>%
    ungroup()
  colnames(nmrpls_prop)[1] <- "Observation"
  colnames(nmrpls_prop)[2] <- "Day"
  nmrpls_prop$Observation2 <- # factor with plotmath labels for subscripts
    factor(nmrpls_prop$Observation,
           labels = c("control",
                      "eT",
                      "eCO[2]",
                      "eCO[2]*eT"))

  nmrrf_prop <-
    group_by(nmrrf_false_preds,
             obs,
             obs_day) %>%
    summarize(n = length(obs)) %>%
    mutate(Proportion = round(n / sum(n), 2)) %>%
    ungroup()
  colnames(nmrrf_prop)[1] <- "Observation"
  colnames(nmrrf_prop)[2] <- "Day"
  nmrrf_prop$Observation2 <- # factor with plotmath labels for subscripts
    factor(nmrrf_prop$Observation,
           labels = c("control",
                      "eT",
                      "eCO[2]",
                      "eCO[2]*eT"))

  # Create side-by-side bar plots -----------------------------------------------
  mzpls_plot <- ggplot(mzpls_prop,
                       aes(x = Day, y = Proportion, fill = Day)) +
    geom_col() +
    facet_wrap( ~ Observation2, nrow = 1, labeller = label_parsed) +
    scale_fill_manual(values = qual_colours[c(1:3, 7)], guide = "none") +
    scale_x_discrete(name = NULL,
                     labels = c("1", "4", "6", "14")) +
    scale_y_continuous(name = NULL,
                       expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "grey65"),
          axis.title = element_text(size = 12),
          axis.ticks = element_blank()
          )

  mzrf_plot <- ggplot(mzrf_prop,
                      aes(x = Day, y = Proportion, fill = Day)) +
    geom_col() +
    facet_wrap( ~ Observation2, nrow = 1, labeller = label_parsed) +
    scale_fill_manual(values = qual_colours[c(1:3, 7)], guide = "none") +
    scale_x_discrete(name = NULL,
                     labels = c("1", "4", "6", "14")) +
    scale_y_continuous(name = NULL,
                       expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "grey65"),
          axis.title = element_text(size = 12),
          axis.ticks = element_blank()
    )

  nmrpls_plot <- ggplot(nmrpls_prop,
                        aes(x = Day, y = Proportion, fill = Day)) +
    geom_col() +
    facet_wrap( ~ Observation2, nrow = 1, labeller = label_parsed) +
    scale_fill_manual(values = qual_colours[c(1:3, 7)], guide = "none") +
    scale_x_discrete(name = NULL,
                     labels = c("1", "4", "6", "14")) +
    scale_y_continuous(name = NULL,
                       expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "grey65"),
          axis.title = element_text(size = 12),
          axis.ticks = element_blank()
    )

  nmrrf_plot <- ggplot(nmrrf_prop,
                       aes(x = Day, y = Proportion, fill = Day)) +
    geom_col() +
    facet_wrap( ~ Observation2, nrow = 1, labeller = label_parsed) +
    scale_fill_manual(values = qual_colours[c(1:3, 7)], guide = "none") +
    scale_x_discrete(name = NULL,
                     labels = c("1", "4", "6", "14")) +
    scale_y_continuous(name = NULL,
                       expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10, colour = "grey65"),
          axis.title = element_text(size = 12),
          axis.ticks = element_blank()
    )

  # Set corner labels ----------------------------------------------------------
  mzpls_plot_a <-
    gridExtra::arrangeGrob(
      mzpls_plot,
      top = grid::textGrob("LCMS PLS-DA",
                           x = grid::unit(0.5, "npc"),
                           y = grid::unit(0.5, "npc"),
                           gp = grid::gpar(fontsize = 12)
      ))

  nmrpls_plot_b <-
    gridExtra::arrangeGrob(
      nmrpls_plot,
      top = grid::textGrob("NMR PLS-DA",
                           x = grid::unit(0.5, "npc"),
                           y = grid::unit(0.5, "npc"),
                           gp = grid::gpar(fontsize = 12)
      ))

  mzrf_plot_c <-
    gridExtra::arrangeGrob(
      mzrf_plot,
      top = grid::textGrob("LCMS Random Forests",
                           x = grid::unit(0.5, "npc"),
                           y = grid::unit(0.5, "npc"),
                           gp = grid::gpar(fontsize = 12)
      ))

  nmrrf_plot_d <-
    gridExtra::arrangeGrob(
      nmrrf_plot,
      top = grid::textGrob("NMR Random Forests",
                           x = grid::unit(0.5, "npc"),
                           y = grid::unit(0.5, "npc"),
                           gp = grid::gpar(fontsize = 12)
      ))

  # create x- and y-axis text grobs --------------------------------------------
  xgrob <- grid::textGrob("Days of Exposure",
                          x = grid::unit(0.5, "npc"),
                          y = grid::unit(0.5, "npc"),
                          just = c("centre"),
                          gp = grid::gpar(fontsize = 12)
                          )

  ygrob <- grid::textGrob("Proportion of Misclassified Samples",
                          x = grid::unit(0.5, "npc"),
                          y = grid::unit(0.5, "npc"),
                          just = c("centre"),
                          rot = 90,
                          gp = grid::gpar(fontsize = 12)
  )

  # Create, view, save plot ----------------------------------------------------

  if (view.plot) {
    gridExtra::grid.arrange(mzpls_plot_a,
                            nmrpls_plot_b,
                            mzrf_plot_c,
                            nmrrf_plot_d,
                            nrow = 2,
                            left = ygrob,
                            bottom = xgrob)
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
                            nrow = 2,
                            left = ygrob,
                            bottom = xgrob)
    grDevices::dev.off()
  }

}

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
  mzpls_impvars <- as_tibble(mzpls_impvars[1:number, ], rownames = NULL)

  # For mzrf
  rownames(mzrf_impvars) <- gsub("mz_", "", rownames(mzrf_impvars))
  mzrf_impvars <- cbind(mz = factor(rownames(mzrf_impvars),
                                     levels = rev(rownames(mzrf_impvars))
  ), mzrf_impvars)
  mzrf_impvars <- as_tibble(mzrf_impvars[1:number, ], rownames = NULL)

  # create plots ---------------------------------------------------------------
  mzpls_plot <- ggplot(mzpls_impvars,
                       aes(x = vip, y = mz)) +
    geom_point(shape = 16,
               colour = seq_colours[4],
               size = 3) +
    theme(axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey75",
                                            size = 0.2,
                                            linetype = "dashed"),
          legend.position = "none")

  mzrf_plot <- ggplot(mzrf_impvars,
                       aes(x = vip, y = mz)) +
    geom_point(shape = 16,
               colour = seq_colours[4],
               size = 3) +
    theme(axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey75",
                                            size = 0.2,
                                            linetype = "dashed"),
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

  xgrob <- grid::textGrob("VIP Scores",
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
