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

  # Plots ----------------------------------------------------------------------
  mzpls_plot <- ggplot(mzpls_prop,
                       aes(x = Observation2, y = Proportion, fill = Day)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Reds")[2:5],
                      guide = "none") +
    scale_x_discrete(labels = parse(text = levels(mzpls_prop$Observation2))) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(axis.text = element_text(size = 12, colour = "grey65"),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank()
    )

  mzrf_plot <- ggplot(mzrf_prop,
                      aes(x = Observation2, y = Proportion, fill = Day)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Reds")[2:5],
                      guide = "none") +
    scale_x_discrete(labels = parse(text = levels(mzrf_prop$Observation2))) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(axis.text = element_text(size = 12, colour = "grey65"),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank()
    )

  nmrpls_plot <- ggplot(nmrpls_prop,
                        aes(x = Observation2, y = Proportion, fill = Day)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Reds")[2:5],
                      guide = "none") +
    scale_x_discrete(labels = parse(text = levels(nmrpls_prop$Observation2))) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(axis.text = element_text(size = 12, colour = "grey65"),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank()
    )

  nmrrf_plot <- ggplot(nmrrf_prop,
                       aes(x = Observation2, y = Proportion, fill = Day)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Reds")[2:5],
                      name = NULL,
                      labels = c("day 1", "day 4", "day 6", "day 14")) +
    scale_x_discrete(labels = parse(text = levels(nmrrf_prop$Observation2))) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 1)) +
    theme(axis.text = element_text(size = 12, colour = "grey65"),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.position = "right"
    )

  # Extract Legend -------------------------------------------------------------
  g_tab <- ggplot_gtable(ggplot_build(nmrrf_plot))
  leg <- which(sapply(g_tab$grobs, function(x) x$name) == "guide-box")
  legend <- g_tab$grobs[[leg]]

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
      nmrrf_plot + theme(legend.position = "none"),
      top = grid::textGrob("NMR Random Forests",
                           x = grid::unit(0.5, "npc"),
                           y = grid::unit(0.5, "npc"),
                           gp = grid::gpar(fontsize = 12)
      ))

  # create x- and y-axis text grobs --------------------------------------------
  ygrob <- grid::textGrob("Proportion of Misclassified Samples",
                          x = grid::unit(0.5, "npc"),
                          y = grid::unit(0.5, "npc"),
                          just = c("centre"),
                          rot = 90,
                          gp = grid::gpar(fontsize = 14)
  )

  # Create, view, save plot ----------------------------------------------------

  if (view.plot) {
    gridExtra::grid.arrange(mzpls_plot_a,
                            nmrpls_plot_b,
                            mzrf_plot_c,
                            nmrrf_plot_d,
                            nrow = 2,
                            left = ygrob,
                            right = legend)
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
                            right = legend)
    grDevices::dev.off()
  }

}
