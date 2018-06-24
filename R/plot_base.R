#' Custom base values for ggplot2
#'
#' Custom base values for setting ggplot2 scales such as shape, color,
#' fill and xy axes. The code is mostly relevant to scatter plots.
#'
#' @keywords internal

plot_base <- function() {
  custom_shapes <- c(21:25)
  custom_colors <- c("orange" = "#E69F00",
                     "sky blue" = "#56B4E9",
                     "green" = "#009E73",
                     "purple" = "#CC79A7",
                     "grey" = "#999999",
                     "yellow" = "#F0E442",
                     "blue" = "#0072B2",
                     "dark orange" = "#D55E00"
                     )
  breaks = c(levels(mzdata$class))
  labels = c(levels(mzdata$class))

  plot_base <- ggplot(data = scores,
                      aes(x = PC1,
                          y = PC2,
                          shape = class,
                          fill = class,
                          color = class)
                      ) +
    scale_shape_manual(values = custom_shapes,
                       breaks = breaks,
                       labels = labels,
                       name = NULL) +
    scale_color_manual(values = grDevices::adjustcolor(custom_colors,
                                                       alpha.f = 0.9),
                       breaks = breaks,
                       labels = labels,
                       name = NULL) +
    scale_fill_manual(values = grDevices::adjustcolor(custom_colors,
                                                      alpha.f = 0.5),
                      breaks = breaks,
                      labels = labels,
                      name = NULL) +
    scale_x_continuous(expand = c(0.15, 0),
                       limits = NULL,
                       name = x_lab) +
    scale_y_continuous(expand = c(0.15, 0),
                       limits = NULL,
                       name = y_lab)

}
