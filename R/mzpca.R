#' Function to perform PCA for LCMS data.
#'
#' \code{mzpca()} was used to perform the Principal Components Analysis of the
#' LCMS data (\code{./data/mzdata.rda})
#'
#' \code{mzpca()} zero-centres the data and performs a PCA using singular value
#' decomposition according to \code{\link[stats]{prcomp}}. It outputs a
#' \code{prcomp} object and a plot, which can be saved as either a \code{.pdf}
#' or a \code{.png} file, or both.
#'
#' @param scale Logical indicating if variables should be scaled to unit
#' variance.
#' @param center Logical indicating if variables should be zero centered.
#' @param savepdf Logical indicating if plot should be saved to \code{.pdf} in
#' the \code{./figs} directory.
#' @param savepng Logical indicating if plot should be saved to \code{.png} in
#' the \code{./figs} directory.
#' @param plotname Name of plot
#' @param ... Other arguments passed on to individual methods.
#'
#' @return returns a list with class \code{"prcomp"}.
#'
#' @note \code{mzpca()} was not intended to be used outside of this package
#' without modification to the underlying code.
#'
#' @seealso
#' \code{\link[stats]{prcomp()}}
#' \code{\link[ggplot2]{ggplot()}}
#'
#' @examples
#' x <- gordon01:::mzpca()
#'
mzpca <- function(scale = FALSE,
                  center = TRUE,
                  savepdf = TRUE,
                  savepng = TRUE,
                  plotname = "mzpca_plot",
                  ...) {

  library(ggplot2)

  load("./data/mzdata.rda")
  pca <- prcomp(mzdata[-1:-6], scale = scale, center = center)

  # Define variables for PCA plot ----------------------------------------------
  exp_var <- summary(pca)$importance[2 ,]
  scores <- data.frame(mzdata[, 2:6], pca$x)
  x_lab <- paste("PC1", " (", round(exp_var[1] * 100, 2), "%)", sep =  "")
  y_lab <- paste("PC2", " (", round(exp_var[2] * 100, 2), "%)", sep =  "")
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
  breaks = c(levels(scores$class))
  labels = c(levels(scores$class))

  # create plot base -----------------------------------------------------------
  pcaplot_base <- ggplot(data = scores,
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
    scale_color_manual(values = adjustcolor(custom_colors, alpha.f = 0.9),
                       breaks = breaks,
                       labels = labels,
                       name = NULL) +
    scale_fill_manual(values = adjustcolor(custom_colors, alpha.f = 0.5),
                      breaks = breaks,
                      labels = labels,
                      name = NULL) +
    scale_x_continuous(expand = c(0.15, 0),
                       limits = NULL,
                       name = x_lab) +
    scale_y_continuous(expand = c(0.15, 0),
                       limits = NULL,
                       name = y_lab)

  # create plot ----------------------------------------------------------------
  pcaplot <- pcaplot_base +
    geom_point(size = 2.5,
               stroke = 0.7,
               position = position_jitter(width = 0.01 * diff(range(scores$PC1)),
                                          height = 0.01 * diff(range(scores$PC2))
                                          )
               ) +
    theme_brg_grid()

  # save plot ------------------------------------------------------------------
  if(savepdf) {
    pdf(paste(c("./figs/", plotname, ".pdf"), collapse = ""),
        width = 10,
        height = 8,
        useDingbats = FALSE)
    print(pcaplot)
    dev.off()
  }
  if(savepng) {
    ppi <- 600
    png(paste(c("./figs/", plotname, ".png"), collapse = ""),
        width = 8*ppi,
        height = 6*ppi,
        res = ppi)
    print(pcaplot)
    dev.off()
  }

  print(pcaplot)
}

