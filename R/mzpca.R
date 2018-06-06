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
#' @param view.plot Logical indicating if plot should be printed to the plot
#' viewer.
#' @param save.pdf Logical indicating if plot should be saved to \code{.pdf} in
#' the \code{./figs} directory.
#' @param save.png Logical indicating if plot should be saved to \code{.png} in
#' the \code{./figs} directory.
#' @param save.gg Logical indicating if ggplot object should be saved to
#' \code{./inst/extdata/mzpca_ggobject.rds}.
#' @param plot.name Name of plot.
#' @param seed An integer for setting the RNG state.
#' @param ... Other arguments passed on to individual methods.
#'
#' @return returns a list with class \code{"prcomp"}.
#'
#' @note Although this function is exported, \code{mzpca()} was not intended to
#' be used outside of this package.
#'
#' @seealso
#' \code{\link[stats]{prcomp}}
#' \code{\link[ggplot2]{ggplot}}
#' \code{\link{qual_colours}}
#' \code{\link{theme_brg_grid}}
#'
#' @author Benjamin R. Gordon
#'
#' @import ggplot2
#' grDevices
#' stats
#'
#' @export
#'
mzpca <- function(scale = FALSE,
                  center = TRUE,
                  view.plot = TRUE,
                  save.pdf = FALSE,
                  save.png = FALSE,
                  save.gg = FALSE,
                  plot.name = "mzpca_plot",
                  seed = 1978,
                  ...) {


  set.seed(seed)
  pca <- prcomp(mzdata[-1:-6], scale = scale, center = center)

  # Define variables for PCA plot ----------------------------------------------
  exp_var <- summary(pca)$importance[2 ,]
  scores <- data.frame(mzdata[, 2:6], pca$x)
  x_lab <- paste("PC1", " (", round(exp_var[1] * 100, 2), "%)", sep =  "")
  y_lab <- paste("PC2", " (", round(exp_var[2] * 100, 2), "%)", sep =  "")
  custom_colors <- qual_colours()


  # create plot ----------------------------------------------------------------
  pcaplot <-
    ggplot(data = scores,
           aes(x = PC1,
               y = PC2,
               color = class,
               fill = class,
               shape = class)) +
    geom_point(size = 2.5,
               stroke = 0.7,
               position = position_jitter(width = 0.01 * diff(range(scores$PC1)),
                                          height = 0.01 * diff(range(scores$PC2))
               )) +
    labs(x = x_lab, y = y_lab) +
    scale_shape_manual(values = c(21:25)) +
    scale_color_manual(values = adjustcolor(custom_colors,
                                            alpha.f = 0.9)) +
    scale_fill_manual(values = adjustcolor(custom_colors,
                                           alpha.f = 0.5)) +
    theme_brg_grid()

  # saves ----------------------------------------------------------------------
  if(save.gg) {
    saveRDS(pcaplot, "./inst/extdata/mzpca_ggobject.rds")
  }

  if(save.pdf) {
    pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
        width = 10,
        height = 8,
        useDingbats = FALSE)
    print(pcaplot)
    dev.off()
  }
  if(save.png) {
    ppi <- 600
    png(paste(c("./figs/", plot.name, ".png"), collapse = ""),
        width = 8*ppi,
        height = 6*ppi,
        res = ppi)
    print(pcaplot)
    dev.off()
  }

  if(view.plot) {
    print(pcaplot)
  }

}
