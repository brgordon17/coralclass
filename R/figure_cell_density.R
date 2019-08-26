#' Boxplots of cell density.
#'
#' \code{figure_cell_density()} reproduces the compound figure of the cell
#' denisty data in section x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{figure_cell_density()} summarises the cell density data located in
#' \code{./data-raw} The figure can be saved to \code{./figs/} if required.
#'
#' @param view.plot Logical indicating if the plot should be printed to the plot
#' viewer.
#' @param save.plot Logical indicating if the plot should be saved to
#' \code{./figs/}.
#' @param plot.name Name of plot.
#'
#' @note Although this function is exported, \code{figure_cell_density()}
#' was not intended to be used outside of this package.
#'
#' @seealso
#' \code{\link[ggplot2]{ggplot}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
figure_cell_density <- function(view.plot = TRUE,
                                save.plot = FALSE,
                                plot.name = "cell_density_plot") {

  # load data and modify
  celldens <- readr::read_csv("./data-raw/cell-densities.csv")
  colnames(celldens)[3] <- "density"
  celldens$class <- factor(celldens$class,
                           levels = c("control", "eCO2", "eT", "eCO2eT"))
  celldens$day <- factor(c(rep("day 5", 24),
                           rep("day 10", 24),
                           rep("day 14", 24)),
                         levels = c("day 5", "day 10", "day 14")
                         )

  # plot
  cell_plot <-
    ggplot(celldens, aes(x = class, y = density)) +
    geom_boxplot() +
    facet_grid(.~day) +
    xlab("Treatment") +
    ylab("Cell density") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "grey65"),
          axis.title = element_text(size = 14),
          axis.ticks = element_blank()
    )

  ggsave("./cell-density-boxplot.pdf",
         height = 5, width = 10, units = "in")
  grDevices::graphics.off()

  # Create, view, save plot ----------------------------------------------------
  if (view.plot) {
    print(cell_plot)
  }

  if (save.plot) {
    grDevices::pdf(paste(c("./figs/", plot.name, ".pdf"), collapse = ""),
                   width = 10,
                   height = 5,
                   useDingbats = FALSE)
    cell_plot
    grDevices::dev.off()
  }
}
