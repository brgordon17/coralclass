#' Boxplots of cell density.
#'
#' \code{anova_cell_density()} reproduces the compound figure of the cell
#' denisty data in section x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{anova_cell_density()} describes the cell density data located in
#' \code{./data-raw}.
#'
#' @param anova Logical indicating if the ANOVA results should be printed to the
#' console.
#' @param posthoc Logical indicating if the Bonferroni posthoc results should be
#' printed to the console.
#'
#' @note Although this function is exported, \code{anova_cell_density()}
#' was not intended to be used outside of this package.
#'
#' @seealso
#' \code{\link[ggplot2]{ggplot}}
#'
#' @author Benjamin R. Gordon
#'
#' @export
#'
anova_cell_density <- function(anova = TRUE,
                               posthoc = TRUE) {

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

  # ANOVA
  day5aov <- aov(density ~ class,
                 data = celldens[celldens$day == "day 5", ])
  day10aov <- aov(density ~ class,
                  data = celldens[celldens$day == "day 10", ])
  day14aov <- aov(density ~ class,
                  data = celldens[celldens$day == "day 14", ])

  # posthoc analysis
  day5post <- pairwise.t.test(celldens$density[celldens$day == "day 5"],
                              celldens$class[celldens$day == "day 5"],
                              p.adjust.method = "bonferroni")

  day10post <- pairwise.t.test(celldens$density[celldens$day == "day 10"],
                               celldens$class[celldens$day == "day 10"],
                               p.adjust.method = "bonferroni")

  day14post <- pairwise.t.test(celldens$density[celldens$day == "day 14"],
                               celldens$class[celldens$day == "day 14"],
                               p.adjust.method = "bonferroni")

  # Printing
  if (anova) {
    message("ANOVA Day 5")
    summary(day5aov)
    message("ANOVA Day 10")
    summary(day10aov)
    message("ANOVA Day 14")
    summary(day14aov)
  }

  if (posthoc) {
    message("Bonferroni Posthoc Day 5")
    day5post
    message("Bonferroni Posthoc Day 10")
    day10post
    message("Bonferroni Posthoc Day 14")
    day14post
  }
}
