#' Boxplots of cell density.
#'
#' \code{anova_cell_density()} reproduces the compound figure of the cell
#' denisty data in section x.xx of Benjamin Gordon's PhD thesis.
#'
#' \code{anova_cell_density()} describes the cell density data located in
#' \code{./data-raw}.
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
anova_cell_density <- function() {

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
  day5aov <- stats::aov(density ~ class,
                 data = celldens[celldens$day == "day 5", ])
  day10aov <- stats::aov(density ~ class,
                  data = celldens[celldens$day == "day 10", ])
  day14aov <- stats::aov(density ~ class,
                  data = celldens[celldens$day == "day 14", ])
  anova_list <- list("day5" = day5aov,
                     "day10" = day10aov,
                     "day14" = day14aov)

  # posthoc analysis
  day5post <- stats::pairwise.t.test(celldens$density[celldens$day == "day 5"],
                              celldens$class[celldens$day == "day 5"],
                              p.adjust.method = "bonferroni")

  day10post <- stats::pairwise.t.test(celldens$density[celldens$day == "day 10"],
                               celldens$class[celldens$day == "day 10"],
                               p.adjust.method = "bonferroni")

  day14post <- stats::pairwise.t.test(celldens$density[celldens$day == "day 14"],
                               celldens$class[celldens$day == "day 14"],
                               p.adjust.method = "bonferroni")
  posthoc_list <- list("day5" = day5post,
                       "day10" = day10post,
                       "day14" = day14post)

  # ccombine ANOVA and posthoc stats
  post_anova <- list("anova" = anova_list,
                     "bonf" = posthoc_list)

  post_anova
}
