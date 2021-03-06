% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figure_pca.R
\name{figure_pca}
\alias{figure_pca}
\title{PCA compound figure.}
\usage{
figure_pca(scale = FALSE, center = TRUE, view.plot = TRUE,
  save.plot = FALSE, plot.name = "pca_figure", seed = 1978)
}
\arguments{
\item{scale}{Logical indicating if scaling should be applied in PCA}

\item{center}{Logical indicatinf if the data should be centered}

\item{view.plot}{Logical indicating if the plot should be printed to the plot
viewer.}

\item{save.plot}{Logical indicating if the plot should be saved to
\code{./figs/}.}

\item{plot.name}{Name of plot.}

\item{seed}{Integer the sets the state of the random number generator}
}
\description{
\code{figure_pca()} reproduces the LCMS and NMR PCA scores compound figure
in Benjamin Gordon's PhD thesis.
}
\details{
\code{figure_pca()} loads the saved gg objects in \code{./inst/extdata/};
extracts the legend from the mzpca plot; adds plot labels (a and b) and saves
the plot to \code{./figs/} as a \code{.pdf} file.
}
\note{
Although this function is exported, \code{figure_pca()} was not
intended to be used outside of this package.
}
\seealso{
\code{\link[gridExtra]{gridExtra-package}}
\code{\link[ggplot2]{ggplot}}
\code{\link[grid]{grid-package}}
}
\author{
Benjamin R. Gordon
}
