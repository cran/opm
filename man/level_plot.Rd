\name{level_plot}
\alias{level_plot}
\title{Levelplot}
\arguments{
  \item{x}{\code{\link{OPM}} object.}

  \item{main}{The settings controlling the construction of
  the main title.  Works like the \code{main} argument of
  \code{\link{xy_plot}}.}

  \item{colors}{Character vector indicating the colors (at
  least two).}

  \item{cex}{Numeric scalar. Magnification of axis
  annotation. If \code{NULL}, automatically adapted to the
  number of wells (at least a good guess is made).}

  \item{...}{Arguments that are passed to
  \code{\link{flatten}}.}
}
\value{
  An object of class \sQuote{trellis}. See \code{levelplot}
  from the \pkg{lattice} package for details.
}
\description{
  Levelplot for \code{\link{OPM}} objects using the
  \pkg{lattice} package.
}
\examples{
data(vaas_1)
level_plot(vaas_1, main = "Levelplot example")
}
\references{
  Jacobsen JS, Joyner DC, Borglin SE, Hazen TC, Arkin AP et
  al.  Visualization of growth curve data from phenotype
  microarray experiments.  2007; 11th International
  Conference on Information Visualization (IV07), Zuerich,
  Switzerland, July 4-6, 2007. Published by the IEEE
  Computer Society.

  Sarkar D. Lattice: Multivariate Data Visualization with
  R. 2008; New York: Springer, 265p.

  Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P.
  Visualization and curve parameter estimation strategies
  for efficient exploration of Phenotype Microarray
  kinetics. PLoS ONE 2012; in press.
}
\seealso{
  lattice::levelplot

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{ci_plot,OPMS}}, \code{\link{heat_map}},
  \code{\link{heat_map,matrix}},
  \code{\link{level_plot,OPMS}},
  \code{\link{max_rgb_contrast}},
  \code{\link{select_colors}}, \code{\link{xy_plot}},
  \code{\link{xy_plot,OPMS}}
}
\keyword{hplot}

\docType{methods}
\alias{level_plot-methods}
\alias{level_plot,OPM-method}
\usage{
  \S4method{level_plot}{OPM}(x, main = list(), colors = NULL,
    cex = NULL, ...) 
}
