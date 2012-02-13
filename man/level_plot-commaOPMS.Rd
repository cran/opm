\name{level_plot,OPMS}
\alias{level_plot,OPMS}
\title{Levelplot (OPMS version)}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{main}{The settings controlling the construction of
  the main title.  Works like the \code{main} argument of
  \code{\link{xy_plot}}.}

  \item{colors}{See \code{\link{level_plot}}.}

  \item{panel.headers}{\code{NULL}, logical scalar,
  expression or character vector. \code{NULL} and
  \code{FALSE} turn panel headers off. \code{TRUE} causes
  the panel headers to be constructed from the plate
  numbers or those metadata that were included by
  \code{\link{flatten}} (see there).  Character vectors and
  expressions are directly used for the text within these
  panel headers.}

  \item{cex}{See \code{\link{level_plot}}.}

  \item{strip.fmt}{List controlling the format of the
  description strip above each panel. For instance, the
  background color is set using the \sQuote{bg} key. For
  further details, see \code{strip.custom} from the
  \pkg{lattice} package. \code{strip.fmt} is ignored if
  panel.headers is \code{FALSE}.}

  \item{striptext.fmt}{List controlling the format of the
  text within the strip above each panel. See
  \code{\link{xy_plot}} for details, which has an argument
  of the same name.}

  \item{legend.sep}{Character scalar. This works like the
  eponymous argument to \code{\link{flatten}} (see there);
  it is ignored unless metadata are chosen for constructing
  the panel headers.}

  \item{...}{Arguments that are passed to
  \code{\link{flatten,OPMS}}.}
}
\value{
  An object of class \sQuote{trellis}. See \code{levelplot}
  from the \pkg{lattice} package for details.
}
\description{
  Levelplot for \code{\link{OPMS}} objects using the
  \pkg{lattice} package.
}
\examples{
data(vaas_4)
# headers include species and strain
level_plot(vaas_4, include = c("Species", "Strain"))
}
\references{
  Jacobsen JS, Joyner DC, Borglin SE, Hazen TC, Arkin AP et
  al.  Visualization of growth curve data from phenotype
  microarray experiments.  2007; 11th International
  Conference on Information Visualization (IV07), Zurich,
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
  \code{\link{heat_map,matrix}}, \code{\link{level_plot}},
  \code{\link{max_rgb_contrast}},
  \code{\link{select_colors}}, \code{\link{xy_plot}},
  \code{\link{xy_plot,OPMS}}
}
\keyword{hplot}

\docType{methods}
\alias{level_plot,OPMS-method}
\usage{
  \S4method{level_plot}{OPMS}(x, main = list(),
    colors = NULL, panel.headers = TRUE, cex = NULL, strip.fmt = list(),
    striptext.fmt = list(), legend.sep = " ", ...) 
}
