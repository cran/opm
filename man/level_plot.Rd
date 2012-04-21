\name{level_plot}
\alias{level_plot}
\title{Levelplot}
\arguments{
  \item{x}{\code{\link{OPM}} or \code{\link{OPMS}} object.}

  \item{main}{The settings controlling the construction of
  the main title.  Works like the \code{main} argument of
  \code{\link{xy_plot}}.}

  \item{colors}{Character vector indicating the colours (at
  least two).}

  \item{panel.headers}{\code{NULL}, logical scalar,
  expression or character vector. \code{NULL} and
  \code{FALSE} turn panel headers off. \code{TRUE} causes
  the panel headers to be constructed from the plate
  numbers or those metadata that were included by
  \code{\link{flatten}} (see there).  Character vectors and
  expressions are directly used for the text within these
  panel headers.}

  \item{cex}{Numeric scalar. Magnification of axis
  annotation. If \code{NULL}, automatically adapted to the
  number of wells (at least a good guess is made).}

  \item{strip.fmt}{List controlling the format of the
  description strip above each panel. For instance, the
  background colour is set using the \sQuote{bg} key. For
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
  \code{\link{flatten}}.}
}
\value{
  An object of class \sQuote{trellis}. See \code{levelplot}
  from the \pkg{lattice} package for details.
}
\description{
  Levelplot for \code{\link{OPM}} and \code{\link{OPMS}}
  objects using the function from the \pkg{lattice}
  package.
}
\examples{
# OPM method
data(vaas_1)
level_plot(vaas_1, main = "Levelplot example")

# OPMS method
data(vaas_4)
# headers include species and strain
level_plot(vaas_4, include = c("Species", "Strain"))
}
\references{
  Jacobsen, J. S., Joyner, D. C., Borglin, S. E., Hazen, T.
  C., Arkin, A. P. et al. 2007 Visualization of growth
  curve data from phenotype microarray experiments.
  \emph{11th International Conference on Information
  Visualization (IV07).} Zuerich, Switzerland, July 4-6
  2007. Published by the IEEE Computer Society.

  Sarkar, D. 2008 \emph{Lattice: Multivariate Data
  Visualization with R.} New York: Springer, 265 p.

  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.
}
\seealso{
  lattice::levelplot

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{max_rgb_contrast}},
  \code{\link{radial_plot}}, \code{\link{select_colors}},
  \code{\link{xy_plot}}
}
\keyword{hplot}

\docType{methods}
\alias{level_plot-methods}
\alias{level_plot,OPM-method}
\alias{level_plot,OPMS-method}
\usage{
  \S4method{level_plot}{OPM}(x, main = list(), colors = NULL,
    cex = NULL, ...) 

  \S4method{level_plot}{OPMS}(x, main = list(), colors = NULL, 
    panel.headers = TRUE, cex = NULL, strip.fmt = list(),
    striptext.fmt = list(), legend.sep = " ", ...) 

}
