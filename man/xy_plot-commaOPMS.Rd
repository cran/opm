\name{xy_plot,OPMS}
\alias{xy_plot,OPMS}
\title{XY plot (OPMS version)}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{col}{Line color. Either a character vector with
  color codes or one of the arguments of
  \code{\link{select_colors}} (for picking one of the
  predefined color sets). It is an error if fewer colors
  are chosen than the number of plate grouping levels (see
  the \code{...} argument below). For user-chosen color
  sets, keep in mind that the sets are not checked for
  duplicates. See \code{\link{max_rgb_contrast}} as a
  method for optimally arranging user-defined colors.}

  \item{lwd}{Numeric scalar determining the line width.}

  \item{neg.ctrl}{Determine the height of a horizontal
  baseline drawn in each panel. If \code{NULL} or
  \code{FALSE}, no baseline will be drawn. If \code{TRUE},
  the baseline's height is the value of
  \code{\link{minmax}}.  If a character scalar,
  \code{neg.ctrl} is interpreted as the name of the wells
  regarded as negative control, and the baseline's height
  becomes the value of \code{\link{minmax}} applied to
  these wells only. Set \code{neg.ctrl} to a numeric value
  for assigning the height directly.}

  \item{base.col}{Character scalar. Baseline color (ignored
  if no baseline is drawn).}

  \item{base.lwd}{Numeric scalar determining the width of
  the baseline (ignored if no baseline is drawn).}

  \item{main}{The settings controlling the construction of
  the main title.  Works like the \code{main} argument of
  \code{\link{xy_plot}}.}

  \item{xlab}{Character scalar. Title of x-axis. Use
  \code{NULL} to turn it off.}

  \item{ylab}{Character scalar. Title of y-axis. Use
  \code{NULL} to turn it off.}

  \item{theor.max}{Logical scalar. Use the theoretical
  maximum as maximum of the y-axis? If \code{FALSE}, use
  the empirical maximum with a small offset.}

  \item{draw.grid}{Logical scalar. Insert background grid?}

  \item{space}{Character scalar indicating the position of
  the legend; either \sQuote{top}, \sQuote{bottom},
  \sQuote{left} or \sQuote{right}. Might be overwritten by
  \code{legend.fmt}.}

  \item{strip.fmt}{List controlling the format of the
  description strip above each panel. For instance, the
  background color is set using the \sQuote{bg} key. For
  further details, see \code{strip.custom} from the
  \pkg{lattice} package.}

  \item{striptext.fmt}{List controlling the textual
  description at the top of each panel. Works like the
  \code{striptext.fmt} argument of \code{\link{xy_plot}}.}

  \item{legend.fmt}{List controlling where and how to draw
  the legend. The content of the legend (mainly a
  description of the assigment of the colors to the curves)
  is determined automatically. See argument \sQuote{key} of
  \code{xyplot} from the \pkg{lattice} package for
  details.}

  \item{legend.sep}{Character scalar. Relevant only if more
  than one columns of metadata have been selected; will
  then be used as separator to join their names in the
  legend.}

  \item{draw.legend}{Logical scalar. If \code{FALSE}, no
  legend is drawn, and the two aforementioned arguments are
  ignored.}

  \item{...}{Arguments that are passed to
  \code{\link{flatten,OPMS}}.  Particularly important is
  \code{include}: the selected metadata are joined into a
  single factor, and the assignment of plates to this
  factor's levels determines the curve color for each
  plate. That is, each combination of metadata entries as
  chosen using \code{include} yields one color. If no
  metadata are selected (the default), each plate gets a
  color of its own.}
}
\value{
  An object of class \sQuote{trellis}. See \code{xyplot}
  from the \pkg{lattice} package for details.
}
\description{
  Customized plotting of multiple PM plates, using
  \code{xyplot} from \pkg{lattice}. The optimal number of
  rows and columns is estimated from the number of selected
  wells. An optimal font size of the panel headers is also
  chosen automatically, but can also be adapted by the
  user, much like most aspects of the resulting graphics
  output. If metadata are selected, curve colors are
  determined according to the combinations of these
  metadata entries, otherwise each plate gets its own
  color.
}
\examples{
data(vaas_4)
# Color by species and strain; note default main title
xy_plot(vaas_4, include = c("Species", "Strain"))
# Use the largest of the negative-control maxima as baseline
xy_plot(vaas_4, include = c("Species", "Strain"),
  neg.ctrl = max(vaas_4, "A01"))
}
\references{
  Sarkar D. Lattice: Multivariate Data Visualization with
  R. 2008; New York: Springer, 265p.

  Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P.
  Visualization and curve parameter estimation strategies
  for efficient exploration of Phenotype Microarray
  kinetics. PLoS ONE 2012; in press.
}
\seealso{
  lattice::xyplot

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{ci_plot,OPMS}}, \code{\link{heat_map}},
  \code{\link{heat_map,matrix}}, \code{\link{level_plot}},
  \code{\link{level_plot,OPMS}},
  \code{\link{max_rgb_contrast}},
  \code{\link{select_colors}}, \code{\link{xy_plot}}
}
\keyword{hplot}

\docType{methods}
\alias{xy_plot,OPMS-method}
\usage{
  \S4method{xy_plot}{OPMS}(x, col = "nora", lwd = 1,
    neg.ctrl = "A01", base.col = "black", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE, space = "top",
    strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE,
    ...) 
}
