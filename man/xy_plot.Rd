\name{xy_plot}
\alias{xy_plot}
\title{XY plot}
\arguments{
  \item{x}{\code{\link{OPM}} object.}

  \item{col}{Character scalar (color name) determining the
  line color.}

  \item{lwd}{Numeric scalar determining the line width.}

  \item{neg.ctrl}{Determine the height of a horizontal
  baseline drawn in each panel. If \code{NULL} or
  \code{FALSE}, no baseline will be drawn. If \code{TRUE},
  the baseline's height is the value of
  \code{\link{minmax}}.  If a character scalar,
  \code{neg.ctrl} is interpreted as the name of the well
  regarded as negative control. Its overall maximum will
  determine the height. Set \code{neg.ctrl} to a numeric
  value to set the height directly (at your own risk).}

  \item{base.col}{Character scalar. Baseline color (ignored
  if no baseline is drawn).}

  \item{base.lwd}{Numeric scalar determining the width of
  the baseline (ignored if no baseline is drawn).}

  \item{main}{The settings controlling the construction of
  the main title.  If a list, a named list with the
  following entries (if missing, they are replaced by the
  respective defaults): \describe{ \item{predef}{Character
  scalar or expression. Predefined title. If set, the other
  entries are ignored.} \item{use}{Logical scalar. If
  \code{FALSE}, returns \code{NULL}.} \item{...}{Other
  arguments are passed to \code{\link{plate_type}}.} } If
  \code{settings} is not a list but a character scalar or
  an expression, this is used as the \sQuote{predef} entry
  of the above-mentioned list. If not a list but a logical
  scalar, it is used as the \sQuote{use} entry of this
  list. If not a list but a numeric value, it is used as
  the \sQuote{max} entry of this list.}

  \item{xlab}{Character scalar. Title of x-axis. Use
  \code{NULL} to turn it off.}

  \item{ylab}{Character scalar. Title of y-axis. Use
  \code{NULL} to turn it off.}

  \item{theor.max}{Logical scalar. Use the theoretical
  maximum as maximum of the y-axis? If \code{FALSE}, use
  the empirical maximum with a small offset.}

  \item{draw.grid}{Logical scalar. Insert background grid?}

  \item{strip.fmt}{List controlling the format of the
  description strip above each panel. For instance, the
  background color is set using the \sQuote{bg} key. For
  further details, see \code{strip.custom} from the
  \pkg{lattice} package.}

  \item{striptext.fmt}{List controlling the textual
  description at the top of each panel. For instance, the
  relative text size is set using the \sQuote{cex} key, the
  color by \sQuote{col}, the font by \sQuote{font} and the
  number of lines by \sQuote{lines}. The latter might be of
  interest in conjunction with the \code{paren.sep}
  argument of \code{\link{wells}}.  See the argument
  \sQuote{par.strip.text} of \code{xyplot} from the
  \pkg{lattice} package for details.}

  \item{...}{Arguments that are passed to
  \code{\link{flatten}}.}
}
\value{
  An object of class \sQuote{trellis}. See \code{xyplot}
  from the \pkg{lattice} package for details.
}
\description{
  Customized plotting of a single PM plate, using
  \code{xyplot} from the \pkg{lattice} package. The optimal
  number of rows and columns is estimated from the number
  of selected wells. An optimal font size of the panel
  headers is also chosen automatically, but can also be
  adapted by the user, much like most aspects of the
  resulting graphics output.
}
\examples{
data(vaas_1)
xy_plot(vaas_1) # note the default main title built from the plate type

x <- vaas_1[, 11:22]
# Gives a warning message: we have deleted the default negative control:
xy_plot(x)
# Turn the baseline off => no warning:
xy_plot(x, neg.ctrl = NULL)
# Or guess a baseline
xy_plot(x, neg.ctrl = 100)
# Some like it ugly:
xy_plot(x, neg.ctrl = 100, col = "pink", base.col = "yellow", main = "Ugly")
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
  \code{\link{select_colors}}, \code{\link{xy_plot,OPMS}}
}
\keyword{hplot}

\docType{methods}
\alias{xy_plot-methods}
\alias{xy_plot,OPM-method}
\usage{
  \S4method{xy_plot}{OPM}(x, col = "midnightblue", lwd = 1,
    neg.ctrl = "A01", base.col = "grey10", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE,
    strip.fmt = list(), striptext.fmt = list(),
    ...) 
}
