\name{xy_plot}
\alias{xy_plot}
\title{X-Y plot}
\arguments{
  \item{x}{\code{\link{OPM}} or \code{\link{OPMS}} object.}

  \item{col}{For the \code{\link{OPM}} method, just a
  character scalar (colour name) determining the line
  colour.

  For the \code{\link{OPMS}} method, either a character
  vector with colour codes or one of the arguments of
  \code{\link{select_colors}} (for picking one of the
  predefined colour sets).

  It is an error if fewer colours are chosen than the
  number of plate grouping levels (see the \code{\dots}
  argument below). For user-chosen colour sets, keep in
  mind that the sets are not checked for duplicates, and
  see \code{max_rgb_contrast} from the \pkg{pkgutils}
  package as a method for optimally arranging user-defined
  colours.}

  \item{lwd}{Numeric scalar determining the line width.}

  \item{neg.ctrl}{Determine the height of a horizontal
  baseline drawn in each panel. \itemize{ \item If
  \code{NULL} or \code{FALSE}, no baseline will be drawn.
  \item If \code{TRUE}, the baseline's height is the value
  of \code{\link{minmax}}.  \item If a character scalar,
  \code{neg.ctrl} is interpreted as the name of the wells
  regarded as negative control, and the baseline's height
  becomes the value of \code{\link{minmax}} applied to
  these wells only.  \item Set \code{neg.ctrl} to a numeric
  value for assigning the height directly (at your own
  risk).  }}

  \item{base.col}{Character scalar. Baseline colour
  (ignored if no baseline is drawn).}

  \item{base.lwd}{Numeric scalar determining the width of
  the baseline (ignored if no baseline is drawn).}

  \item{main}{The settings controlling the construction of
  the main title. If a list, a named list with the
  following entries (if missing, they are replaced by the
  respective defaults): \describe{ \item{predef}{Character
  scalar or expression. Predefined title. If set, the other
  entries are ignored.} \item{use}{Logical scalar. If
  \code{FALSE}, returns \code{NULL}.} \item{...}{Other
  arguments are passed to \code{\link{plate_type}}.} } If
  \code{settings} is not a list but a character scalar or
  an expression, this is used as the \kbd{predef} entry of
  the above-mentioned list. If not a list but a logical
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

  \item{space}{Character scalar indicating the position of
  the legend; either \sQuote{top}, \sQuote{bottom},
  \sQuote{left} or \sQuote{right}. Might be overwritten by
  \code{legend.fmt}.}

  \item{strip.fmt}{List controlling the format of the
  description strip above each panel. For instance, the
  background colour is set using the \kbd{bg} key. For
  further details, see \code{strip.custom} from the
  \pkg{lattice} package. Note that the \strong{content} of
  these descriptions is determined by arguments passed from
  \code{xy_plot} to \code{\link{wells}}; see there for
  details.}

  \item{striptext.fmt}{List controlling the textual
  description at the top of each panel. For instance, the
  relative text size is set using the \kbd{cex} key, the
  colour by \sQuote{col}, the font by \sQuote{font} and the
  number of lines by \sQuote{lines}. The latter might be of
  interest in conjunction with the \code{paren.sep}
  argument of \code{\link{wells}}. See the argument
  \code{par.strip.text} of \code{xyplot} from the
  \pkg{lattice} package for details.}

  \item{legend.fmt}{List controlling where and how to draw
  the legend. The content of the legend (mainly a
  description of the assignment of the colours to the
  curves) is determined automatically. See argument
  \sQuote{key} of \code{xyplot} from the \pkg{lattice}
  package for details.}

  \item{legend.sep}{Character scalar. Relevant only if more
  than one columns of metadata have been selected; will
  then be used as separator to join their names in the
  legend.}

  \item{draw.legend}{Logical scalar. If \code{FALSE}, no
  legend is drawn, and the two aforementioned arguments are
  ignored.}

  \item{...}{Arguments that are passed to
  \code{\link{flatten}}. For the \code{\link{OPMS}} method,
  \code{include} is particularly important: the selected
  metadata are joined into a single factor, and the
  assignment of plates to this factor's levels determines
  the curve colour for each plate.  That is, each
  combination of metadata entries as chosen using
  \code{include} yields one colour. If no metadata are
  selected (the default), each plate gets a colour of its
  own. Also note that arguments passed via
  \code{\link{flatten}} to \code{\link{wells}} can be given
  here which determine the content of the panel
  description.}

  \item{f}{Formula (for the data-frame method).}

  \item{groups}{Character vector (for the data-frame
  method).}
}
\value{
  An object of class \sQuote{trellis}. See \code{xyplot}
  from the \pkg{lattice} package for details.
}
\description{
  Customised plotting of a single \acronym{PM} plate or
  multiple plates, using \code{xyplot} from the
  \pkg{lattice} package.
}
\details{
  The optimal number of rows and columns is estimated from
  the number of selected wells. An optimal font size of the
  panel headers is also chosen automatically, but can also
  be adapted by the user, much like most aspects of the
  resulting graphics output.

  In the case of the \code{\link{OPMS}} method, if metadata
  are selected, curve colours are determined according to
  the combinations of these metadata entries, otherwise
  each plate gets its own colour.

  The data-frame method is not intended for phenotype
  microarray data. It is currently \strong{undocumented}
  and potentially subject to frequent changes or even
  removal. Users interested in the method should contact
  the authors.
}
\examples{
# OPM method
\dontrun{
xy_plot(vaas_1) # note the default main title built from the plate type
}

x <- vaas_1[, 11:22]
# Yields a warning message: we have deleted the default negative control.
xy_plot(x)
# Turn the baseline off => no warning:
xy_plot(x, neg.ctrl = NULL)
# Or guess a baseline:
xy_plot(x, neg.ctrl = 100)
# Some like it ugly:
xy_plot(x, neg.ctrl = 100, col = "pink", base.col = "yellow", main = "Ugly")

# OPMS method
\dontrun{
# Colour by species and strain; note default main title
xy_plot(vaas_4, include = c("Species", "Strain"))
# Use the largest of the negative-control maxima as baseline
xy_plot(vaas_4, include = c("Species", "Strain"),
  neg.ctrl = max(vaas_4, "A01"))
}
}
\references{
  Sarkar, D. 2008 \emph{Lattice: Multivariate Data
  Visualization with R.} New York: Springer, 265 p.

  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.
}
\seealso{
  lattice::xyplot

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{parallelplot}}, \code{\link{radial_plot}},
  \code{\link{summary}}
}
\keyword{hplot}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{xy_plot-methods}
\alias{xy_plot,OPM-method}
\alias{xy_plot,OPMS-method}
\alias{xy_plot,data.frame-method}
\usage{
  \S4method{xy_plot}{OPM}(x, col = "midnightblue", lwd = 1,
    neg.ctrl = "A01", base.col = "grey10", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE,
    strip.fmt = list(), striptext.fmt = list(),
    ...) 
  \S4method{xy_plot}{OPMS}(x, col = opm_opt("colors"), lwd = 1,
    neg.ctrl = "A01", base.col = "black", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE, space = "top",
    strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE,
    ...) 
  \S4method{xy_plot}{data.frame}(x, f, groups,
    col = opm_opt("colors"), lwd = 1, neg.ctrl = NULL, base.col = "black",
    base.lwd = lwd, main = groups, xlab = elem(f, 3L:2L), ylab = elem(f, 2L),
    draw.grid = TRUE, space = "top", strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
