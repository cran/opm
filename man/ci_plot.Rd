\name{ci_plot}
\alias{ci_plot}
\title{Plot point estimates with CIs}
\arguments{
  \item{object}{\code{\link{OPMS}} object or (seldomly) a
  data frame. If an \code{\link{OPMS}} object, it is in
  most cases necessary to restrict the plates to at most
  about one dozen wells. See \code{\link{[}} for how to
  achieve this.

  The data frame method is not normally directly called by
  an \pkg{opm} user but via the \code{\link{OPMS}} method,
  unless it is used after \code{\link{extract}} was applied
  to a data frame for calculating point estimates and
  confidence intervals from groups of oberservations. See
  there for details.

  Otherwise, the data frame should be as exported bythe
  \code{\link{OPMS}} method of \code{\link{extract}} with
  \code{ci} set to \code{TRUE}. There must be a column
  named \code{\link{param_names}("split.at")} followed by
  columns with only numeric values. Columns before that
  split column, if any, are used for grouping. The rows
  must entirely comprise triplets representing (i) the
  point estimate, (ii) the lower and (iii) the upper
  confidence interval.}

  \item{as.labels}{List. Metadata to be joined and used to
  draw a legend.  Ignored if \code{NULL}.}

  \item{subset}{Character scalar. The parameter to plot.
  Only a single one can be selected. See
  \code{\link{param_names}} for the options.}

  \item{rowname.sep}{Character scalar. Used when joining
  explanatory columns into row labels of the plots.}

  \item{prop.offset}{Numeric scalar. A proportional offset
  that is added to the vertical range of the panels (after
  determining the maximum range among all panels to ensure
  consistency within the plot).}

  \item{align}{Character scalar. How to apply the offset;
  one of \sQuote{center}, \sQuote{left} and
  \sQuote{right}.}

  \item{col}{Character scalar. Colour to be used.}

  \item{na.action}{Character scalar. What to do if a
  confidence interval contains \code{NA} values; one of
  \sQuote{ignore}, \sQuote{warn} and \sQuote{error}.}

  \item{draw.legend}{Logical scalar. Ignored if there are
  no explanatory columns.}

  \item{legend.field}{Two-element numeric vector. Indicates
  the panel in which the legend is drawn. Subsequent
  arguments work then relative to this panel.  If
  \code{legend.field} has less then two fields, the number
  of panels is set to 1 (the entire plot), and the legend
  is drawn relative to that.}

  \item{x}{Legend position, passed to \code{legend} from
  the \pkg{graphics} package. Ignored unless
  \code{draw.legend} is \code{TRUE}.}

  \item{xpd}{Logical scalar. Also passed to that function.}

  \item{vline}{Numeric scalar with the position on the
  y-axis of a vertical line to be drawn. Ignored if
  \code{NULL}.}

  \item{...}{Optional other arguments passed to
  \code{legend}, or arguments passed from the
  \code{\link{OPMS}} method to the data frame method.}

  \item{split.at}{Character vector. See
  \code{\link{extract}}.}
}
\value{
  Character vector describing the plot's legend, returned
  invisibly.
}
\description{
  Draw point estimates with their confidence intervals.
  Used for comparing aggregated values together with their
  confidence intervals between plates. This method can in
  most cases \strong{not} be applied to entire plates but
  to selected wells only.
}
\details{
  The default placement of the legend is currently not
  necessarily very useful. When plotting entire PM plates,
  the \sQuote{mar} parameter of \code{par} most likely
  would need to be set to a lower value, but it is
  recommended to plot only subsets of plates, i.e. selected
  wells.
}
\examples{
data(vaas_4)

x <- ci_plot(vaas_4[, , 1:3], as.labels = list("Species", "Strain"),
  subset = "A", x = "bottomright", legend.field = NULL)
# note that the values on the y axes are drawn to scale
x
stopifnot(is.character(x), identical(length(x), 4L))
# ... and that the return value contains the legend (even if it is not drawn)

## See also the examples for the data-frame method of extract().
}
\references{
  Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P.
  2012 Visualization and curve parameter estimation
  strategies for efficient exploration of Phenotype
  Microarray kinetics. \emph{PLoS ONE} \strong{7}, e34846.
}
\seealso{
  graphics::plot

  Other plotting-functions: \code{\link{heat_map}},
  \code{\link{level_plot}}, \code{\link{radial_plot}},
  \code{\link{select_colors}}, \code{\link{show}},
  \code{\link{xy_plot}}
}
\keyword{hplot}

\docType{methods}
\alias{ci_plot-methods}
\alias{ci_plot,data.frame-method}
\alias{ci_plot,OPMS-method}
\usage{
  \S4method{ci_plot}{data.frame}(object, rowname.sep = " ",
    prop.offset = 0.04, align = "center", col = "blue", na.action = "warn",
    draw.legend = TRUE, legend.field = c(1, 1), x = "topleft", xpd = TRUE,
    vline = 0, split.at = param_names("split.at"), ...) 

  \S4method{ci_plot}{OPMS}(object, as.labels,
    subset = opm_opt("curve.param"), ...) 

}
