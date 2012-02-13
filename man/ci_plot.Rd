\name{ci_plot}
\alias{ci_plot}
\title{Plot point estimates with CIs}
\arguments{
  \item{object}{Dataframe as exported by
  \code{\link{extract}} with \code{ci} set to \code{TRUE}.
  There must be a column named \sQuote{Parameter} followed
  by columns with only numeric values. Columns before the
  \sQuote{Parameter} column, if any, are used for grouping.
  The rows must entirely comprise triplets representing (i)
  the point estimate, (ii) the lower and (iii) the upper
  confidence interval.}

  \item{rowname.sep}{Character scalar. Used when joining
  explanatory columns into row labels of the plots.}

  \item{prop.offset}{Numeric scalar. A proportional offset
  that is added to the vertical range of the panels (after
  determining the maximum range among all panels to ensure
  consistency within the plot).}

  \item{align}{Character scalar. How to apply the offset;
  one of \sQuote{center}, \sQuote{left} and
  \sQuote{right}.}

  \item{col}{Character scalar. Color to be used.}

  \item{na.action}{Character scalar. What to do if a
  confidence interval contains \code{NA} values; one of
  \sQuote{ignore}, \sQuote{warn} and \sQuote{error}.}

  \item{draw.legend}{Logical scalar. Ignored if there are
  no explanatory columns.}

  \item{legend.field}{Two-element numeric vector. Indicates
  the panel in which the legend is drawn. Subsequent
  arguments work then relative to this panel. If
  \code{legend.field} has less then two fields, the number
  of panels is set to 1 (the entire plot), and the legend
  is drawn relative to that.}

  \item{x}{Legend position, passed to \code{legend} from
  the \pkg{graphics} package. Ignored unless
  \code{draw.legend} is \code{TRUE}.}

  \item{xpd}{Logical scalar. Also passed to that function.}

  \item{...}{Optional other arguments passed to that
  function.}
}
\value{
  Character vector describing the plot's legend, returned
  invisibly.
}
\description{
  Draw point estimates with their confidence intervals.
  This is not normally directly called by an\pkg{opm} user
  because \code{\link{ci_plot,OPMS}} is available. The
  graphical parameters are described here, however.
}
\note{
  \itemize{ \item The default placement of the legend is
  currently not necessarily very useful.  \item When
  plotting entire PM plates, the \sQuote{mar} parameter of
  \code{par} most likely would need to be set to a lower
  value, but it is recommended to plot only subsets of
  plates, i.e. selected wells.  }
}
\examples{
# This function is usually called via the OPMS method ci_plot(); see
# there for examples.
}
\seealso{
  plot

  Other plotting-functions: \code{\link{ci_plot,OPMS}},
  \code{\link{heat_map}}, \code{\link{heat_map,matrix}},
  \code{\link{level_plot}}, \code{\link{level_plot,OPMS}},
  \code{\link{max_rgb_contrast}},
  \code{\link{select_colors}}, \code{\link{xy_plot}},
  \code{\link{xy_plot,OPMS}}
}
\keyword{hplot}

\docType{methods}
\alias{ci_plot-methods}
\alias{ci_plot,data.frame-method}
\usage{
  \S4method{ci_plot}{data.frame}(object, rowname.sep = " ", 
    prop.offset = 0.04, align = "center", col = "blue", na.action = "warn",
    draw.legend = TRUE, legend.field = c(1, 1), x = "topleft", xpd = TRUE, 
    ...) 
}
