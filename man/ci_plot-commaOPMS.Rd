\name{ci_plot,OPMS}
\alias{ci_plot,OPMS}
\title{Plot aggregated values}
\arguments{
  \item{object}{\code{\link{OPMS}} object. It is in most
  cases necessary to restrict the plates at most about one
  dozen wells. See \code{\link{[,OPMS}} for how to achieve
  this.}

  \item{as.labels}{List. Metadata to be joined and used to
  draw a legend.  Ignored if \code{NULL}.}

  \item{subset}{Character scalar. The parameter to plot.
  Only a single one can be selected. See
  \code{\link{param_names}} for the options.}

  \item{...}{Passed to \code{\link{ci_plot}}.}
}
\value{
  See \code{\link{ci_plot}}.
}
\description{
  Compare aggregated values with their confidence intervals
  between plates. This function can in most cases
  \strong{not} be applied to entire plates but to selected
  wells only.
}
\examples{
data(vaas_4)

# most of the parameters used here are explained under the data.frame
# method of ci_plot()
x <- ci_plot(vaas_4[, , 1:3], as.labels = list("Species", "Strain"),
   subset = "A", x = "bottomright", legend.field = NULL)
# note that the values on the y axes are drawn to scale
x
stopifnot(is.character(x), identical(length(x), 4L))
# ... and that the return value contains the legend (even if it is not drawn)
}
\references{
  Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P.
  Visualization and curve parameter estimation strategies
  for efficient exploration of Phenotype Microarray
  kinetics. PLoS ONE 2012; in press.
}
\seealso{
  plot

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{heat_map,matrix}},
  \code{\link{level_plot}}, \code{\link{level_plot,OPMS}},
  \code{\link{max_rgb_contrast}},
  \code{\link{select_colors}}, \code{\link{xy_plot}},
  \code{\link{xy_plot,OPMS}}
}
\keyword{hplot}

\docType{methods}
\alias{ci_plot,OPMS-method}
\usage{
  \S4method{ci_plot}{OPMS}(object, as.labels, subset = "A", ...) 
}
