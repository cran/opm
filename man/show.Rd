\name{show}
\alias{show}
\title{Show OPM or OPMS objects}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}
}
\value{
  See \code{\link{summary}}.
}
\description{
  Display an \code{\link{OPM}} or \code{\link{OPMS}} object
  on screen. Currently this is just a wrapper for the
  \code{\link{summary}} method for these objects with an
  additional call to \code{\link{print}}.
}
\examples{
# OPMA method
data(vaas_1)
vaas_1

# OPMS method
data(vaas_4)
vaas_4
}
\seealso{
  methods::show base::print

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{radial_plot}}, \code{\link{select_colors}},
  \code{\link{xy_plot}}
}
\keyword{attribute}

\docType{methods}
\alias{show-methods}
\alias{show,OPMX-method}
\alias{show,CMAT-method}
\usage{
  \S4method{show}{OPMX}(object) 

  \S4method{show}{CMAT}(object) 

}
