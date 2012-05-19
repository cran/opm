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
  \code{\link{summary}} method for these objects.
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

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{show-methods}
\alias{show,OPM-method}
\alias{show,OPMS-method}
\usage{
  \S4method{show}{OPM}(object) 

  \S4method{show}{OPMS}(object) 

}
