\name{discretized}
\alias{discretized}
\title{Get discretized kinetic data}
\arguments{
  \item{object}{\code{\link{OPMD}} or \code{\link{OPMS}}
  object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Logical vector or matrix.
}
\description{
  This yields the discretized values of the curve parameter
  \sQuote{maximum height}.
}
\note{
  See \code{\link{do_disc}} for generating discretized
  data.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- discretized(vaas_1))
stopifnot(is.logical(x), !is.matrix(x), length(x) == dim(x)[2L])
stopifnot(names(x) == colnames(aggregated(vaas_1)))

# 'OPMS' method
data(vaas_4)
(x <- discretized(vaas_4))
stopifnot(is.logical(x), is.matrix(x), ncol(x) == dim(x)[2L])
stopifnot(colnames(x) == colnames(aggregated(vaas_1)))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{subset}}, \code{\link{summary}},
  \code{\link{well}}, \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{discretized-methods}
\alias{discretized,OPMD-method}
\alias{discretized,OPMS-method}
\usage{
  \S4method{discretized}{OPMD}(object) 

  \S4method{discretized}{OPMS}(object, ...) 

}
