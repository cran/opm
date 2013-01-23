\name{anyDuplicated}
\alias{anyDuplicated}
\title{Determine whether plates are duplicated}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{incomparables}{Vector passed to
  \code{\link{duplicated}}. The default is \code{FALSE}.}

  \item{...}{Optional arguments passed to
  \code{\link{duplicated}}. See the examples.}
}
\value{
  Integer scalar. \code{0} if no values are duplicated, the
  index of the first or last (depending on \code{fromLast})
  duplicated object otherwise.
}
\description{
  Check whether duplicated \code{\link{OPM}} or
  \code{\link{OPMA}} objects are contained within an
  \code{\link{OPMS}} object.  For reasons of consistency,
  the \code{\link{OPM}} method always returns \code{0}.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- anyDuplicated(vaas_1))
stopifnot(identical(x, 0L))
(x <- anyDuplicated(vaas_1, what = list("Strain", "Species")))
stopifnot(identical(x, 0L))

# 'OPMS' method
data(vaas_4)
stopifnot(identical(anyDuplicated(vaas_4), 0L))
stopifnot(identical(anyDuplicated(vaas_4, what = list("Strain")), 0L))
stopifnot(identical(anyDuplicated(vaas_4, what = list("Species")), 2L))
x <- vaas_4[c(1, 1)]
stopifnot(identical(anyDuplicated(x), 2L))
}
\seealso{
  base::anyDuplicated

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
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
\alias{anyDuplicated-methods}
\alias{anyDuplicated,OPM,ANY-method}
\alias{anyDuplicated,OPMS,missing-method}
\alias{anyDuplicated,OPMS,ANY-method}
\usage{
  \S4method{anyDuplicated}{OPM,ANY}(x, incomparables, ...) 

  \S4method{anyDuplicated}{OPMS,missing}(x, incomparables, ...) 

  \S4method{anyDuplicated}{OPMS,ANY}(x, incomparables, ...) 

}
