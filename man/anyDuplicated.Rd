\name{anyDuplicated}
\alias{anyDuplicated}
\title{Determine whether plates are duplicated}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{incomparables}{Vector passed to
  \code{\link{duplicated}}.}

  \item{...}{Optional arguments passed to
  \code{\link{duplicated}}.}
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
## 'OPMS' method
data(vaas_4)
stopifnot(anyDuplicated(vaas_4) == 0)
stopifnot(anyDuplicated(vaas_4, what = "Species") == 2)
x <- vaas_4[c(1, 1)]
stopifnot(anyDuplicated(x) == 2)

## 'OPM' method
data(vaas_1)
(x <- anyDuplicated(vaas_1))
stopifnot(identical(x, 0L))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{show}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{anyDuplicated-methods}
\alias{anyDuplicated,OPM-method}
\alias{anyDuplicated,OPMS-method}
\usage{
  \S4method{anyDuplicated}{OPM}(x, incomparables = FALSE, ...) 

  \S4method{anyDuplicated}{OPMS}(x, incomparables = FALSE, ...) 

}
