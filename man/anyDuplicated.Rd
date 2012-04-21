\name{anyDuplicated}
\alias{anyDuplicated}
\title{Determine whether plates are duplicated}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{incomparables}{Vector of values that cannot be
  compared. See \code{anyDuplicated} from the \pkg{base}
  package for details.}

  \item{fromLast}{Logical scalar. See below and
  \code{anyDuplicated} from the \pkg{base} package for
  details.}

  \item{...}{Optional other arguments passed to that
  function.}
}
\value{
  Integer scalar. \code{0} if no values are duplicated, the
  index of the first or last (depending on \code{fromLast})
  duplicated object otherwise.
}
\description{
  Check whether duplicated \code{\link{OPM}} or
  \code{\link{OPMA}} objects are contained within an
  \code{\link{OPMS}} object.
}
\examples{
data(vaas_4)
stopifnot(!anyDuplicated(vaas_4))
x <- vaas_4[c(1, 1)]
stopifnot(anyDuplicated(x) == 2)
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
  \code{\link{setup_time}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{anyDuplicated-methods}
\alias{anyDuplicated,OPMS-method}
\usage{
  \S4method{anyDuplicated}{OPMS}(x, incomparables = FALSE,
    fromLast = FALSE, ...) 

}
