\name{duplicated}
\alias{duplicated}
\title{Determine duplicated plates}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{incomparables}{Vector of values that cannot be
  compared. See \code{duplicated} from the \pkg{base}
  package for details.}

  \item{...}{Optional other arguments passed to that
  function.}
}
\value{
  Logical vector.
}
\description{
  Check whether duplicated \code{\link{OPM}} or
  \code{\link{OPMA}} objects are contained within an
  \code{\link{OPMS}} object.
}
\examples{
data(vaas_4)
stopifnot(!duplicated(vaas_4))
x <- vaas_4[c(1, 1)]
stopifnot(c(FALSE, TRUE) == duplicated(x))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
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
\alias{duplicated-methods}
\alias{duplicated,OPMS-method}
\usage{
  \S4method{duplicated}{OPMS}(x, incomparables = FALSE, ...) 

}
