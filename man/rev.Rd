\name{rev}
\alias{rev}
\title{Revert OPMS objects}
\arguments{
  \item{x}{\code{\link{OPMS}} or \code{\link{OPM}} object.}
}
\value{
  \code{\link{OPMS}} object with the reversed order of
  plates, or or \code{\link{OPM}} object.
}
\description{
  Revert the order of plates within an \code{\link{OPMS}}
  object. This should be slightly more efficient than
  calling the default \code{rev} method. There is also an
  \code{\link{OPM}} method which just returns the input
  data (to avoid destructive effects due to the way the
  default \code{rev} interacts with \code{\link{OPM}}
  indexing).
}
\examples{
## 'OPMS' method
data(vaas_4)
dim(x <- rev(vaas_4))
stopifnot(dim(x) == dim(vaas_4), !identical(x, vaas_4))
stopifnot(identical(rev(x), vaas_4))

## 'OPMS' method
data(vaas_1)
dim(x <- rev(vaas_1)) # trivial
stopifnot(identical(x, vaas_1))
}
\seealso{
  base::rev

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{manip}

\docType{methods}
\alias{rev-methods}
\alias{rev,OPM-method}
\alias{rev,OPMS-method}
\usage{
  \S4method{rev}{OPM}(x) 

  \S4method{rev}{OPMS}(x) 

}
