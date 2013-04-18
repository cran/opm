\name{rep}
\alias{rep}
\title{Repeat OPMS objects}
\arguments{
  \item{x}{\code{\link{OPMS}} or \code{\link{OPM}} object.}

  \item{...}{Optional parameters passed to \code{rep} from
  the \pkg{base} package.}
}
\value{
  \code{\link{OPMS}} object with another number of plates,
  or or \code{\link{OPM}} object, or \code{NULL}.
}
\description{
  Repeat \code{\link{OPMS}} or \code{\link{OPM}} objects
  zero times, once, or several times, and accordingly
  create a novel \code{\link{OPMS}} or \code{\link{OPM}}
  object (\code{NULL} if zero length is chosen).
}
\examples{
## 'OPMS' method
data(vaas_4)
dim(x <- rep(vaas_4))
stopifnot(identical(x, vaas_4))
dim(x <- rep(vaas_4, times = 2))
stopifnot(length(x) == length(vaas_4) * 2)
dim(y <- rep(vaas_4, each = 2))
stopifnot(length(y) == length(vaas_4) * 2, !identical(x, y))
stopifnot(is.null(rep(vaas_4, 0)))

## 'OPM' method
data(vaas_1)
dim(x <- rep(vaas_1, 1))
stopifnot(identical(x, vaas_1))
dim(x <- rep(vaas_1, 2)) # conversion to OPMS if > 1 element
stopifnot(length(x) == 2, is(x, "OPMS"))
stopifnot(is.null(rep(vaas_4, 0)))
}
\seealso{
  base::rep

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{manip}

\docType{methods}
\alias{rep-methods}
\alias{rep,OPM-method}
\alias{rep,OPMS-method}
\usage{
  \S4method{rep}{OPM}(x, ...) 

  \S4method{rep}{OPMS}(x, ...) 

}
