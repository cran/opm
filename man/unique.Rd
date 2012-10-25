\name{unique}
\alias{unique}
\title{Make OPMS objects unique}
\arguments{
  \item{x}{\code{\link{OPMS}} or \code{\link{OPM}} object.}

  \item{incomparables}{Vector passed to
  \code{\link{duplicated}}. The default is \code{FALSE}.}

  \item{...}{Optional further arguments passed to
  \code{\link{duplicated}}.  See the examples.}
}
\value{
  \code{\link{OPMS}} or \code{\link{OPM}} object or
  \code{NULL}.
}
\description{
  Check whether duplicated \code{\link{OPM}} or
  \code{\link{OPMA}} objects are contained within an
  \code{\link{OPMS}} object and remove the duplicated ones.
  The \code{\link{OPM}} method just returns the object
  passed.
}
\examples{
## 'OPMS' method
data(vaas_4)
(x <- unique(vaas_4))
stopifnot(identical(x, vaas_4))
(x <- unique(c(vaas_4, vaas_4)))
stopifnot(identical(x, vaas_4))
(x <- unique(vaas_4, what = "Species"))
stopifnot(dim(x)[1L] < dim(vaas_4)[1L])
(x <- unique(vaas_4, what = list("Species", "Strain")))
stopifnot(identical(x, vaas_4))

## 'OPM' method
data(vaas_1)
(x <- unique(vaas_1))
stopifnot(identical(x, vaas_1))
(x <- unique(vaas_1, what = list("Species", "Strain")))
stopifnot(identical(x, vaas_1))
}
\seealso{
  base::unique

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}}
}
\keyword{manip}

\docType{methods}
\alias{unique-methods}
\alias{unique,OPM,ANY-method}
\alias{unique,OPMS,missing-method}
\alias{unique,OPMS,ANY-method}
\usage{
  \S4method{unique}{OPM,ANY}(x, incomparables, ...) 

  \S4method{unique}{OPMS,missing}(x, incomparables, ...) 

  \S4method{unique}{OPMS,ANY}(x, incomparables, ...) 

}
