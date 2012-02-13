\name{c}
\alias{c}
\title{Combination}
\arguments{
  \item{x}{\code{\link{OPMX}} object.}

  \item{...}{Other R objects.}

  \item{recursive}{Logical scalar. See \code{c} from the
  \pkg{base} package.}
}
\value{
  \code{\link{OPMS}} object, list, or \code{\link{OPM}}
  object (if \code{...} is not given and \code{x} is such
  an object).
}
\description{
  Combine a \code{\link{OPM}} or \code{\link{OPMS}} object
  with other objects. If possible, create an
  \code{\link{OPMS}} object, otherwise return a list.
}
\examples{
data(vaas_1)
data(vaas_4)

# Adding nothing
x <- c(vaas_1)
stopifnot(identical(x, vaas_1))
x <- c(vaas_4)
stopifnot(identical(x, vaas_4))

# not particularly useful: adding identical plates!
x <- c(vaas_1, vaas_1) # yields a two-plate OPMS object
stopifnot(identical(dim(x), c(2L, dim(vaas_1))))

# also not particularly useful: adding partially identical plates!
x <- c(vaas_4, vaas_1)
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
}
\seealso{
  c

  Other combination-functions: \code{\link{opms}},
  \code{\link{plus}}, \code{\link{plus,OPM+OPMS}},
  \code{\link{plus,OPM+list}}, \code{\link{plus,OPMS+OPM}},
  \code{\link{plus,OPMS+OPMS}},
  \code{\link{plus,OPMS+list}}
}
\keyword{manip}

\docType{methods}
\alias{c-methods}
\alias{c,OPMX-method}
\usage{
  \S4method{c}{OPMX}(x, ..., recursive = FALSE) 
}
