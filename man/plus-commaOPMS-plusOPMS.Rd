\name{plus,OPMS+OPMS}
\alias{plus,OPMS+OPMS}
\title{Addition (OPMS+OPMS version)}
\arguments{
  \item{e1}{\code{\link{OPMS}} object.}

  \item{e2}{\code{\link{OPMS}} object.}
}
\value{
  \code{\link{OPMS}} object that contains the plates from
  both \code{e1} and \code{e2}.
}
\description{
  Add two \code{\link{OPMS}} objects together. Raise an
  error if the two are incompatible.
}
\note{
  See \sQuote{See Also} for the other \sQuote{+} methods of
  the class.
}
\examples{
data(vaas_4)
# not particularly useful: adding partially identical plates!
x <- vaas_4 + vaas_4
stopifnot(identical(dim(x), c(8L, dim(vaas_4)[-1L])))
}
\seealso{
  +

  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus}},
  \code{\link{plus,OPM+OPMS}}, \code{\link{plus,OPM+list}},
  \code{\link{plus,OPMS+OPM}}, \code{\link{plus,OPMS+list}}
}
\keyword{manip}

\docType{methods}
\alias{plus,OPMS,OPMS-method}
\alias{+,OPMS,OPMS-method}
\usage{
  \S4method{+}{OPMS,OPMS}(e1, e2) 
}
