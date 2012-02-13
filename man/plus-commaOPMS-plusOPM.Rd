\name{plus,OPMS+OPM}
\alias{plus,OPMS+OPM}
\title{Addition (OPMS+OPM version)}
\arguments{
  \item{e1}{\code{\link{OPMS}} object.}

  \item{e2}{\code{\link{OPM}} object.}
}
\value{
  \code{\link{OPMS}} object that contains the plates from
  both \code{e1} and \code{e2}.
}
\description{
  Add an \code{\link{OPM}} to an \code{\link{OPMS}} object.
  Raise an error if the two objects are incompatible.
}
\note{
  See \sQuote{See Also} for the other \sQuote{+} methods of
  the class.
}
\examples{
data("vaas_1")
data(vaas_4)
# not particularly useful: adding partially identical plates!
x <- vaas_4 + vaas_1
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
}
\seealso{
  +

  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus}},
  \code{\link{plus,OPM+OPMS}}, \code{\link{plus,OPM+list}},
  \code{\link{plus,OPMS+OPMS}},
  \code{\link{plus,OPMS+list}}
}
\keyword{manip}

\docType{methods}
\alias{plus,OPMS,OPM-method}
\alias{+,OPMS,OPM-method}
\usage{
  \S4method{+}{OPMS,OPM}(e1, e2) 
}
