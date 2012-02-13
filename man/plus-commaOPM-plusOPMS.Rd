\name{plus,OPM+OPMS}
\alias{plus,OPM+OPMS}
\title{Addition (OPM+OPMS version)}
\arguments{
  \item{e1}{\code{\link{OPM}} object.}

  \item{e2}{\code{\link{OPMS}} object.}
}
\value{
  \code{\link{OPMS}} object that contains the plates from
  both \code{e1} and \code{e2}.
}
\description{
  Add an \code{\link{OPMS}} to an \code{\link{OPM}} object.
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
x <- vaas_1 + vaas_4
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
}
\seealso{
  +

  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus}},
  \code{\link{plus,OPM+list}}, \code{\link{plus,OPMS+OPM}},
  \code{\link{plus,OPMS+OPMS}},
  \code{\link{plus,OPMS+list}}
}
\keyword{manip}

\docType{methods}
\alias{plus,OPM,OPMS-method}
\alias{+,OPM,OPMS-method}
\usage{
  \S4method{+}{OPM,OPMS}(e1, e2) 
}
