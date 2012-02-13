\name{plus,OPMS+list}
\alias{plus,OPMS+list}
\title{Addition (OPMS+list version)}
\arguments{
  \item{e1}{\code{\link{OPMS}} object.}

  \item{e2}{List of \code{\link{OPM}} objects.}
}
\value{
  \code{\link{OPMS}} object that contains the plates from
  both \code{e1} and \code{e2}.
}
\description{
  Add a list of \code{\link{OPM}} objects to an
  \code{\link{OPMS}} object. Raise an error if any of the
  contained objects are incompatible.
}
\note{
  See \sQuote{See Also} for the other \sQuote{+} methods of
  the class.
}
\examples{
data("vaas_1")
data(vaas_4)
# not particularly useful: adding partially identical plates!
x <- vaas_4 + list(vaas_1)
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
}
\seealso{
  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus}},
  \code{\link{plus,OPM+OPMS}}, \code{\link{plus,OPM+list}},
  \code{\link{plus,OPMS+OPM}}, \code{\link{plus,OPMS+OPMS}}
}
\keyword{manip}

\docType{methods}
\alias{plus,OPMS,list-method}
\alias{+,OPMS,list-method}
\usage{
  \S4method{+}{OPMS,list}(e1, e2) 
}
