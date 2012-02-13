\name{plus}
\alias{plus}
\title{Addition}
\arguments{
  \item{e1}{\code{\link{OPM}} object.}

  \item{e2}{\code{\link{OPM}} object.}
}
\value{
  \code{\link{OPMS}} object that contains the plates from
  both \code{e1} and \code{e2}.
}
\description{
  Combine two \code{\link{OPM}} objects to an
  \code{\link{OPMS}} object. Raise an error if the two
  objects are incompatible.
}
\note{
  See \sQuote{See Also} for the other \sQuote{+} methods of
  the class.
}
\examples{
data("vaas_1")
x <- vaas_1 + vaas_1 # not particularly useful: adding identical plates!
stopifnot(identical(dim(x), c(2L, dim(vaas_1))))
}
\seealso{
  +

  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus,OPM+OPMS}},
  \code{\link{plus,OPM+list}}, \code{\link{plus,OPMS+OPM}},
  \code{\link{plus,OPMS+OPMS}},
  \code{\link{plus,OPMS+list}}
}
\keyword{manip}

\docType{methods}
\alias{plus-methods}
\alias{plus,OPM,OPM-method}
\alias{+,OPM,OPM-method}
\usage{
  \S4method{+}{OPM,OPM}(e1, e2) 
}
