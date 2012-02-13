\name{plus,OPM+list}
\alias{plus,OPM+list}
\title{Addition (OPM+list version)}
\arguments{
  \item{e1}{\code{\link{OPM}} object.}

  \item{e2}{List of \code{\link{OPM}} objects.}
}
\value{
  \code{\link{OPMS}} object that contains the plates from
  both \code{e1} and \code{e2}.
}
\description{
  Add a list of \code{\link{OPM}} objects to an
  \code{\link{OPM}} object. Raise an error if any of the
  contained \code{\link{OPM}} objects are not compatible.
}
\note{
  See \sQuote{See Also} for the other \sQuote{+} methods of
  the class.
}
\examples{
data("vaas_1")
# not particularly useful: adding identical plates!
x <- vaas_1 + list(vaas_1, vaas_1)
stopifnot(identical(dim(x), c(3L, dim(vaas_1))))
}
\seealso{
  +

  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus}},
  \code{\link{plus,OPM+OPMS}}, \code{\link{plus,OPMS+OPM}},
  \code{\link{plus,OPMS+OPMS}},
  \code{\link{plus,OPMS+list}}
}
\keyword{manip}

\docType{methods}
\alias{plus,OPM,list-method}
\alias{+,OPM,list-method}
\usage{
  \S4method{+}{OPM,list}(e1, e2) 
}
