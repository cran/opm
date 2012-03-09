\name{opms}
\alias{opms}
\title{OPMS constructor}
\usage{
  opms(..., precomputed = TRUE, skip = FALSE)
}
\arguments{
  \item{...}{One to several objects which are either
  potentially nested lists of \code{\link{OPMS}},
  \code{\link{OPM}} or \code{\link{OPMA}} objects, or
  really nested lists whose sublists can be converted to an
  \code{\link{OPM}} or \code{\link{OPMA}} object.}

  \item{precomputed}{Logical scalar. If \code{TRUE},
  sublists have already been converted to one of the three
  classes. Otherwise, suitable sublists will be converted.}

  \item{skip}{Logical scalar. If \code{precomputed} is
  \code{TRUE}, silently skip non-list elements of nested
  lists? If \code{precomputed} is \code{FALSE}, silently
  skip objects that do not belong to the three target
  classes? Otherwise, an error is generated if such a list
  element is encountered.}
}
\value{
  \code{\link{OPMS}} object.
}
\description{
  Easily build an \code{\link{OPMS}} object.
}
\note{
  While otherwise rather flexible, this function will fail
  to construct an \code{\link{OPMS}} object if the plate
  types do not match (simply because such
  \code{\link{OPMS}} objects are disallowed).
}
\examples{
# Testing distinct OPM/OPMS combinations -- all should work
data(vaas_1, vaas_4)
x <- opms(vaas_1, vaas_1)
stopifnot(is(x, "OPMS"), length(x) == 2L)
x <- opms(vaas_4, vaas_1)
stopifnot(is(x, "OPMS"), length(x) == 5L)
x <- opms(vaas_1, vaas_4)
stopifnot(is(x, "OPMS"), length(x) == 5L)
x <- opms(vaas_4, vaas_4)
stopifnot(is(x, "OPMS"), length(x) == 8L)
}
\seealso{
  Other combination-functions: \code{\link{c}},
  \code{\link{plus}}
}
\keyword{manip}

