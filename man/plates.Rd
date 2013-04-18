\name{plates}
\alias{plates}
\title{Get available plates}
\arguments{
  \item{object}{List, \code{\link{OPM}} or
  \code{\link{OPMS}} object.}
}
\value{
  List of \code{\link{OPM}} objects (may be empty instead
  if \code{object} is a list).
}
\description{
  Get all plates contained in an \code{\link{OPMS}} object
  or a list, or create a list containing a single
  \code{\link{OPM}} object as element. The list method
  traverses the input recursively and skips all objects of
  other classes than \code{\link{OPM}}.
}
\note{
  See also \code{\link{opms}}, which is somewhat similar
  but more flexible.
}
\examples{
# 'OPM' method
data(vaas_1)
summary(x <- plates(vaas_1)) # => list of OPM objects
stopifnot(is.list(x), length(x) == 1L, sapply(x, inherits, what = "OPM"))

# 'OPMS' method
data(vaas_4)
summary(x <- plates(vaas_4)) # => list of OPM objects
stopifnot(is.list(x), length(x) == 4L, sapply(x, inherits, what = "OPM"))

# list method
x <- list(vaas_1, letters, vaas_4, 1:10)
summary(x <- plates(x)) # => list of OPM objects
stopifnot(is.list(x), length(x) == 5, sapply(x, inherits, what = "OPM"))
}
\seealso{
  base::list base::as.list

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{rep}},
  \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{attribute}

\docType{methods}
\alias{plates-methods}
\alias{plates,OPMS-method}
\alias{plates,OPM-method}
\alias{plates,list-method}
\usage{
  \S4method{plates}{OPMS}(object) 

  \S4method{plates}{OPM}(object) 

  \S4method{plates}{list}(object) 

}
