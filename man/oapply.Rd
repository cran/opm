\name{oapply}
\alias{oapply}
\title{Apply method for OPMS objects}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{fun}{A function. Should expect an \code{\link{OPM}}
  (or \code{\link{OPMA}}) object as first argument.}

  \item{...}{Optional other arguments passed to
  \code{fun}.}

  \item{simplify}{Logical scalar. If \code{FALSE}, the
  result is a list. If \code{TRUE}, it is attempted to
  simplify the result to a vector or matrix or to an
  \code{\link{OPMS}} object (if the result is a list of
  \code{\link{OPM}} or \code{\link{OPMA}} objects). If this
  is impossible, a list is returned.}
}
\value{
  List, vector, matrix or \code{\link{OPMS}} object.
}
\description{
  Apply a function to all \code{\link{OPM}} or
  \code{\link{OPMA}} objects within an \code{\link{OPMS}}
  object. Optionally simplify the result to an
  \code{\link{OPMS}} object if possible, or other
  structures simpler than a list.
}
\examples{
data(vaas_4)
x <- oapply(vaas_4, identity)
stopifnot(identical(x, vaas_4))
x <- oapply(vaas_4, identity, simplify = FALSE)
stopifnot(is.list(x), length(x) == 4, sapply(x, class) == "OPMA")
}
\seealso{
  base::sapply

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{plates}}, \code{\link{to_yaml}}
}
\keyword{manip}

\docType{methods}
\alias{oapply-methods}
\alias{oapply,OPMS-method}
\usage{
  \S4method{oapply}{OPMS}(object, fun, ..., simplify = TRUE) 

}
