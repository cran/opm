\name{plates}
\alias{oapply}
\alias{plates}
\title{Get available plates or apply function to them}
\arguments{
  \item{object}{List, \code{\link{OPM}}, \code{\link{OPMS}}
  or \code{\link{MOPMX}} object.}

  \item{fun}{A function. Should accept an \code{\link{OPM}}
  object as first argument.}

  \item{...}{Optional other arguments passed to
  \code{fun}.}

  \item{simplify}{Logical scalar. If \code{FALSE}, the
  result is a list. If \code{TRUE}, it is attempted to
  simplify this to a vector, matrix or \code{\link{OPMS}}
  object (if the result is a list of \code{\link{OPM}} or
  \code{\link{OPMA}} objects). If this is impossible, a
  list is returned.  The \code{\link{MOPMX}} method tries
  creating a \code{\link{MOPMX}} object again after
  removing \code{NULL} values, if any.}
}
\value{
  For \code{plates}, a list of \code{\link{OPM}} objects
  (may be empty instead if \code{object} is a list). The
  result of \code{oapply} depends on \code{fun} and
  \code{simplify}: a list, vector, matrix or
  \code{\link{OPMS}} object are possible outcomes.
}
\description{
  Get all \code{plates} contained in an \code{\link{OPMS}}
  object or a list, or create a list containing a single
  \code{\link{OPM}} object as element, or apply a function
  to a collection of \code{\link{OPM}} objects.
}
\details{
  The list method of \code{plates} traverses the input
  recursively and skips all objects of other classes than
  \code{\link{OPM}}. See also \code{\link{opms}}, which is
  somewhat similar but more flexible.

  \code{oapply} applies a function to all \code{\link{OPM}}
  objects within an \code{\link{OPMS}} object. Optionally
  it simplifies the result to an \code{\link{OPMS}} object
  if possible, or other structures simpler than a list. The
  \code{\link{OPM}} method of simply applies \code{fun}
  once (to \code{object}).
}
\examples{
# plates(), 'OPM' method
summary(x <- plates(vaas_1)) # => list of OPM objects
stopifnot(is.list(x), length(x) == 1L, sapply(x, inherits, what = "OPM"))

# plates(), 'OPMS' method
summary(x <- plates(vaas_4)) # => list of OPM objects
stopifnot(is.list(x), length(x) == 4L, sapply(x, inherits, what = "OPM"))

# plates(), list method
x <- list(vaas_1, letters, vaas_4, 1:10)
summary(x <- plates(x)) # => list of OPM objects
stopifnot(is.list(x), length(x) == 5, sapply(x, inherits, what = "OPM"))

## oapply()
summary(x <- oapply(vaas_4, identity)) # trivial
stopifnot(identical(x, vaas_4))
summary(x <- oapply(vaas_4, identity, simplify = FALSE)) # => yields list
stopifnot(is.list(x), length(x) == 4, sapply(x, class) == "OPMD")
}
\seealso{
  base::list base::as.list base::sapply

  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{extract}}, \code{\link{extract_columns}},
  \code{\link{flatten}}, \code{\link{merge}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{split}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{attribute}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{oapply-methods}
\alias{oapply,MOPMX-method}
\alias{oapply,OPM-method}
\alias{oapply,OPMS-method}
\alias{plates-methods}
\alias{plates,OPM-method}
\alias{plates,OPMS-method}
\alias{plates,list-method}
\usage{
  \S4method{oapply}{MOPMX}(object, fun, ...,
    simplify = TRUE) 
  \S4method{oapply}{OPM}(object, fun, ...,
    simplify = TRUE) 
  \S4method{oapply}{OPMS}(object, fun, ...,
    simplify = TRUE) 

  \S4method{plates}{OPM}(object) 
  \S4method{plates}{OPMS}(object) 
  \S4method{plates}{list}(object) 
}
%% END INSERTION BY repair_S4_docu.rb
