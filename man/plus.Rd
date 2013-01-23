\name{plus}
\alias{plus}
\title{Addition}
\arguments{
  \item{e1}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{e2}{\code{\link{OPM}} or \code{\link{OPMS}} object,
  or list.}
}
\value{
  \code{\link{OPMS}} object that contains the plates from
  both \code{e1} and \code{e2}.
}
\description{
  Combine an \code{\link{OPM}} or \code{\link{OPMS}} object
  with another \code{\link{OPM}} or \code{\link{OPMS}}
  object or a list of such objects. Raise an error if the
  objects are incompatible.
}
\examples{
data(vaas_1)
data(vaas_4)
# the examples do not show particularly useful additions, as the plates
# are either entirely or partially identical

# OPM+OPM method
summary(x <- vaas_1 + vaas_1)
stopifnot(identical(dim(x), c(2L, dim(vaas_1))))

# OPM+OPMS method
summary(x <- vaas_1 + vaas_4)
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))

# OPM+list method
summary(x <- vaas_1 + list(vaas_1, vaas_1))
stopifnot(identical(dim(x), c(3L, dim(vaas_1))))

# OPMS+OPMS method
summary(x <- vaas_4 + vaas_4)
stopifnot(identical(dim(x), c(8L, dim(vaas_4)[-1L])))

# OPMS+OPM method
summary(x <- vaas_4 + vaas_1)
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))

# OPMS+list method
summary(x <- vaas_4 + list(vaas_1))
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
}
\seealso{
  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}
}
\keyword{manip}

\docType{methods}
\alias{plus-methods}
\alias{plus,OPM,OPM-method}
\alias{+,OPM,OPM-method}
\alias{plus,OPM,OPMS-method}
\alias{+,OPM,OPMS-method}
\alias{plus,OPM,list-method}
\alias{+,OPM,list-method}
\alias{plus,OPMS,OPMS-method}
\alias{+,OPMS,OPMS-method}
\alias{plus,OPMS,OPM-method}
\alias{+,OPMS,OPM-method}
\alias{plus,OPMS,list-method}
\alias{+,OPMS,list-method}
\usage{
  \S4method{+}{OPM,OPM}(e1, e2) 

  \S4method{+}{OPM,OPMS}(e1, e2) 

  \S4method{+}{OPM,list}(e1, e2) 

  \S4method{+}{OPMS,OPMS}(e1, e2) 

  \S4method{+}{OPMS,OPM}(e1, e2) 

  \S4method{+}{OPMS,list}(e1, e2) 

}
