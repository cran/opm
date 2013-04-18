\name{[<-}
\alias{[<-}
\title{Assign subset}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{i}{One to several plate indexes. Should be
  compatible with the length of \code{value}.}

  \item{j}{Must \strong{not} be set. See the examples.}

  \item{value}{Value to be assigned. \code{NULL} causes the
  selected plates to be removed. Alternatively,
  \code{\link{OPM}} or \code{\link{OPMS}} objects can be
  assigned, subject to the restrictions explained in the
  help entry of the \code{\link{OPMS}} class.}
}
\value{
  \code{value}.
}
\description{
  Assign subsets of \code{\link{OPMS}} objects.
}
\examples{
data(vaas_4)
copy <- vaas_4
copy[5] <- NULL # has no effect
stopifnot(identical(vaas_4, copy))
length(copy)
copy[2:3] <- NULL # removes these plates
length(copy)
stopifnot(length(vaas_4) == length(copy) + 2)
copy[1:4] <- vaas_4 # set all plates to the plates from 'vaas_4'
stopifnot(identical(vaas_4, copy))
copy[3] <- copy[3] # no change
stopifnot(identical(vaas_4, copy))
copy[3] <- copy[2] # now assign other plate
stopifnot(!identical(vaas_4, copy))
}
\seealso{
  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus}}
}
\keyword{manip}

\docType{methods}
\alias{bracket.set-methods}
\alias{bracket.set,OPMS,ANY,missing,NULL-method}
\alias{[<-,OPMS,ANY,missing,NULL-method}
\alias{bracket.set,OPMS,ANY,missing,OPM-method}
\alias{[<-,OPMS,ANY,missing,OPM-method}
\alias{bracket.set,OPMS,ANY,missing,OPMS-method}
\alias{[<-,OPMS,ANY,missing,OPMS-method}
\usage{
  \S4method{[}{OPMS,ANY,missing,NULL}(x, i, j) <- value

  \S4method{[}{OPMS,ANY,missing,OPM}(x, i, j) <- value

  \S4method{[}{OPMS,ANY,missing,OPMS}(x, i, j) <- value

}
