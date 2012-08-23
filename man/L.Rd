\name{L}
\alias{L}
\title{Assert a length}
\usage{
  L(x, wanted = 1L,
    msg = "need object '\%s' of length \%i")
}
\arguments{
  \item{x}{R objects to test.}

  \item{wanted}{Integer scalar giving the desired length.
  Note that this can \strong{not} be given as scalar with
  \code{storage.mode} evaluating to \sQuote{double}.}

  \item{msg}{Error message passed to \code{sprintf},
  receiving the name of the calling function, the name of
  \code{x} and the value of \code{wanted} as additional
  arguments.}
}
\value{
  If successful, \code{x}, but an error message is raised
  if \code{length(x)} is not identical to \code{wanted}.
}
\description{
  Raise an error if a given R object does not have the
  specified length. This is mainly used to generate more
  meaningful error messages related to function arguments.
}
\keyword{internal}

