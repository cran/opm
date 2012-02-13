\name{assert_length}
\alias{assert_length}
\title{Assert length}
\usage{
  assert_length(..., .wanted = 1L)
}
\arguments{
  \item{...}{Any R objects to test.}

  \item{wanted}{Integer scalar giving the desired length.}
}
\value{
  The names of the arguments contained in \code{...},
  returned invisibly.
}
\description{
  Raise an error if a given R object does not have the
  specified length. This is mainly used to generate
  meaningful error messages for function arguments.
}
\keyword{internal}

