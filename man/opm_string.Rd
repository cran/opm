\name{opm_string}
\alias{opm_string}
\title{Name of this package}
\usage{
  opm_string(version = FALSE)
}
\arguments{
  \item{version}{Logical scalar indicating whether or not
  to append version information.}
}
\value{
  One- or two-element character scalar.
}
\description{
  Generate character string describing this package,
  optionally with its version.
}
\details{
  The version might be wrong if this function is called
  after loading the files with \code{source} instead of
  \code{library}. If it is unavailable, it is silently
  ignored.
}
\keyword{internal}

