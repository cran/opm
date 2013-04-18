\name{try_opms}
\alias{try_opms}
\alias{try_opms.list}
\title{Convert list to OPMS}
\usage{
  try_opms(object, ...)

  \method{try_opms}{list} (object, precomputed = TRUE,
    skip = FALSE)
}
\arguments{
  \item{object}{List.}

  \item{precomputed}{Logical scalar. See
  \code{\link{opms}}.}

  \item{skip}{Logical scalar. See \code{\link{opms}}.}
}
\value{
  \code{\link{OPMS}} object (if conversions was successful)
  or just the input \code{object} (if conversions was
  unsuccessful).
}
\description{
  Conditionally convert a list to an \code{\link{OPMS}}
  object. This method is used by \code{\link{c}}.
}
\keyword{internal}

