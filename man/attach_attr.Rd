\name{attach_attr}
\alias{attach_attr}
\title{Attach slots}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{other}{Arbitrary other object.}
}
\value{
  \code{other} with additional attributes.
}
\description{
  Attach the contents of all slots, except the
  measurements, to another object. Useful in conversions
  (coercions).
}
\keyword{internal}

\docType{methods}
\alias{attach_attr-methods}
\alias{attach_attr,OPM-method}
\usage{
  \S4method{attach_attr}{OPM}(object, other) 

}
