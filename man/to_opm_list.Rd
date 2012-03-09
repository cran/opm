\name{to_opm_list}
\alias{to_opm_list}
\title{Convert to OPM list}
\arguments{
  \item{List}{the objects that can be passed to
  \code{\link{opms}}}

  \item{precomputed}{Logical scalar. See
  \code{\link{opms}}.}

  \item{skip}{Logical scalar. See \code{\link{opms}}.}
}
\value{
  List.
}
\description{
  Convert to list of \code{\link{OPM}} objects. Used for
  building an \code{\link{OPMS}} object. This method is
  used by \code{\link{opms}} and \code{\link{try_opms}}.
}
\keyword{internal}

\docType{methods}
\alias{to_opm_list,list-method}
\usage{
  \S4method{to_opm_list}{list}(object, precomputed = TRUE,
    skip = FALSE) 

}
