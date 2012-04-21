\name{seq}
\alias{seq}
\title{Sequence of plate indexes}
\arguments{
  \item{...}{\code{\link{OPMS}} objects. Several ones can
  be provided, but only the first one is used. Passing
  \code{\link{OPMS}} objects together with other objects to
  this function makes the results uninterpretable.}
}
\value{
  Integer vector (starting with 1 and at least of length
  2).
}
\description{
  Get the indexes of all plates contained in an
  \code{\link{OPMS}} object. This is mainly useful for
  looping over such objects. See \code{\link{[}} for a
  loop-construct usage example, and note that
  \code{\link{oapply}} is also available.
}
\examples{
data(vaas_4)
(x <- seq(vaas_4))
stopifnot(identical(x, 1:4))
}
\seealso{
  base::seq

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{setup_time}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{seq-methods}
\alias{seq,OPMS-method}
\usage{
  \S4method{seq}{OPMS}(...) 

}
