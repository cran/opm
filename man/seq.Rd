\name{seq}
\alias{seq}
\title{Sequence of plate indexes}
\arguments{
  \item{...}{\code{\link{OPMS}} objects. Several ones can
  be provided, but all but the first one are ignored. For
  reasons of comparability, the \code{\link{OPM}} method
  deliberately results in an error.}
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
# 'OPMS' method
data(vaas_4)
(x <- seq(vaas_4))
stopifnot(identical(x, 1:4)) # indexes for 4 plates
(y <- seq(vaas_4, letters, LETTERS)) # other arguments are ignored
stopifnot(identical(x, y))

# 'OPM' method
data(vaas_1)
(x <- try(seq(vaas_1), silent = TRUE)) # deliberately yields an error
stopifnot(inherits(x, "try-error"))
}
\seealso{
  base::seq

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{seq-methods}
\alias{seq,OPM-method}
\alias{seq,OPMS-method}
\usage{
  \S4method{seq}{OPM}(...) 

  \S4method{seq}{OPMS}(...) 

}
