\name{summary}
\alias{summary}
\title{Summary}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{...}{Optional arguments passed to \code{formatDL}.}
}
\value{
  For the \code{\link{OPM}} method, a named list, returned
  invisibly.  The \sQuote{metadata} entry is the number of
  non-list elements in \code{\link{metadata}}. For the
  \code{\link{OPMS}} method, a list of such lists (one per
  plate), also returned invisibly.
}
\description{
  Print summary information to screen.
}
\examples{
# OPM method
data(vaas_1)
x <- summary(vaas_1)
stopifnot(is.list(x))

# OPMS method
data(vaas_4)
x <- summary(vaas_4)
stopifnot(is.list(x), length(x) == 4L, all(sapply(x, is.list)))
}
\seealso{
  base::summary

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{summary-methods}
\alias{summary,OPM-method}
\alias{summary,OPMS-method}
\usage{
  \S4method{summary}{OPM}(object, ...) 

  \S4method{summary}{OPMS}(object, ...) 

}
