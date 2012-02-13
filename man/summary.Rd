\name{summary}
\alias{summary}
\title{Summary}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{...}{Optional arguments passed to \code{formatDL}.}
}
\value{
  Named list, returned invisibly. The \sQuote{metadata}
  entry is the number of non-list elements in
  \code{\link{metadata}}.
}
\description{
  Print summary information to screen.
}
\examples{
data(vaas_1)
x <- summary(vaas_1)
stopifnot(is.list(x))
}
\seealso{
  summary

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{max,OPMS}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{minmax,OPMS}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{setup_time}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{summary-methods}
\alias{summary,OPM-method}
\usage{
  \S4method{summary}{OPM}(object, ...) 
}
