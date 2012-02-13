\name{summary,OPMS}
\alias{summary,OPMS}
\title{Summary (OPMS version)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{...}{See \code{\link{summary}}.}
}
\value{
  List of named lists (one per plate), returned invisibly.
}
\description{
  Print summary information to screen.
}
\examples{
data(vaas_4)
x <- summary(vaas_4)
stopifnot(is.list(x), length(x) == 4L, all(sapply(x, is.list)))
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
  \code{\link{setup_time}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{summary,OPMS-method}
\usage{
  \S4method{summary}{OPMS}(object, ...) 
}
