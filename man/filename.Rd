\name{filename}
\alias{filename}
\title{Original input filename}
\arguments{
  \item{object}{\code{\link{OPM}} object.}
}
\value{
  Character scalar.
}
\description{
  Get the name of the original CSV input file. This is a
  convenience function for one of the more important
  entries of \code{\link{csv_data}}.
}
\examples{
data(vaas_1)
(x <- filename(vaas_1))
stopifnot(is.character(x), length(x) == 1L)
}
\seealso{
  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{max,OPMS}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{filename-methods}
\alias{filename,OPM-method}
\usage{
  \S4method{filename}{OPM}(object) 
}
