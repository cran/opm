\name{filename}
\alias{filename}
\title{Original input filename}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{...}{Optional arguments passed between the
  methods.}
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
# 'OPM' method
data(vaas_1)
(x <- filename(vaas_1))
stopifnot(is.character(x), length(x) == 1L)

# 'OPMS' method
data(vaas_4)
(x <- filename(vaas_4))
stopifnot(is.character(x), length(x) == 4L)
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{show}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{filename-methods}
\alias{filename,OPM-method}
\alias{filename,OPMS-method}
\usage{
  \S4method{filename}{OPM}(object) 

  \S4method{filename}{OPMS}(object, ...) 

}
