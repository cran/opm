\name{minmax,OPMS}
\alias{minmax,OPMS}
\title{Smallest maximum (OPMS version)}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{...}{See \code{\link{minmax}}.}

  \item{na.rm}{See \code{\link{max}}.}
}
\value{
  Numeric scalar.
}
\description{
  Get the smallest maximum among all wells of all plates
  stored in an \code{\link{OPMS}} object. This works by
  calling \code{\link{minmax}} on all plates and then
  determining the overall minimum.
}
\examples{
data(vaas_4)
(x <- max(vaas_4))
(y <- minmax(vaas_4))
stopifnot(x > y)
}
\seealso{
  min max

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{max,OPMS}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}
\keyword{dplot}

\docType{methods}
\alias{minmax,OPMS-method}
\usage{
  \S4method{minmax}{OPMS}(x, ..., na.rm = FALSE) 
}
