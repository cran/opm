\name{csv_data}
\alias{csv_data}
\title{Information from input CSV file}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{keys}{Character vector (or other objects useable as
  vector index). An optional subselection. If empty (the
  default), all CSV data are returned.  It is an error to
  select non-existing items.}
}
\value{
  Named character vector.
}
\description{
  Information about the plate as originally read from the
  input CSV file. See \code{\link{read_opm}} and
  \code{\link{read_single_opm}} for reading such files.
}
\examples{
data(vaas_1)
# compare this to setup_time()
(x <- csv_data(vaas_1, "Setup Time"))
stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))
}
\seealso{
  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{dim}}, \code{\link{dim,OPMS}},
  \code{\link{filename}}, \code{\link{has_aggr}},
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
\alias{csv_data-methods}
\alias{csv_data,OPM-method}
\usage{
  \S4method{csv_data}{OPM}(object, keys = character()) 
}
