\name{csv_data}
\alias{csv_data}
\title{Information from input CSV file}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{keys}{Character vector (or other objects usable as
  vector index). An optional sub-selection. If empty (the
  default), all \acronym{CSV} data are returned. By default
  it is an error to select non-existing items.}

  \item{strict}{Logical scalar indicating whether or not it
  is an error if \code{keys} are not found.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Named character vector.
}
\description{
  Information about the plate as originally read from the
  input \acronym{CSV} file. See \code{\link{read_opm}} and
  \code{\link{read_single_opm}} for reading such files.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- csv_data(vaas_1, "Setup Time")) # compare this to setup_time()
stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))

# 'OPMS' method
data(vaas_4)
(x <- csv_data(vaas_4, "Setup Time")) # one setup time per plate
stopifnot(is.character(x), length(x) == 4)
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{dim}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{subset}}, \code{\link{summary}},
  \code{\link{well}}, \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{csv_data-methods}
\alias{csv_data,OPM-method}
\alias{csv_data,OPMS-method}
\usage{
  \S4method{csv_data}{OPM}(object, keys = character(),
    strict = TRUE) 

  \S4method{csv_data}{OPMS}(object, ...) 

}
