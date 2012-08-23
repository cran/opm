\name{metadata}
\alias{metadata}
\title{Get metadata}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{key}{If \code{NULL} or otherwise empty, return all
  metadata. If a non-empty list, treat it as list of keys
  and return list of corresponding metadata values. Here,
  character vectors of length > 1 can be used to query
  nested metadata lists. If neither empty nor a list (i.e.
  usually a character or numeric scalar), treat \code{key}
  as a single list key.  Factors are converted to
  \sQuote{character} mode.}

  \item{exact}{Logical scalar. Use exact or partial
  matching of keys? Has no effect if \code{key} is empty.}

  \item{strict}{Logical scalar. Is it an error if a
  \code{NULL} value results from fetching a metadata key?}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  List (empty if metadata were not set or if subselection
  using \code{key} did not result).
}
\description{
  Get meta-information stored together with the data.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- metadata(vaas_1, "Strain"))
stopifnot(identical(x, "DSM30083T"))

# 'OPMS' method
data(vaas_4)
(x <- metadata(vaas_4, "Strain"))
stopifnot(x == c("DSM18039", "DSM30083T", "DSM1707", "429SC1"))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{show}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{metadata-methods}
\alias{metadata,WMD-method}
\alias{metadata,OPMS-method}
\usage{
  \S4method{metadata}{WMD}(object, key = NULL, exact = TRUE,
    strict = FALSE) 

  \S4method{metadata}{OPMS}(object, ...) 

}
