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
  as a single list key.}

  \item{exact}{Logical scalar. Use exact or partial
  matching of keys? Has no effect if \code{key} is empty.}

  \item{strict}{Logical scalar. Is it an error if a
  \code{NULL} value results from fetching a metadata key?}
}
\value{
  List (empty if metadata were not set or if subselection
  using \code{key} did not result).
}
\description{
  Get meta-information stored together with the data.
}
\examples{
data(vaas_1)
(x <- metadata(vaas_1, "Strain"))
stopifnot(identical(x, "DSM30083T"))
}
\seealso{
  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{max,OPMS}}, \code{\link{measurements}},
  \code{\link{minmax}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}

  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}},
  \code{\link{map_metadata,OPMS}},
  \code{\link{map_metadata,WMD+function}},
  \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{attribute}

\docType{methods}
\alias{metadata-methods}
\alias{metadata,WMD-method}
\usage{
  \S4method{metadata}{WMD}(object, key = NULL, exact = TRUE, 
    strict = FALSE) 
}
