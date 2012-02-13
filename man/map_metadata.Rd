\name{map_metadata}
\alias{map_metadata}
\title{Map metadata using a character vector}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{mapping}{Character vector. See
  \code{\link{map_values,character}} for details.
  \code{\link{metadata_chars}} can be used to create a
  template for such a vector.}

  \item{values}{Logical scalar. If \code{FALSE}, metadata
  names, not values, are mapped, and \code{coerce} is
  ignored (names are always of class \sQuote{character} and
  need not be coerced).}

  \item{coerce}{Character vector. See
  \code{\link{map_values}} for details.}
}
\value{
  \code{\link{WMD}} object with modified metadata.
}
\description{
  Map meta-information stored together with the
  measurements by using a \sQuote{character} vector-based
  mapping.
}
\examples{
data(vaas_1)

# Mapping a value
map <- metadata_chars(vaas_1)
map["First replicate"] <- "Rep. 1"
copy <- map_metadata(vaas_1, map)
stopifnot(identical(names(metadata(copy)), names(metadata(vaas_1))))
stopifnot(!identical(metadata(copy, "Experiment"),
  metadata(vaas_1, "Experiment")))

# Mapping a name
map <- metadata_chars(vaas_1, values = FALSE)
map["Plate number"] <- "Plate no."
copy <- map_metadata(vaas_1, map, values = FALSE)
stopifnot(!identical(names(metadata(copy)), names(metadata(vaas_1))))
}
\seealso{
  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata,OPMS}},
  \code{\link{map_metadata,WMD+function}},
  \code{\link{metadata}}, \code{\link{metadata-set}},
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
\keyword{manip}

\docType{methods}
\alias{map_metadata-methods}
\alias{map_metadata,WMD,character-method}
\usage{
  \S4method{map_metadata}{WMD,character}(object, mapping,
    values = TRUE, coerce = "factor") 
}
