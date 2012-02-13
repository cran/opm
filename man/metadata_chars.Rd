\name{metadata_chars}
\alias{metadata_chars}
\title{Get metadata characters}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{values}{Logical scalar. If \code{FALSE}, metadata
  names, not values, are collected, and \code{coerce} is
  ignored (names are always of class \sQuote{character} and
  need not be coerced).}

  \item{coerce}{Character vector containing the names of
  classes that should also be collected (and coerced to
  \sQuote{character}).}
}
\value{
  Character vector, sorted and made unique. Original
  \code{names} attributes, if any, are dropped and replaced
  by the character vector itself. (This might be convenient
  regarding its use with \code{\link{map_metadata}}.)
}
\description{
  Collect all \sQuote{character} entries from the
  meta-information stored together with the measurements.
  Optionally coerce data of other types. The result can be
  used to create a mapping for \code{\link{map_metadata}}.
}
\examples{
data(vaas_1)
(x <- metadata_chars(vaas_1, values = FALSE))
stopifnot(names(x) == x)
(x <- metadata_chars(vaas_1, values = TRUE))
stopifnot(names(x) == x)
# See map_metadata() for a potential usage of the metadata_chars() result
}
\seealso{
  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}},
  \code{\link{map_metadata,OPMS}},
  \code{\link{map_metadata,WMD+function}},
  \code{\link{metadata}}, \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{attribute}

\docType{methods}
\alias{metadata_chars-methods}
\alias{metadata_chars,WMD-method}
\usage{
  \S4method{metadata_chars}{WMD}(object, values = TRUE, 
    coerce = "factor") 
}
