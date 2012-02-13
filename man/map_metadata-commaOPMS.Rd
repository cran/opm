\name{map_metadata,OPMS}
\alias{map_metadata,OPMS}
\title{Map metadata (OPMS version)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{mapping}{See \code{\link{map_metadata}} and
  \code{\link{map_metadata,WMD+function}} for possible
  values.}

  \item{...}{Optional other argument passed to one of these
  two functions.}
}
\value{
  \code{\link{OPMS}} object.
}
\description{
  This applies the eponymous \code{\link{OPM}} methods to
  all plates in turn and returns an \code{\link{OPMS}}
  object with accordingly modified metadata.
}
\examples{
data(vaas_4)
# using a function
copy <- map_metadata(vaas_4, identity)
stopifnot(identical(copy, vaas_4))
copy <- map_metadata(vaas_4, identity, values = FALSE)
stopifnot(identical(copy, vaas_4))
copy <- map_metadata(vaas_4, function(x) paste(x, "!"), values = FALSE)
(x <- metadata_chars(vaas_4, values = FALSE))
(y <- metadata_chars(copy, values = FALSE))
stopifnot(identical(as.character(y), paste(x, "!")))

# using a character vector
map <- metadata_chars(vaas_4)
map["First replicate"] <- "Rep. 1"
copy <- map_metadata(vaas_4, map)
x <- metadata(vaas_4, "Experiment")
stopifnot(x == "First replicate")
y <- metadata(copy, "Experiment")
stopifnot(y == "Rep. 1")
}
\seealso{
  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}},
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
\alias{map_metadata,OPMS,ANY-method}
\usage{
  \S4method{map_metadata}{OPMS,ANY}(object, mapping, ...) 
}
