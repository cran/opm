\name{metadata_chars,OPMS}
\alias{metadata_chars,OPMS}
\title{Get metadata characters (OPMS version)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{...}{Optional arguments to
  \code{\link{metadata_chars}}.}
}
\value{
  Character vector, sorted and made unique over all plates,
  containing itself as \code{names} attribute. See
  \code{\link{metadata_chars}} for further details.
}
\description{
  This just applies \code{\link{metadata_chars}} to all
  plates in an \code{\link{OPMS}} object in turn.
}
\examples{
data(vaas_4)
(x <- metadata_chars(vaas_4, values = TRUE)) # the values
(y <- metadata_chars(vaas_4, values = FALSE)) # the keys
stopifnot(length(x) > length(y))
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
  \code{\link{metadata_chars}}, \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{attribute}

\docType{methods}
\alias{metadata_chars,OPMS-method}
\usage{
  \S4method{metadata_chars}{OPMS}(object, ...) 
}
