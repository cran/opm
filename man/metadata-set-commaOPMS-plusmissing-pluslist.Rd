\name{metadata-set,OPMS+missing+list}
\alias{metadata-set,OPMS+missing+list}
\alias{metadata<-,OPMS+missing+list}
\title{Replace metadata (OPMS version)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{value}{List. Values to be set as metadata. Previous
  metadata, if any, are replaced entirely by \code{value}
  for all plates.}
}
\value{
  \code{value}.
}
\description{
  Set the meta-information stored together with the
  measurements for all plates at once. This version
  replaces all metadata by its argument.
}
\note{
  See \sQuote{See Also} for the other \sQuote{metadata<-}
  methods.
}
\examples{
data(vaas_4)
copy <- vaas_4
(metadata(copy) <- list(x = -99))
stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))
# See the metadata()<- methods of the WMD class for further examples. The
# OPMS version simply works by applying the mapping to each plate in the
# same way.
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
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other setter-functions: \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}}
}
\keyword{manip}

\docType{methods}
\alias{metadata-set,OPMS,missing,list-method}
\alias{metadata<-,OPMS,missing,list-method}
\usage{
  \S4method{metadata}{OPMS,missing,list}(object) <- value
}
