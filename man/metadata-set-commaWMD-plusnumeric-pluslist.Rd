\name{metadata-set,WMD+numeric+list}
\alias{metadata-set,WMD+numeric+list}
\alias{metadata<-,WMD+numeric+list}
\title{Set metadata (WMD+numeric+list version)}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{key}{Numeric scalar. If positive, prepend
  \code{value} to old metadata. If negative, append
  \code{value} to old metadata. If zero, replace old
  metadata entirely by \code{value}.}

  \item{value}{List. Values to be prepended, appended or
  set as metadata, depending on \code{key}.}
}
\value{
  \code{value}.
}
\description{
  Set meta-information stored together with the data.
}
\note{
  See \sQuote{See Also} for the other \sQuote{metadata<-}
  methods.
}
\examples{
data(vaas_1)
copy <- vaas_1
metadata(copy, 1) <- list(Authors = "Vaas et al.")
stopifnot(length(metadata(copy)) > length(metadata(vaas_1)))
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
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other setter-functions: \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}}
}
\keyword{manip}

\docType{methods}
\alias{metadata-set,WMD,numeric,list-method}
\alias{metadata<-,WMD,numeric,list-method}
\usage{
  \S4method{metadata}{WMD,numeric,list}(object, key) <- value
}
