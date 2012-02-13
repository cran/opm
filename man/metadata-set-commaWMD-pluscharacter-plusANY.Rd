\name{metadata-set,WMD+character+ANY}
\alias{metadata-set,WMD+character+ANY}
\alias{metadata<-,WMD+character+ANY}
\title{Set metadata (WMD+character version)}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{key}{Character vector. Use it as key and
  set/replace this metadata entry to/by \code{value}. It is
  an error if \code{key} has zero length.  If it contains
  more than one entry, a nested query is done. See
  \code{[[} from the \pkg{base} package for details.}

  \item{value}{Value(s) to be included in the metadata. If
  \code{NULL}, this metadata entry is deleted.}
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
metadata(copy, "Strain") <- "08/15"
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(metadata(copy, "Strain") != metadata(vaas_1, "Strain"))
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
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other setter-functions: \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}}
}
\keyword{manip}

\docType{methods}
\alias{metadata-set,WMD,character,ANY-method}
\alias{metadata<-,WMD,character,ANY-method}
\usage{
  \S4method{metadata}{WMD,character,ANY}(object, key) <- value
}
