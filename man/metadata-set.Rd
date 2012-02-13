\name{metadata-set}
\alias{metadata-set}
\alias{metadata<-}
\title{Replace metadata}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{value}{List. Values to be set as metadata. Previous
  metadata, if any, are replaced entirely by \code{value}.}
}
\value{
  \code{value}.
}
\description{
  Set the meta-information stored together with the data.
  This version replaces all metadata by its argument.
}
\note{
  See \sQuote{See Also} for the other \sQuote{metadata<-}
  methods.
}
\examples{
data(vaas_1)
copy <- vaas_1
new.md <- list(Species = "Thermomicrobium roseum")
metadata(copy) <- new.md
stopifnot(identical(metadata(copy), new.md))
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
  \code{\link{metadata}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other setter-functions:
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}}
}
\keyword{manip}

\docType{methods}
\alias{metadata-set-methods}
\alias{metadata-set,WMD,missing,list-method}
\alias{metadata<-,WMD,missing,list-method}
\usage{
  \S4method{metadata}{WMD,missing,list}(object) <- value
}
