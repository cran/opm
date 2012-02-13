\name{metadata-set,OPMS+ANY+ANY}
\alias{metadata-set,OPMS+ANY+ANY}
\alias{metadata<-,OPMS+ANY+ANY}
\title{Set metadata (OPMS version)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{key}{Any R object that is accepted as the
  \code{key} argument of one of the eponymous methods of
  the \code{\link{OPM}} class. Must fit to the type of
  \code{value}.}

  \item{value}{Any R object that is accepted as the
  \code{value} argument of one of the eponymous methods of
  the \code{\link{OPM}} class. Must fit to the type of
  \code{key}.}
}
\value{
  \code{value}.
}
\description{
  Set specified or all meta-information stored together
  with the measurements of all plates at once. This version
  replaces specfied metadata or all by its \code{value}
  argument, details depending on the \code{key} argument.
}
\note{
  See \sQuote{See Also} for the other \sQuote{metadata<-}
  methods.
}
\examples{
data(vaas_4)
copy <- vaas_4
(metadata(copy, "Species") <- "Bacillus subtilis")
stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))
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
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other setter-functions: \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}}
}
\keyword{manip}

\docType{methods}
\alias{metadata-set,OPMS,ANY,ANY-method}
\alias{metadata<-,OPMS,ANY,ANY-method}
\usage{
  \S4method{metadata}{OPMS,ANY,ANY}(object, key) <- value
}
