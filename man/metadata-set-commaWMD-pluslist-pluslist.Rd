\name{metadata-set,WMD+list+list}
\alias{metadata-set,WMD+list+list}
\alias{metadata<-,WMD+list+list}
\title{Set metadata (WMD+list+list version)}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{key}{List. Treat it as list of keys; expect
  \code{value} to be a list of corresponding metadata
  values to be set. Names are replaced by the values of
  either list if they are missing.}

  \item{value}{List, Values to be included in the metadata.
  See \code{key} for details.}
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
stopifnot(identical(metadata(copy, "Species"), "Escherichia coli"))

# You can use this to translate the keys on-the-fly...
metadata(copy, list(Organism = "Species")) <- list(
  Organism = "Bacillus subtilis")
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(identical(metadata(copy, "Species"), "Bacillus subtilis"))
stopifnot(is.null(metadata(copy, "Organism"))) # this was not set!

# ...but you need not
metadata(copy, list("Species")) <- list(Species = "Yersinia pestis")
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(identical(metadata(copy, "Species"), "Yersinia pestis"))

# Names need not be duplicated
metadata(copy, list("Species")) <- list("Gen. sp.")
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(identical(metadata(copy, "Species"), "Gen. sp."))

# ...but this would delete the entry because nothing would be found in
# 'value'
metadata(copy, list("Species")) <- list(Organism = "E. coli")
stopifnot(length(metadata(copy)) < length(metadata(vaas_1)))
stopifnot(is.null(metadata(copy, "Species")))

# ...this yields a general mechanism for metadata deletion by providing an
# empty list as 'value'.
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
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other setter-functions: \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+numeric+list}}
}
\keyword{manip}

\docType{methods}
\alias{metadata-set,WMD,list,list-method}
\alias{metadata<-,WMD,list,list-method}
\usage{
  \S4method{metadata}{WMD,list,list}(object, key) <- value
}
