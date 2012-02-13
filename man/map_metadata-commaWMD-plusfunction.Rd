\name{map_metadata,WMD+function}
\alias{map_metadata,WMD+function}
\title{Map metadata using a function}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{mapping}{A function. It is applied to all non-list
  elements of \code{\link{metadata}}, which is traversed
  recursively.}

  \item{values}{Logical scalar. If \code{FALSE}, metadata
  names, not values, are mapped, and \code{classes} is
  ignored (names are always of class \sQuote{character}).}

  \item{classes}{Character vector specifying the classes to
  be mapped; others are returned unchanged.}

  \item{...}{Optional argument passed to \code{mapping}.}
}
\value{
  \code{\link{WMD}} object with modified metadata.
}
\description{
  Modify meta-information stored together with the
  measurements by using a function. (This is just a wrapper
  for \code{rapply}, with \code{how} set to
  \sQuote{replace}, if \code{values} is \code{TRUE}.)
}
\examples{
data(vaas_1)
copy <- map_metadata(vaas_1, identity)
stopifnot(identical(copy, vaas_1))
copy <- map_metadata(vaas_1, identity, values = FALSE)
stopifnot(identical(copy, vaas_1))
copy <- map_metadata(vaas_1, function(x) paste(x, "!"), values = FALSE)
(x <- metadata_chars(vaas_1, values = FALSE))
(y <- metadata_chars(copy, values = FALSE))
stopifnot(identical(as.character(y), paste(x, "!")))
}
\seealso{
  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}},
  \code{\link{map_metadata,OPMS}}, \code{\link{metadata}},
  \code{\link{metadata-set}},
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
\alias{map_metadata,WMD,function-method}
\usage{
  \S4method{map_metadata}{WMD,function}(object, mapping, 
    values = TRUE, classes = "ANY", ...) 
}
