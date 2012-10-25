\name{metadata_chars}
\alias{metadata_chars}
\title{Get metadata characters}
\arguments{
  \item{object}{\code{\link{WMD}} or \code{\link{OPMS}}
  object.}

  \item{values}{Logical scalar. If \code{FALSE}, metadata
  names, not values, are collected, and \code{classes} is
  ignored (names are always of class \sQuote{character} and
  need not be coerced).}

  \item{classes}{Character vector containing the names of
  classes that should also be collected (and coerced to
  \sQuote{character}), or \code{TRUE}. In that case, a
  mapping template for the classes themselves is returned.
  See the \code{coerce} argument of \code{map_values} for
  details.}

  \item{...}{Optional argument passed from the
  \code{\link{OPMS}} to the \code{\link{WMD}} method.}
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
  The \code{\link{OPMS}} method just applies the
  \code{\link{WMD}} method to all contained plates in turn.
}
\examples{
# WMD method
data(vaas_1)
(x <- metadata_chars(vaas_1, values = FALSE))
stopifnot(names(x) == x)
(x <- metadata_chars(vaas_1, values = TRUE))
stopifnot(names(x) == x)
# See map_metadata() for a potential usage of the metadata_chars() result

# OPMS method
data(vaas_4)
(x <- metadata_chars(vaas_4, values = TRUE)) # the values
(y <- metadata_chars(vaas_4, values = FALSE)) # the keys
stopifnot(length(x) > length(y))
}
\seealso{
  Other metadata-functions: \code{\link{include_metadata}},
  \code{\link{map_metadata}}, \code{\link{metadata.set}},
            }
\keyword{attribute}

\docType{methods}
\alias{metadata_chars-methods}
\alias{metadata_chars,WMD-method}
\alias{metadata_chars,OPMS-method}
\usage{
  \S4method{metadata_chars}{WMD}(object, values = TRUE,
    classes = "factor") 

  \S4method{metadata_chars}{OPMS}(object, ...) 

}
