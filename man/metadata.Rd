\name{metadata}
\alias{metadata}
\alias{metadata_chars}
\title{Get metadata}
\arguments{
  \item{object}{\code{\link{WMD}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{key}{\code{NULL}, vector, factor or formula.
  \itemize{ \item If \code{NULL} or otherwise empty, return
  all metadata.  \item If a non-empty list, treated as list
  of keys. Return value would be the list of corresponding
  metadata values. Here, character vectors of length > 1
  can be used to query nested metadata lists.  \item If
  neither empty nor a list nor a formula (i.e. usually a
  character or numeric vector), \code{key} is treated as a
  single list key. Factors are converted to
  \sQuote{character} mode.  \item Formulae can also be used
  and are converted to a list or character or numeric
  vector using the rules described under \sQuote{Details}.
  \item It is in general not recommended to use numeric
  vectors as \code{key} arguments, either directly or
  within a list or formula. }}

  \item{exact}{Logical scalar. Use exact or partial
  matching of keys? Has no effect if \code{key} is empty.}

  \item{strict}{Logical scalar. Is it an error if a
  \code{NULL} value results from fetching a metadata key?}

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
  \code{metadata} generates a list (empty if metadata were
  not set or if partial selection using \code{key} did not
  result).

  \code{metadata_chars} yields a character vector, sorted
  and made unique.  Original \code{names} attributes, if
  any, are dropped and replaced by the character vector
  itself. (This might be convenient regarding its use with
  \code{\link{map_metadata}}.)
}
\description{
  Get meta-information stored together with the data or
  collect all \sQuote{character} entries from the
  meta-information stored together with the measurements.
  Optionally coerce data of other types.
}
\details{
  The result of \code{metadata_chars} can be used to create
  a mapping for \code{\link{map_metadata}}. The
  \code{\link{OPMS}} method just applies the
  \code{\link{WMD}} method to all contained plates in turn.
}
\examples{
# 'OPM' methods

(x <- metadata(vaas_1, "Strain"))
stopifnot(x == "DSM30083T")
(y <- metadata(vaas_1, ~ Strain)) # using a formula => same result
stopifnot(identical(x, y))

(x <- metadata_chars(vaas_1, values = FALSE))
stopifnot(names(x) == x) # mapping metadata keys to themselves
(x <- metadata_chars(vaas_1, values = TRUE))
stopifnot(names(x) == x) # mapping metadata values to themselves
# See map_metadata() for a potential usage of the metadata_chars() result

# 'OPMS' methods

(x <- metadata(vaas_4, "Strain"))
stopifnot(x == c("DSM18039", "DSM30083T", "DSM1707", "429SC1"))
(y <- metadata(vaas_4, ~ Strain)) # using a formula => same result
stopifnot(identical(x, y))

(x <- metadata_chars(vaas_4, values = TRUE)) # the values
(y <- metadata_chars(vaas_4, values = FALSE)) # the keys
stopifnot(length(x) > length(y))
}
\seealso{
  Other metadata-functions: \code{\link{edit}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}}, \code{\link{metadata.set}},
                                  }
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{metadata-methods}
\alias{metadata,MOPMX-method}
\alias{metadata,OPMS-method}
\alias{metadata,WMD-method}
\alias{metadata_chars-methods}
\alias{metadata_chars,MOPMX-method}
\alias{metadata_chars,OPMS-method}
\alias{metadata_chars,WMD-method}
\usage{
  \S4method{metadata}{MOPMX}(object, ...) 
  \S4method{metadata}{OPMS}(object, ...) 
  \S4method{metadata}{WMD}(object, key = NULL, exact = TRUE,
    strict = FALSE) 

  \S4method{metadata_chars}{MOPMX}(object, ...) 
  \S4method{metadata_chars}{OPMS}(object, ...) 
  \S4method{metadata_chars}{WMD}(object, values = TRUE,
    classes = "factor") 
}
%% END INSERTION BY repair_S4_docu.rb
