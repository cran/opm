\name{metadata}
\alias{metadata}
\title{Get metadata}
\arguments{
  \item{object}{\code{\link{WMD}} object.}

  \item{key}{\code{NULL}, vector, factor or formula.
  \itemize{ \item If \code{NULL} or otherwise empty, return
  all metadata.  \item If a non-empty list, treated as list
  of keys. Return value would be the list of corresponding
  metadata values. Here, character vectors of length > 1
  can be used to query nested metadata lists.  \item If
  neither empty nor a list nior a formula (i.e. usually a
  character or numeric vector), \code{key} is treated as a
  single list key. Factors are converted to
  \sQuote{character} mode.  \item Formulas can also be used
  and are converted to a list or character or numeric
  vector using the rules described under \sQuote{Details}.
  \item It is in general not recommended to use numeric
  vectors as \code{key} arguments, either directly or
  within a list or formula. }}

  \item{exact}{Logical scalar. Use exact or partial
  matching of keys? Has no effect if \code{key} is empty.}

  \item{strict}{Logical scalar. Is it an error if a
  \code{NULL} value results from fetching a metadata key?}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  List (empty if metadata were not set or if subselection
  using \code{key} did not result).
}
\description{
  Get meta-information stored together with the data.
}
\details{
  If a named list is used as \code{key} argument, its names
  will be used within the first level of the resulting
  nested or non-nested list.  That is, \code{key} can be
  used to translate names on the fly, and this can be used
  by all functions that call \code{metadata} indirectly,
  usually via an \code{as.labels} or \code{as.groups}
  argument.

  Even though it is not technically impossible per se, it
  is usually a bad idea to select metadata entries using
  numeric (positional) keys. The problem is that, in
  contrast to, e.g., data frames, their is no guarantee
  that metadata entries with the same name occur in the
  same position, even if they belong to \code{\link{OPM}}
  objects within a single \code{\link{OPMS}} object.

  Formulas passed as \code{key} argument are treated by
  ignoring the left side (if any) and converting the right
  side to a list or other vector. Code enclosed in \code{I}
  is evaluated with a call to \code{eval}. It is up to the
  user to ensure that this call succeeds and yields a
  character vector or a list. Operators in all other code
  within the formula are used just as separators, and all
  names are converted to character scalars. The \code{$}
  operator binds tightly, i.e. it separates elements of a
  character vector (for nested querying) in the output. The
  same effect have other operators of high precedence such
  as \code{::} but their use is not recommended. All
  operators with a lower precedence than \code{$} separate
  list elements.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- metadata(vaas_1, "Strain"))
stopifnot(x == "DSM30083T")
(y <- metadata(vaas_1, ~ Strain)) # using a formula => same result
stopifnot(identical(x, y))

# 'OPMS' method
data(vaas_4)
(x <- metadata(vaas_4, "Strain"))
stopifnot(x == c("DSM18039", "DSM30083T", "DSM1707", "429SC1"))
(y <- metadata(vaas_4, ~ Strain)) # using a formula => same result
stopifnot(identical(x, y))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{subset}}, \code{\link{summary}},
  \code{\link{well}}, \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{metadata-methods}
\alias{metadata,WMD-method}
\alias{metadata,OPMS-method}
\usage{
  \S4method{metadata}{WMD}(object, key = NULL, exact = TRUE,
    strict = FALSE) 

  \S4method{metadata}{OPMS}(object, ...) 

}
