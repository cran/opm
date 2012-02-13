\name{extract_columns}
\alias{extract_columns}
\title{Create data frame or vector from metadata}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{what}{List of metadata keys to consider, or single
  such key; passed to \code{\link{metadata}}.}

  \item{join}{Logical scalar. Join each row together to
  yield a character vector? Otherwise it is just attempted
  to construct a data frame.}

  \item{sep}{Character scalar. Used as separator between
  the distinct metadata entries if these are to be pasted
  together. Ignored unless \code{join} is \code{TRUE}.}

  \item{dups}{Character scalar specifying what to do in the
  case of duplicate labels: either \sQuote{warn},
  \sQuote{error} or \sQuote{ignore}. Ignored unless
  \code{join} is \code{TRUE}.}

  \item{exact}{Logical scalar. Also passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar. Also passed to
  \code{\link{metadata}}.}
}
\value{
  Data frame or character vector, depending on the
  \code{join} argument.
}
\description{
  Extract selected metadata entries for use as additional
  columns in a dataframe or (after joining) as character
  vector with labels. This is not normally directly called
  by an \pkg{opm} user because \code{\link{extract}} is
  available, which uses this function, but can be used for
  testing the applied metadata selections beforehand.
}
\examples{
data(vaas_4)

# Create data frame
(x <- extract_columns(vaas_4, what = list("Species", "Strain")))
stopifnot(is.data.frame(x), identical(dim(x), c(4L, 2L)))

# Create a character vector
(x <- extract_columns(vaas_4, what = list("Species", "Strain"), join = TRUE))
stopifnot(is.character(x), length(x) == 4L)
(x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
   dups = "error"), silent = TRUE))
stopifnot(is(x, "try-error"))
(x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
  dups = "warn"), silent = TRUE))
stopifnot(is.character(x), length(x) == 4L)
}
\seealso{
  data.frame as.data.fram cbind

  Other conversion-functions: \code{\link{extract}},
  \code{\link{flatten}}, \code{\link{flatten,OPMS}},
  \code{\link{plates}}, \code{\link{to_yaml}}

  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}},
  \code{\link{map_metadata,OPMS}},
  \code{\link{map_metadata,WMD+function}},
  \code{\link{metadata}}, \code{\link{metadata-set}},
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
\keyword{dplot}
\keyword{manip}

\docType{methods}
\alias{extract_columns-methods}
\alias{extract_columns,OPMS-method}
\usage{
  \S4method{extract_columns}{OPMS}(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), exact = TRUE, 
    strict = TRUE) 
}
