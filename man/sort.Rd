\name{sort}
\alias{sort}
\title{Sort OPMS objects}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{by}{A list of one to several keys passed as
  \code{key} argument to \code{\link{metadata}}. Other
  vectors are converted to a list, but only if a list is
  used directly, nested queries are possible.}

  \item{decreasing}{Logical scalar. Passed to \code{order}
  from the \pkg{base} package.}

  \item{na.last}{Logical scalar. Also passed to
  \code{order}.}

  \item{exact}{Logical scalar. Passed to
  \code{\link{metadata}}. Affects only metadata querying,
  not directly the sorting.}

  \item{strict}{Logical scalar. Is it an error if metadata
  keys are not found?  If \code{FALSE}, \code{x} gets
  ordered according to only the found keys, and remains in
  the original order if none of the keys in \code{by} are
  found at all. Note that it is always an error if keys are
  found in the \code{\link{metadata}} of some of
  the\code{\link{plates}} but not in those of others.}
}
\value{
  \code{\link{OPMS}} object with not necessarily the same
  order of plates than before.
}
\description{
  Sort an \code{\link{OPMS}} based on metadata entries.
}
\examples{
data(vaas_4)

# Existing keys
stopifnot(is.unsorted(metadata(vaas_4, "Strain")))
x <- sort(vaas_4, by = list("Strain"))
stopifnot(is(x, "OPMS"), !is.unsorted(metadata(x, "Strain")))
x <- sort(vaas_4, by = "Strain", decreasing = TRUE)
stopifnot(is(x, "OPMS"), is.unsorted(metadata(x, "Strain")))

# Non-existing keys
x <- try(sort(vaas_4, list("Not there", "Missing"), strict = TRUE))
stopifnot(inherits(x, "try-error"))
x <- try(sort(vaas_4, list("Not there", "Missing"), strict = FALSE))
stopifnot(identical(x, vaas_4))
}
\seealso{
  base::order base::sort

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{merge}}, \code{\link{oapply}},
  \code{\link{plates}}, \code{\link{to_yaml}}
}
\keyword{manip}

\docType{methods}
\alias{sort-methods}
\alias{sort,OPMS-method}
\usage{
  \S4method{sort}{OPMS}(x, by, decreasing = FALSE, na.last = TRUE, 
    exact = TRUE, strict = TRUE) 

}
