\name{sort}
\alias{sort}
\title{Sort OPMS objects}
\arguments{
  \item{x}{\code{\link{OPMS}} or \code{\link{OPM}} object.}

  \item{decreasing}{Logical scalar. Passed to \code{order}
  from the \pkg{base} package.}

  \item{by}{List or character vector. If a list, a list of
  one to several keys passed as \code{key} argument to
  \code{\link{metadata}}. If a character vector of length
  one, \code{by} must specify the name of one of the
  functions \code{\link{setup_time}},
  \code{\link{filename}} or \code{\link{position}}. If
  longer, passed step-by-step to \code{\link{csv_data}} as
  \code{keys} argument.}

  \item{parse}{Logical scalar. Convert the
  \code{\link{setup_time}} via \code{strptime} before
  ordering? Has only an effect if \code{by} is
  \sQuote{setup_time}. It is an error if the time format is
  not recognized.  See \code{\link{opm_opt}}, arguments
  \sQuote{time.fmt} and \sQuote{time.zone}, for modifying
  the parsing of setup-time entries.}

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

  \item{na.last}{Logical scalar. Also passed to
  \code{order}.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  \code{\link{OPMS}} object with not necessarily the same
  order of plates than before, or \code{\link{OPM}} object.
}
\description{
  Sort an \code{\link{OPMS}} object based on one to several
  metadata or \acronym{CSV} data entries. There is also an
  \code{\link{OPM}} method which returns the input data (to
  avoid destructive effects due to the way the default
  \code{sort} interacts with \code{\link{OPM}} indexing).
}
\examples{
## 'OPMS' method
data(vaas_4)

# Existing keys
stopifnot(is.unsorted(metadata(vaas_4, "Strain")))
x <- sort(vaas_4, by = list("Strain"))
stopifnot(is(x, "OPMS"), !is.unsorted(metadata(x, "Strain")))
x <- sort(vaas_4, by = list("Strain"), decreasing = TRUE)
stopifnot(is(x, "OPMS"), is.unsorted(metadata(x, "Strain")))

# Non-existing keys
x <- try(sort(vaas_4, by = list("Not there", "Missing"), strict = TRUE))
stopifnot(inherits(x, "try-error")) # yields error
x <- try(sort(vaas_4, by = list("Not there", "Missing"), strict = FALSE))
stopifnot(identical(x, vaas_4)) # no error, but no new order

# CSV-data based
copy <- sort(vaas_4) # default: by setup time
setup_time(vaas_4)
setup_time(copy)
stopifnot(!identical(copy, vaas_4))
copy <- sort(vaas_4, by = c("Position", "Setup Time"))
position(vaas_4)
position(copy)
stopifnot(!is.unsorted(position(copy)), is.unsorted(position(vaas_4)))

## 'OPMS' method
data(vaas_1)
summary(x <- sort(vaas_1))
stopifnot(identical(x, vaas_1))
}
\seealso{
  base::order base::sort base::strptime

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{manip}

\docType{methods}
\alias{sort-methods}
\alias{sort,OPMX,missing-method}
\alias{sort,OPM,logical-method}
\alias{sort,OPMS,logical-method}
\usage{
  \S4method{sort}{OPMX,missing}(x, decreasing, ...) 

  \S4method{sort}{OPM,logical}(x, decreasing, ...) 

  \S4method{sort}{OPMS,logical}(x, decreasing, by = "setup_time",
    parse = TRUE, exact = TRUE, strict = TRUE, na.last = TRUE) 

}
