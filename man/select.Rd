\name{select}
\alias{select}
\title{Select a subset of the plates (or time points)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{query}{Logical, numeric or character vector, or
  list (other objects can be provided but are coerced to
  class \sQuote{character}). If a logical or numeric
  vector, \code{query} is directly used as the first
  argument of \code{\link{[,OPMS}}, and all following
  arguments, if any, are ignored.  If a list or a character
  vector, it is used for conducting a query based on one of
  the infix operators as described below.}

  \item{values}{Logical scalar. If \code{TRUE}, the values
  of \code{query} are also considered (by using
  \code{\link{infix-q}} or \code{\link{infix-largeq}}). If
  \code{FALSE} only the keys are considered (by using
  \code{\link{infix-k}}). That is, choose either the plates
  for which certain metadata entries contain certain
  values, or choose the plates for which these metadata
  have been set at all (to some arbitrary value).  See the
  mentioned functions for details, and note the special
  behaviour if \code{query} is a character vector and
  \code{values} is \code{FALSE}.}

  \item{invert}{Logical scalar. If \code{TRUE}, return the
  plates for which the condition is not \code{TRUE}.}

  \item{exact}{Logical scalar. If the values of
  \code{query} are considered, should this be done using
  \code{\link{infix-q}} (when \code{FALSE}) or
  \code{\link{infix-largeq}} (when \code{TRUE})? See these
  functions and \code{\link{contains}} for details.}

  \item{time}{Logical scalar. If \code{TRUE}, all other
  arguments are ignored and the object is reduced to a
  common subset of time point (measurement hours and
  minutes).}

  \item{use}{Character scalar. An alternative way to
  specify the settings. If \sQuote{i} or \sQuote{I},
  ignored. If \sQuote{t} or \sQuote{T}, \code{time} is set
  to \code{TRUE}. Otherwise, \code{use} is taken directly
  as the one-latter name of the infix operators to use for
  plate selection, overriding \code{values} and
  \code{exact}.}
}
\value{
  \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}}
  object. This depends on how many plates are selected; see
  \code{\link{[,OPMS}} for details.
}
\description{
  Select a subset of the plates in an \code{\link{OPMS}}
  object based on the content of the metadata.
  Alternatively, select a common subset of time points from
  all plates.
}
\examples{
data(vaas_4)
# simple object comparison function
mustbe <- function(a, b) stopifnot(identical(a, b))

# all plates have that entry: selection identical to original object
mustbe(vaas_4, vaas_4["Species" \%k\% vaas_4, ])
mustbe(vaas_4, select(vaas_4, list(Species = "Escherichia coli"),
  values  = FALSE)) # equivalent

# two plates also have that value: yielding OPMS object woth only two plates
mustbe(vaas_4[1:2], vaas_4[list(Species = "Escherichia coli") \%q\% vaas_4, ])
mustbe(vaas_4[1:2], select(vaas_4, list(Species = "Escherichia coli")))

# select all plates that have aggregated values
x <- select(vaas_4, has_aggr(vaas_4))
mustbe(x, vaas_4) # all have such values

# select a common set of time points
x <- select(vaas_4, time = TRUE)
mustbe(x, vaas_4) # the time points had already been identical
# create unequal time points
copy <- vaas_4[, list(1:10, 1:20, 1:15, 1:10)]
mustbe(hours(copy), c(2.25, 4.75, 3.50, 2.25))
# now restrict to common subset
x <- select(copy, time = TRUE)
mustbe(hours(x), rep(2.25, 4))
}
\seealso{
  [ [[ subset

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{max,OPMS}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{minmax,OPMS}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{manip}

\docType{methods}
\alias{select-methods}
\alias{select,OPMS-method}
\usage{
  \S4method{select}{OPMS}(object, query, values = TRUE,
    invert = FALSE, exact = FALSE, time = FALSE, 
    use = c("i", "I", "k", "K", "q", "Q", "t", "T")) 
}
