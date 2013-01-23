\name{select}
\alias{select}
\title{Select a subset of the plates (or time points) [deprecated]}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{query}{Logical, numeric or character vector, or
  list (other objects can be provided but are coerced to
  class \sQuote{character}). If a logical or numeric
  vector, \code{query} is directly used as the first
  argument of \code{\link{[}}, and all following arguments,
  if any, are ignored. If a list or a character vector, it
  is used for conducting a query based on one of the infix
  operators as described below. The data-frame method
  expects a character vector containing class names.}

  \item{...}{Optional arguments passed to
  \code{\link{subset}}.}
}
\value{
  \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}}
  object. This depends on how many plates are selected; see
  \code{\link{[}} for details.  The data-frame method
  returns a data frame.
}
\description{
  Select a subset of the plates in an \code{\link{OPMS}}
  object based on the content of the metadata.
  Alternatively, select a common subset of time points from
  all plates. The data-frame method selects columns that
  belong to certain classes.
}
\note{
  The use of this function is \strong{deprecated}. Please
  use \code{\link{subset}} instead.
}
\examples{
## 'OPMS' method
data(vaas_4)
# simple object comparison function
mustbe <- function(a, b) stopifnot(identical(a, b))

# all plates have that entry: selection identical to original object
mustbe(vaas_4, vaas_4["Species" \%k\% vaas_4, ])
mustbe(vaas_4, select(vaas_4, list(Species = "Escherichia coli"),
  values  = FALSE)) # equivalent

# two plates also have that value: yielding OPMS object with only two plates
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
# see also the example with split() given under "["

# select all wells that have positive reactions
summary(x <- select(vaas_4, use = "p")) # in at least one plate
stopifnot(dim(x)[3] < dim(vaas_4)[3])
summary(y <- select(vaas_4, use = "P")) # in all plates
stopifnot(dim(y)[3] < dim(x)[3])

# select all wells that have non-negative reactions in at least one plate
summary(y <- select(vaas_4, use = "N", invert = TRUE))
stopifnot(dim(y)[3] > dim(x)[3])

## data-frame method
x <- data.frame(a = 1:5, b = letters[1:5], c = LETTERS[1:5])
(y <- select(x, "factor"))
stopifnot(dim(y) == c(5, 2))
(y <- select(x, "integer"))
stopifnot(dim(y) == c(5, 1))
(y <- select(x, c("factor", "integer")))
mustbe(x, y)
}
\seealso{
  base::`[` base::`[[` base::subset

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{subset}}, \code{\link{summary}},
  \code{\link{well}}, \code{\link{wells}}
}
\keyword{manip}

\docType{methods}
\alias{select-methods}
\alias{select,OPMS-method}
\alias{select,data.frame-method}
\usage{
  \S4method{select}{OPMS}(object, query, ...) 

  \S4method{select}{data.frame}(object, query) 

}
