\name{[}
\alias{[}
\title{Select subset}
\arguments{
  \item{x}{\code{\link{OPM}}, \code{\link{OPMA}} or
  \code{\link{OPMS}} object.}

  \item{i}{Vector or missing. For the \code{\link{OPM}} and
  \code{\link{OPMA}} method, the indexes of one to several
  time points. For the \code{\link{OPMS}} method, the
  indexes of one to several plates.}

  \item{j}{Vector or missing. For the \code{\link{OPM}} and
  \code{\link{OPMA}} method, the indexes or names of one to
  several wells. For the \code{\link{OPMS}} method, the
  indexes of one to several time points. In that case, if
  \code{j} is a list, its values are passed to the
  respective \code{\link{OPM}} object separately, allowing
  for individual choices of time points. Otherwise \code{j}
  is used as the \code{i} argument of the \code{\link{OPM}}
  and \code{\link{OPMA}} method.}

  \item{k}{Vector or missing. The \code{\link{OPMS}} method
  uses \code{k} as \code{j} argument of the
  \code{\link{OPM}} and \code{\link{OPMA}} method.  That
  is, this parameter selects the wells.}

  \item{...}{This should \strong{not} be set. It is an
  error to specify additional dimensions.}

  \item{drop}{Logical scalar. Remove the aggregated data
  and turn \code{\link{OPMA}} to \code{\link{OPM}} objects?
  Has no effect if \code{x} already is an \code{\link{OPM}}
  object or contains only such objects.}
}
\value{
  \code{\link{OPM}}, \code{\link{OPMA}} or
  \code{\link{OPMS}} object, or \code{NULL}.
}
\description{
  Select a subset of the measurements. Return other slots
  unchanged. In contrast to the usual `[` functions, always
  return a matrix (as a component of the returned object),
  even if it could be simplified to a vector. The time
  column is not counted and always copied. It is an error
  to delete the entire matrix. In all other respects, this
  method behaves like the `[` functions from the \pkg{base}
  package. The \code{\link{OPMS}} method selects a subset
  of the plates and/or the measurements of the individual
  plates. It simplifies the outcome to a \code{\link{OPM}}
  or \code{\link{OPMA}} object if only a single plate
  remains and to \code{NULL} if no plate remains. This
  behaves like subsetting a three-dimensional array with
  plates as first dimension, time points as second, and
  wells as third.
}
\details{
  The \code{\link{OPMA}} method works like the
  \code{\link{OPM}} one, but the function applies the
  subsetting to the original and the aggregated data in
  parallel. The aggregated data may also be dropped
  entirely; this might be appropriate if a subset of the
  time points is selected, potentially yielding aggregated
  values that do not fit to the measurements anymore.
}
\examples{
# OPM(A) method
data(vaas_1)
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))
copy <- vaas_1[, 11:22]
(x <- dim(copy))
stopifnot(identical(x, c(384L, 12L)))
copy <- vaas_1[]
stopifnot(has_aggr(copy))
stopifnot(identical(copy, vaas_1))
copy <- vaas_1[drop = TRUE]
stopifnot(!has_aggr(copy))
stopifnot(!identical(copy, vaas_1))

# OPMS method
data(vaas_4)

# Create OPMS object with fewer plates (the first two ones)
x <- vaas_4[1:2]
stopifnot(dim(x) == c(2, 384, 96))

# If only a single plate is selected, this is reduced to OPM(A)
x <- vaas_4[3]
stopifnot(dim(x) == c(384, 96))

# Create OPMS object with fewer time points (the first 100 in that case;
# usually this would correspond to the first 25 hours)
x <- vaas_4[, 1:100]
stopifnot(dim(x) == c(4, 100, 96))

# Create OPMS object with fewer wells
x <- vaas_4[, , 1:12]
stopifnot(dim(x) == c(4, 384, 12))

# The same with well names
x <- vaas_4[, , sprintf("A\%02i", 1:12)] # this yields A01...A12
stopifnot(dim(x) == c(4, 384, 12))

# Select all plates that have aggregated values
x <- vaas_4[has_aggr(vaas_4)]
stopifnot(identical(x, vaas_4)) # all have such values!

# Split into list of OPMS objects with the same overall measurement hours;
# the default split() method can be applied here based on "["
x <- split(vaas_4, hours(vaas_4))
stopifnot(class(x) == "list", length(x) == 1, class(x[[1]]) == "OPMS")
# ... because the running times were actually already identical, the list
# contains only a single element.

# Traverse all contained OPM objects
for (i in seq(vaas_4)) { # OR: for (i in 1:length(vaas_4))
  x <- vaas_4[i]
  # now do something with 'x'...
  stopifnot(dim(x) == c(384, 96))
}
# see also opms_apply() for a more elegant approach
}
\seealso{
  base::`[` base::`[[`
}
\keyword{manip}

\docType{methods}
\alias{bracket-methods}
\alias{bracket,OPM-method}
\alias{[,OPM-method}
\alias{bracket,OPMA-method}
\alias{[,OPMA-method}
\alias{bracket,OPMD-method}
\alias{[,OPMD-method}
\alias{bracket,OPMS-method}
\alias{[,OPMS-method}
\usage{
  \S4method{[}{OPM}(x, i, j, ..., drop = FALSE) 

  \S4method{[}{OPMA}(x, i, j, ..., drop = FALSE) 

  \S4method{[}{OPMD}(x, i, j, ..., drop = FALSE) 

  \S4method{[}{OPMS}(x, i, j, k, ..., drop = FALSE) 

}
