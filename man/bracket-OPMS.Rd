\name{[,OPMS}
\alias{[,OPMS}
\title{Select subset (OPMS version)}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{i}{Numeric or logical vector. Position(s) of
  plate(s) or missing.}

  \item{j}{Passed as first argument to the eponymous method
  \code{\link{[}} of the \code{\link{OPM}} class. See there
  for details. If \code{j} is a list, its values are passed
  to the respective \code{\link{OPM}} object separately,
  allowing for individual choices of time points.}

  \item{...}{Passed as second argument to that method (here
  \code{...} can actually only comprise a single
  parameter).}

  \item{drop}{Logical scalar. Remove the aggregated values
  from contained \code{\link{OPMA}} objects, if any?}
}
\value{
  \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}}
  object (depending on \code{i}).
}
\description{
  Select a subset of the plates and/or the measurements of
  the individual plates. Simplify to \code{\link{OPM}} or
  \code{\link{OPMA}} object if only a single plate remains.
  This behaves like subsetting a three-dimensional array
  with plates as first dimension, time points as second,
  and wells as third.
}
\examples{
data(vaas_4)

# simple function testing object identity
mustbe <- function(a, b) stopifnot(identical(a, b))
mustbe(dim(vaas_4), c(4L, 384L, 96L))

# Create OPMS object with fewer plates (the first two ones)
x <- vaas_4[1:2]
mustbe(dim(x), c(2L, 384L, 96L))

# If only a single plate is selected, this is reduced to OPM(A)
x <- vaas_4[3]
mustbe(dim(x), c(384L, 96L))

# Create OPMS object with fewer time points (the first 100 in that case;
# usually this would correspond to the first 25 hours)
x <- vaas_4[, 1:100]
mustbe(dim(x), c(4L, 100L, 96L))

# Create OPMS object with fewer wells
x <- vaas_4[, , 1:12]
mustbe(dim(x), c(4L, 384L, 12L))

# The same with well names
x <- vaas_4[, , sprintf("A\%02i", 1:12)] # this yields A01...A12
mustbe(dim(x), c(4L, 384L, 12L))

# Select all plates that have aggregated values
x <- vaas_4[has_aggr(vaas_4)]
mustbe(x, vaas_4) # all have such values!
}
\seealso{
  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{dim,OPMS}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{max,OPMS}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{manip}

\docType{methods}
\alias{bracket,OPMS-method}
\alias{[,OPMS-method}
\usage{
  \S4method{[}{OPMS}(x, i, j, ..., drop = FALSE) 
}
