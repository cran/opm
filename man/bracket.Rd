\name{[}
\alias{[}
\title{Select subset}
\arguments{
  \item{x}{\code{\link{OPM}}, \code{\link{OPMA}} or
  \code{\link{OPMS}} object.}

  \item{i}{Vector or missing. For the \code{\link{OPM}} and
  \code{\link{OPMA}} method, the indexes of one to several
  time points.

  For the \code{\link{OPMS}} method, the indexes of one to
  several plates. A warning is issued if indexing goes
  beyond the range. If \code{i} is neither a numeric nor a
  logical vector, the \code{\link{OPMS}} method passes it
  through \code{\link{infix.q}} to yield a logical vector
  for indexing. If \code{i} is a formula, its left side can
  be used to choose another infix operator as
  \code{\link{infix.q}}.

  For the \code{\link{MOPMX}} method, either missing or a
  vector or a formula or expression. The latter work like
  for \code{\link{OPMS}} objects and yield a list of
  logical vectors (one per element of \code{x}) Such a list
  is then further used for selecting subset within the
  elements of \code{x}.  A list can also be provided
  directly.}

  \item{j}{Vector or missing. \itemize{ \item For the
  \code{\link{OPM}} and \code{\link{OPMA}} method, the
  indexes or names of one to several wells. Can also be a
  formula, which allows for sequences of well coordinates,
  which are translated to their positions within the
  currently present well names. Be aware that this means
  that the content of a sequence of well coordinates is
  dependent on \code{x}!  \item For the \code{\link{OPMS}}
  method, the indexes of one to several time points. In
  that case, if \code{j} is a list, its values are passed
  to the respective \code{\link{OPM}} object separately,
  allowing for individual choices of time points. Otherwise
  \code{j} is used as the \code{i} argument of the
  \code{\link{OPM}} and \code{\link{OPMA}} method.  }}

  \item{k}{Vector or missing. The \code{\link{OPMS}} method
  passes \code{k} as \code{j} argument of the
  \code{\link{OPM}} and \code{\link{OPMA}} method.  That
  is, in that case \emph{this} parameter selects the wells.
  See \code{j} for details.}

  \item{...}{This should \strong{not} be set. It is an
  error to specify additional dimensions.}

  \item{drop}{Logical scalar. Remove the aggregated data
  (and the discretised data, if any) and turn an
  \code{\link{OPMA}} or \code{\link{OPMD}} object to an
  \code{\link{OPM}} object? Has no effect if \code{x}
  already is an \code{\link{OPM}} object or contains only
  such objects. For the \code{\link{MOPMX}} method,
  \code{TRUE} means dropping the class and generating a
  list.}
}
\value{
  \code{\link{OPM}}, \code{\link{OPMA}} or
  \code{\link{OPMS}} object, or \code{NULL}.
}
\description{
  Select a subset of the \code{\link{measurements}} (and,
  if present, of the \code{\link{aggregated}} data and the
  \code{\link{discretized}} data) or plates. Return this
  subset (or these subsets) together with the other slots
  (which are unchanged).
}
\details{
  The \code{\link{OPMA}} method works like the
  \code{\link{OPM}} one, but the function applies the
  subset creation to the original and the aggregated data
  in parallel. The \code{\link{OPMD}} method applies the
  selection also to the discretised data.

  The aggregated and discretised data may also be dropped
  entirely; this might be appropriate if a subset of the
  time points is selected, potentially yielding aggregated
  values that do not fit to the measurements anymore.

  In contrast to the usual `[` methods, with respect to the
  measurements this always return a matrix (as a component
  of the returned object), even if it could be simplified
  to a vector. The time column is not counted and always
  copied. It is an error to delete the entire matrix. In
  all other respects, the \code{\link{OPM}} method behaves
  like the `[` methods from the \pkg{base} package.

  The \code{\link{OPMS}} method selects a subset of the
  plates and/or the measurements of the individual plates.
  It simplifies the outcome to a \code{\link{OPM}} or
  \code{\link{OPMA}} object if only a single plate remains
  and to \code{NULL} if no plate remains. This is different
  from creating subsets of a list in \R. \code{\link{OPMS}}
  subset creation rather behaves like subset creation a
  three-dimensional array with plates as first dimension,
  time points as second, and wells as third.
}
\examples{
## OPM(A) method

# complete dataset, full 96-well plates
(x <- dim(vaas_1))
stopifnot(x == c(384, 96))

# selecting specific wells
copy <- vaas_1[, 11:22]
(x <- dim(copy))
stopifnot(x == c(384, 12))
# indexing with formulae allows for sequences of well coordinates
copy <- vaas_1[, ~ A11:B10] # "A11" is 11th, "B10" is 22th well name
stopifnot(dim(copy) == c(384, 12)) # same result as above
# can also be combined
copy <- vaas_1[, ~ A11:22]
stopifnot(dim(copy) == c(384, 12)) # same result as above

# dropping aggregated data
copy <- vaas_1[] # normal selection
stopifnot(has_aggr(copy), identical(copy, vaas_1))
copy <- vaas_1[drop = TRUE] # selection with dropping
stopifnot(!has_aggr(copy), !identical(copy, vaas_1))


## OPMS method

# Create OPMS object with fewer plates (the first two ones)
(x <- vaas_4[1:2])
stopifnot(is(x, "OPMS"), dim(x) == c(2, 384, 96))
# we can select the same objects with a formula (which is passed through
# the infix-q operator)
stopifnot(identical(vaas_4[~ Species == "Escherichia coli"], x))
# we can select another infix operator with the left side of the formula
stopifnot(identical(vaas_4[k ~ Species], vaas_4))

# If only a single plate is selected, this is reduced to OPM(A)
x <- vaas_4[3]
stopifnot(!is(x, "OPMS"), dim(x) == c(384, 96))

# Create OPMS object with fewer time points (the first 100 in that case;
# usually this would correspond to the first 25 hours)
x <- vaas_4[, 1:100]
stopifnot(dim(x) == c(4, 100, 96))

# Create OPMS object with fewer wells
(x <- vaas_4[, , 1:12])
stopifnot(dim(x) == c(4, 384, 12))

# The same with well names
x <- vaas_4[, , ~ A01:A12] # within x, these are well names 1 to 12
stopifnot(dim(x) == c(4, 384, 12))
# to do this with a vector, one would need sprintf("A\%02i", 1:12)

# Select all plates that have aggregated values
x <- vaas_4[has_aggr(vaas_4)]
stopifnot(identical(x, vaas_4)) # all have such values!

# Traverse all contained OPM objects
for (i in seq(vaas_4)) { # OR: for (i in 1:length(vaas_4))
  x <- vaas_4[i]
  # now do something with 'x'...
  stopifnot(dim(x) == c(384, 96))
}
# see also oapply() for a more elegant approach

## MOPMX method
(x <- new("MOPMX", list(vaas_1, vaas_4))) # create MOPMX object
stopifnot(is(x, "MOPMX"), length(x) == 2)
(y <- x[~ Species != "Escherichia coli"])
stopifnot(is(y, "MOPMX"), length(y) == 1)
(y <- x[list(1, 3:4)]) # only 2nd element reduced
stopifnot(is(y, "MOPMX"), length(y) == 2, !identical(x, y))
}
\seealso{
  base::`[` base::`[[`
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{bracket-methods}
\alias{bracket,MOPMX,ANY,missing,ANY-method}
\alias{bracket,MOPMX,ANY,missing-method}
\alias{[,MOPMX,ANY,missing,ANY-method}
\alias{[,MOPMX,ANY,missing-method}
\alias{bracket,MOPMX,ANY,missing,missing-method}
\alias{[,MOPMX,ANY,missing,missing-method}
\alias{bracket,MOPMX,character,missing,ANY-method}
\alias{bracket,MOPMX,character,missing-method}
\alias{[,MOPMX,character,missing,ANY-method}
\alias{[,MOPMX,character,missing-method}
\alias{bracket,MOPMX,character,missing,missing-method}
\alias{[,MOPMX,character,missing,missing-method}
\alias{bracket,MOPMX,expression,missing,ANY-method}
\alias{bracket,MOPMX,expression,missing-method}
\alias{[,MOPMX,expression,missing,ANY-method}
\alias{[,MOPMX,expression,missing-method}
\alias{bracket,MOPMX,expression,missing,missing-method}
\alias{[,MOPMX,expression,missing,missing-method}
\alias{bracket,MOPMX,formula,missing,ANY-method}
\alias{bracket,MOPMX,formula,missing-method}
\alias{[,MOPMX,formula,missing,ANY-method}
\alias{[,MOPMX,formula,missing-method}
\alias{bracket,MOPMX,formula,missing,missing-method}
\alias{[,MOPMX,formula,missing,missing-method}
\alias{bracket,MOPMX,list,missing,ANY-method}
\alias{bracket,MOPMX,list,missing-method}
\alias{[,MOPMX,list,missing,ANY-method}
\alias{[,MOPMX,list,missing-method}
\alias{bracket,MOPMX,list,missing,missing-method}
\alias{[,MOPMX,list,missing,missing-method}
\alias{bracket,MOPMX,missing,missing,ANY-method}
\alias{bracket,MOPMX,missing,missing-method}
\alias{[,MOPMX,missing,missing,ANY-method}
\alias{[,MOPMX,missing,missing-method}
\alias{bracket,MOPMX,missing,missing,missing-method}
\alias{[,MOPMX,missing,missing,missing-method}
\alias{bracket,OPM,ANY,ANY,ANY-method}
\alias{bracket,OPM-method}
\alias{[,OPM,ANY,ANY,ANY-method}
\alias{[,OPM-method}
\alias{bracket,OPMA,ANY,ANY,ANY-method}
\alias{bracket,OPMA-method}
\alias{[,OPMA,ANY,ANY,ANY-method}
\alias{[,OPMA-method}
\alias{bracket,OPMD,ANY,ANY,ANY-method}
\alias{bracket,OPMD-method}
\alias{[,OPMD,ANY,ANY,ANY-method}
\alias{[,OPMD-method}
\alias{bracket,OPMS,ANY,ANY,ANY-method}
\alias{bracket,OPMS-method}
\alias{[,OPMS,ANY,ANY,ANY-method}
\alias{[,OPMS-method}
\usage{
  \S4method{[}{MOPMX,ANY,missing,ANY}(x, i, j, drop) 
  \S4method{[}{MOPMX,ANY,missing,missing}(x, i, j, drop) 
  \S4method{[}{MOPMX,character,missing,ANY}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,character,missing,missing}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,expression,missing,ANY}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,expression,missing,missing}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,formula,missing,ANY}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,formula,missing,missing}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,list,missing,ANY}(x, i, j, drop) 
  \S4method{[}{MOPMX,list,missing,missing}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,missing,missing,ANY}(x, i, j,
    drop) 
  \S4method{[}{MOPMX,missing,missing,missing}(x, i, j,
    drop) 
  \S4method{[}{OPM,ANY,ANY,ANY}(x, i, j, ...,
    drop = FALSE) 
  \S4method{[}{OPMA,ANY,ANY,ANY}(x, i, j, ...,
    drop = FALSE) 
  \S4method{[}{OPMD,ANY,ANY,ANY}(x, i, j, ...,
    drop = FALSE) 
  \S4method{[}{OPMS,ANY,ANY,ANY}(x, i, j, k, ...,
    drop = FALSE) 
}
%% END INSERTION BY repair_S4_docu.rb
