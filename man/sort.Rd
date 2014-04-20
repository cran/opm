\name{sort}
\alias{rep}
\alias{rev}
\alias{sort}
\alias{unique}
\title{Sort, unify, revert or repeat \acronym{OPMS} objects}
\arguments{
  \item{x}{\code{\link{OPM}} or \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{decreasing}{Logical scalar. Passed to \code{order}
  or \code{sort.list} from the \pkg{base} package.}

  \item{by}{List or character vector. For
  \code{\link{OPMS}} objects, if a list, a list of one to
  several keys passed as \code{key} argument to
  \code{\link{metadata}}. If a character vector of length
  one, \code{by} is passed as \sQuote{what} argument to
  \code{\link{csv_data}}. If longer, passed step-by-step to
  \code{\link{csv_data}} as \code{keys} argument.

  For \code{\link{MOPMX}} objects, either
  \sQuote{plate.type}, which sorts according to the plate
  types, \sQuote{length}, which sorts the elements
  according to their lengths (i.e., number of plates), or a
  metadata query that yields, for each element of \code{x},
  a vector to which \code{max} can be applied. Sorting
  \code{x} is then done according to these maxima.}

  \item{parse}{Logical scalar. Convert the
  \code{setup_time} via \code{strptime} before ordering?
  Has only an effect if \code{by} is \sQuote{setup_time}.
  It is an error if the time format is not recognised.  See
  \code{\link{opm_opt}}, arguments \code{time.fmt} and
  \code{time.zone}, for modifying the parsing of setup-time
  entries, and \code{\link{csv_data}} for this kind of
  entries.}

  \item{exact}{Logical scalar. Passed to
  \code{\link{metadata}}. Affects only metadata querying,
  not directly the sorting.}

  \item{strict}{Logical scalar. Is it an error if metadata
  keys are not found?  If \code{FALSE}, \code{x} gets
  ordered according to only the found keys, and remains in
  the original order if none of the keys in \code{by} are
  found at all. Note that it is always an error if keys are
  found in the \code{\link{metadata}} of some of the
  \code{\link{plates}} but not in those of others.}

  \item{na.last}{Logical scalar. Also passed to
  \code{order} or \code{sort.list}.}

  \item{incomparables}{Vector passed to
  \code{\link{duplicated}}. The default is \code{FALSE}.}

  \item{...}{Optional arguments passed between the methods
  or to \code{\link{duplicated}} or to \code{rep} from the
  \pkg{base} package. See the examples.}
}
\value{
  \code{\link{OPMS}} object with not necessarily the same
  order of plates than before, or \code{\link{OPM}} object.
}
\description{
  Sort an \code{\link{OPMS}} object based on one to several
  metadata or \acronym{CSV} data entries, or sort elements
  of a \code{\link{MOPMX}} object based on plate type,
  length, or a metadata entry. Alternatively, remove
  duplicated elements from a \code{\link{OPMS}} or
  \code{\link{MOPMX}} object, or revert the order of plates
  within an \code{\link{OPMS}} object, or, repeat
  \code{\link{OPMS}} or \code{\link{OPM}} objects zero
  times, once, or several times.
}
\details{
  The \code{sort} \code{\link{OPM}} method just returns the
  input data to avoid destructive effects due to the way
  the default \code{sort} interacts with \code{\link{OPM}}
  indexing.

  \code{rev} should be slightly more efficient than calling
  the default \code{rev} method. There is also an
  \code{\link{OPM}} method which just returns the input
  data (to avoid destructive effects due to the way the
  default \code{rev} interacts with \code{\link{OPM}}
  indexing).

  The \code{\link{OPM}} method of \code{unique} also
  returns the passed object.

  \code{rev} yields an \code{\link{OPMS}} object with
  another number of plates, or an \code{\link{OPM}} object,
  or \code{NULL}.
}
\examples{
## 'OPMS' methods

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
csv_data(vaas_4, what = "setup_time")
csv_data(copy, what = "setup_time")
stopifnot(!identical(copy, vaas_4))
copy <- sort(vaas_4, by = c("Position", "Setup Time"))
csv_data(vaas_4, what = "position")
csv_data(copy, what = "position")
stopifnot(!is.unsorted(csv_data(copy, what = "position")))
stopifnot(is.unsorted(csv_data(vaas_4, what = "position")))

# making OPMS objects unique
dim(x <- unique(vaas_4))
stopifnot(identical(x, vaas_4))
dim(x <- unique(c(vaas_4, vaas_4)))
stopifnot(identical(x, vaas_4))
dim(x <- unique(vaas_4, what = "Species")) # species are not unique
stopifnot(dim(x)[1L] < dim(vaas_4)[1L])
dim(x <- unique(vaas_4, what = list("Species", "Strain")))
stopifnot(identical(x, vaas_4)) # organisms are unique

# reverting an OPMS object
dim(x <- rev(vaas_4))
stopifnot(dim(x) == dim(vaas_4), !identical(x, vaas_4))
stopifnot(identical(rev(x), vaas_4))

# repeating an OPMS object
dim(x <- rep(vaas_4))
stopifnot(identical(x, vaas_4))
dim(x <- rep(vaas_4, times = 2))
stopifnot(length(x) == length(vaas_4) * 2)
dim(y <- rep(vaas_4, each = 2))
stopifnot(length(y) == length(vaas_4) * 2, !identical(x, y))
stopifnot(is.null(rep(vaas_4, 0)))

## 'OPM' methods
summary(x <- sort(vaas_1))
stopifnot(identical(x, vaas_1))
dim(x <- unique(vaas_1)) # trivial
stopifnot(identical(x, vaas_1))
dim(x <- unique(vaas_1, what = list("Species", "Strain")))
stopifnot(identical(x, vaas_1))
dim(x <- rev(vaas_1)) # trivial
stopifnot(identical(x, vaas_1))
dim(x <- rep(vaas_1, 1))
stopifnot(identical(x, vaas_1))
dim(x <- rep(vaas_1, 2)) # conversion to OPMS if > 1 element
stopifnot(length(x) == 2, is(x, "OPMS"))
stopifnot(is.null(rep(vaas_4, 0)))
}
\seealso{
  base::order base::sort base::strptime base::unique
  base::rev

  base::rep

  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{extract}}, \code{\link{extract_columns}},
  \code{\link{flatten}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{opmx}},
  \code{\link{plates}}, \code{\link{split}},
  \code{\link{to_yaml}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{rep-methods}
\alias{rep,OPM-method}
\alias{rep,OPMS-method}
\alias{rev-methods}
\alias{rev,OPM-method}
\alias{rev,OPMS-method}
\alias{sort-methods}
\alias{sort,MOPMX,ANY-method}
\alias{sort,MOPMX-method}
\alias{sort,MOPMX,missing-method}
\alias{sort,OPM,ANY-method}
\alias{sort,OPM-method}
\alias{sort,OPM,missing-method}
\alias{sort,OPMS,ANY-method}
\alias{sort,OPMS-method}
\alias{sort,OPMS,missing-method}
\alias{unique-methods}
\alias{unique,MOPMX,ANY-method}
\alias{unique,MOPMX-method}
\alias{unique,MOPMX,missing-method}
\alias{unique,OPM,ANY-method}
\alias{unique,OPM-method}
\alias{unique,OPM,missing-method}
\alias{unique,OPMS,ANY-method}
\alias{unique,OPMS-method}
\alias{unique,OPMS,missing-method}
\usage{
  \S4method{rep}{OPM}(x, ...) 
  \S4method{rep}{OPMS}(x, ...) 

  \S4method{rev}{OPM}(x) 
  \S4method{rev}{OPMS}(x) 

  \S4method{sort}{MOPMX,ANY}(x, decreasing,
    by = c("plate.type", "length"), exact = TRUE, strict = TRUE,
    na.last = TRUE, ...) 
  \S4method{sort}{MOPMX,missing}(x, decreasing, ...) 
  \S4method{sort}{OPM,ANY}(x, decreasing, ...) 
  \S4method{sort}{OPM,missing}(x, decreasing, ...) 
  \S4method{sort}{OPMS,ANY}(x, decreasing, by = "setup_time",
    parse = by == "setup_time", exact = TRUE, strict = TRUE, na.last = TRUE) 
  \S4method{sort}{OPMS,missing}(x, decreasing, ...) 

  \S4method{unique}{MOPMX,ANY}(x, incomparables, ...) 
  \S4method{unique}{MOPMX,missing}(x, incomparables, ...) 
  \S4method{unique}{OPM,ANY}(x, incomparables, ...) 
  \S4method{unique}{OPM,missing}(x, incomparables, ...) 
  \S4method{unique}{OPMS,ANY}(x, incomparables, ...) 
  \S4method{unique}{OPMS,missing}(x, incomparables, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
