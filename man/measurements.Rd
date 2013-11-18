\name{measurements}
\alias{hours}
\alias{measurements}
\alias{well}
\title{Stored measurements}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{i}{Optional character or numeric vector with
  name(s) or position(s) of well(s). Wells are originally
  named \sQuote{A01} to \sQuote{H12} but might have been
  subset beforehand. \code{i} can also be a formula,
  allowing for sequences of well coordinates. See the
  examples.}

  \item{drop}{Logical scalar. If only a single well was
  selected, simplify it to a vector?}

  \item{use.names}{Logical scalar indicating whether the
  time points should be used as names for the
  measurements.}

  \item{what}{Character scalar determining the output mode
  as follows: \describe{ \item{all}{Numeric vector: all
  time points, in order.} \item{interval}{The difference
  between each pair of adjacent time points, \code{NA} if
  this is irregular or only one time point is left.}
  \item{max}{Numeric scalar: the largest time point.}
  \item{minmax}{Numeric scalar: the smallest maximum. For
  \code{\link{OPM}} objects this is apparently identical to
  \sQuote{max}.} \item{size}{Integer scalar: the number of
  time points.} \item{summary}{Display a summary.} }}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  \code{measurements} returns a numeric matrix with column
  names indicating the well coordinate and a first column
  containing the time points. The other columns contain the
  values from each well. There is one row per time point.
  Column names are appropriately set, but not translated
  (as, e.g., to substrate names). It is possible to select
  wells, but the time points are always included as first
  column (in contrast to \code{well}). The \code{i}
  argument refers only to the remaining matrix.

  Do not confuse \code{well} with \code{\link{wells}}.
  \code{well} yields a numeric matrix or vector, depending
  on \code{i} and \code{drop}. It will always ignore the
  time points as values, in contrast to
  \code{measurements}. But depending on \code{use.names}
  they would be inserted as names.

  The return value of \code{hours} is dependent on the
  \code{what} argument; see there.
}
\description{
  Return the measurements, optionally only from selected
  wells and with or without the time points, or only the
  time points.
}
\examples{
# 'OPM' methods

head(x <- measurements(vaas_1))[, 1:5] # => numeric matrix
stopifnot(is.matrix(x), is.numeric(x))
stopifnot(dim(x) == c(384, 97))
head(x <- measurements(vaas_1, "B03"))
stopifnot(is.matrix(x), is.numeric(x), dim(x) == c(384, 2))
head(y <- measurements(vaas_1, ~B03)) # => same result with formula
stopifnot(identical(y, x))

head(x <- well(vaas_1, "B04")) # => numeric vector
stopifnot(is.numeric(x), length(x) == 384)
head(x <- well(vaas_1, c("B08", "C07"))) # => numeric matrix
stopifnot(is.matrix(x), dim(x) == c(384, 2))
# selecting adjacent wells is easer when using a formula
head(x <- well(vaas_1, c("B12", "C01", "C02")))
stopifnot(is.matrix(x), dim(x) == c(384, 3))
head(y <- well(vaas_1, ~ B12:C02)) # => same result
stopifnot(identical(x, y))

(x <- hours(vaas_1)) # the default is 'max'
stopifnot(identical(x, 95.75))
(x <- hours(vaas_1, "minmax"))
stopifnot(identical(x, 95.75))
(x <- hours(vaas_1, "summary"))
stopifnot(is.table(x))
(x <- hours(vaas_1, "interval"))
stopifnot(identical(x, 0.25))
(x <- hours(vaas_1, "size"))
stopifnot(identical(x, 384L))

# 'OPMS' methods

summary(x <- measurements(vaas_4)) # => list of numeric matrices
stopifnot(is.list(x), length(x) == length(vaas_4))
stopifnot(sapply(x, is.matrix), sapply(x, is.numeric))

head(x <- well(vaas_4, "B04"))[, 1:5] # => numeric matrix
stopifnot(is.matrix(x), dim(x) == c(4, 384))
head(y <- well(vaas_4, ~ B04))[, 1:5] # using a formula
stopifnot(identical(x, y)) # => same result

(x <- hours(vaas_4)) # all with the same overall running time
stopifnot(length(x) == 4, x == 95.75)
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{max}}, \code{\link{minmax}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{thin_out}}
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{hours-methods}
\alias{hours,MOPMX-method}
\alias{hours,OPM-method}
\alias{hours,OPMS-method}
\alias{measurements-methods}
\alias{measurements,MOPMX-method}
\alias{measurements,OPM-method}
\alias{measurements,OPMS-method}
\alias{well-methods}
\alias{well,MOPMX-method}
\alias{well,OPM-method}
\alias{well,OPMS-method}
\usage{
  \S4method{hours}{MOPMX}(object, ...) 
  \S4method{hours}{OPM}(object,
    what = c("max", "all", "size", "summary", "interval", "minmax")) 
  \S4method{hours}{OPMS}(object, ...) 

  \S4method{measurements}{MOPMX}(object, ...) 
  \S4method{measurements}{OPM}(object, i) 
  \S4method{measurements}{OPMS}(object, ...) 

  \S4method{well}{MOPMX}(object, ...) 
  \S4method{well}{OPM}(object, i, drop = TRUE, use.names = TRUE) 
  \S4method{well}{OPMS}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
