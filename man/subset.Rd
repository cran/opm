\name{subset}
\alias{subset}
\alias{thin_out}
\title{Select a subset of the plates (or time points)}
\arguments{
  \item{x}{\code{\link{OPMX}} or \code{\link{MOPMX}}
  object.}

  \item{query}{Logical or numeric vector or object accepted
  as query by the infix operators. If a logical or numeric
  vector, \code{query} is directly used as the first
  argument of \code{\link{[}}, and all following arguments,
  if any, are ignored. If otherwise, it is used for
  conducting a query based on one of the infix operators as
  described below.}

  \item{values}{Logical scalar. If \code{TRUE}, the values
  of \code{query} are also considered (by using
  \code{\link{infix.q}} or \code{\link{infix.largeq}}). If
  \code{FALSE} only the keys are considered (by using
  \code{\link{infix.k}}).

  That is, choose either the plates for which certain
  metadata entries contain certain values, or choose the
  plates for which these metadata have been set at all (to
  some arbitrary value). See the mentioned functions for
  details, and note the special behaviour if \code{query}
  is a character vector and \code{values} is \code{FALSE}.}

  \item{invert}{Logical scalar. If \code{TRUE}, return the
  plates for which the condition is not \code{TRUE}.}

  \item{exact}{Logical scalar. If the values of
  \code{query} are considered, should this be done using
  \code{\link{infix.q}} (when \code{FALSE}) or
  \code{\link{infix.largeq}} (when \code{TRUE})? See these
  functions and \code{\link{contains}} for details.}

  \item{time}{Logical scalar. If \code{TRUE}, all other
  arguments are ignored and the object is reduced to the
  common subset of time points (measurement hours and
  minutes).}

  \item{positive}{Character scalar. If \sQuote{ignore}, not
  used. Otherwise all previous arguments except
  \code{object} are ignored, and the function yields an
  error unless \code{object} has been discretised
  throughout, i.e.  either it is an \code{\link{OPMD}}
  object or all elements of \code{object} have discretised
  values.

  If \code{object} has the necessary discretised values, if
  \sQuote{any}, wells are selected that contain positive
  reactions in at least one plate.  If \sQuote{all}, wells
  are selected that contain positive reactions in all
  plates. Using \code{invert} means selecting all negative
  or weak reactions.}

  \item{negative}{Character scalar. Like \code{positive},
  but returns the negative reactions. Using \code{invert}
  means selecting all positive or weak reactions.}

  \item{use}{Character scalar. An alternative way to
  specify the settings.

  If \sQuote{i} or \sQuote{I}, ignored. If \sQuote{t} or
  \sQuote{T}, \code{time} is set to \code{TRUE}. If
  \sQuote{p} or \sQuote{P}, \code{positive} is set to
  \sQuote{any} or \sQuote{all}, respectively. If \sQuote{n}
  or \sQuote{N}, \code{negative} is set to \sQuote{any} or
  \sQuote{all}, respectively.

  Otherwise, \code{use} is taken directly as the one-latter
  name of the infix operators to use for plate selection,
  overriding \code{values} and \code{exact}.}

  \item{object}{\code{\link{OPMX}} object.}

  \item{factor}{Numeric scalar >= 1 indicating how much the
  data set shall be thinned out.}

  \item{drop}{Logical scalar. See \code{\link{[}}.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}}
  object. This depends on how many plates are selected; see
  \code{\link{[}} for details.  The \code{\link{MOPMX}}
  method always returns a \code{\link{MOPMX}} object.
}
\description{
  Select a subset of the plates in an \code{\link{OPMS}}
  object based on the content of the metadata.
  Alternatively, select a common subset of time points from
  all plates. \code{thin_out} keeps only a regular subset
  of the time points from \code{\link{OPM}} measurements.
  This is a mainly experimental function that might be of
  use in testing.
}
\details{
  The \code{\link{MOPMX}} method creates subsets of all
  contained \code{\link{OPMX}} objects (if any) in turn and
  then removes those that yielded \code{NULL}. Thus
  \code{subset} is not intended for directly creating
  subsets of \code{\link{MOPMX}} but of their elements to
  yield, e.g., elements that have a common set of
  \code{\link{metadata}} entries, as required under most
  circumstances by some other \code{\link{MOPMX}} methods
  such as \code{\link{extract}}.

  Thinning the plates out is experimental insofar as it has
  \strong{not} been tested whether and how this could
  sensibly be applied before aggregating the data.
}
\examples{
# simple object comparison function
mustbe <- function(a, b) stopifnot(identical(a, b))

# all plates have that entry: selection identical to original object
mustbe(vaas_4, vaas_4["Species" \%k\% vaas_4, ])
mustbe(vaas_4, subset(vaas_4, list(Species = "Escherichia coli"),
  values = FALSE)) # equivalent
mustbe(vaas_4, subset(vaas_4, ~ Species == "Escherichia coli",
  values = FALSE)) # also equivalent

# two plates also have that value: yielding OPMS object with only two plates
mustbe(vaas_4[1:2], vaas_4[list(Species = "Escherichia coli") \%q\% vaas_4, ])
mustbe(vaas_4[1:2], subset(vaas_4, list(Species = "Escherichia coli")))
mustbe(vaas_4[1:2], subset(vaas_4, ~ Species == "Escherichia coli"))

# these are also equivalent
mustbe(vaas_4[c(1, 3)],
  vaas_4[list(Strain = c("DSM18039", "DSM1707")) \%q\% vaas_4])
mustbe(vaas_4[c(1, 3)],
  subset(vaas_4, list(Strain = c("DSM18039", "DSM1707"))))
mustbe(vaas_4[c(1, 3)],
  subset(vaas_4, ~ Strain \%in\% c("DSM18039", "DSM1707")))
mustbe(vaas_4[c(1, 3)],
  subset(vaas_4, ~ Strain == "DSM18039" || Strain == "DSM1707"))
# note that particularly formulae can be used to set up very complex queries

# select all plates that have aggregated values
dim(x <- subset(vaas_4, has_aggr(vaas_4)))
mustbe(x, vaas_4) # all have such values

# select a common set of time points
dim(x <- subset(vaas_4, time = TRUE))
mustbe(x, vaas_4) # the time points had already been identical
# create unequal time points
dim(copy <- vaas_4[, list(1:10, 1:20, 1:15, 1:10)])
mustbe(hours(copy), c(2.25, 4.75, 3.50, 2.25))
# now restrict to common subset
dim(x <- subset(copy, time = TRUE))
mustbe(hours(x), rep(2.25, 4))
# see also the example with split() given under "["

# select all wells that have positive reactions
dim(x <- subset(vaas_4, use = "p")) # in at least one plate
stopifnot(dim(x)[3] < dim(vaas_4)[3])
dim(y <- subset(vaas_4, use = "P")) # in all plates
stopifnot(dim(y)[3] < dim(x)[3])

# select all wells that have non-negative reactions in at least one plate
dim(y <- subset(vaas_4, use = "N", invert = TRUE))
stopifnot(dim(y)[3] > dim(x)[3])

## thin_out()

# 'OPM' method
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))
copy <- thin_out(vaas_1, 10) # keep every 10th time point and measurement
(x <- dim(copy))
stopifnot(identical(x, c(38L, 96L)), has_aggr(copy))
copy <- thin_out(vaas_1, 10, drop = TRUE) # also remove the parameters
(x <- dim(copy))
stopifnot(identical(x, c(38L, 96L)), !has_aggr(copy))

# 'OPMS' method
(x <- dim(vaas_4))
stopifnot(identical(x, c(4L, 384L, 96L)))
copy <- thin_out(vaas_4, 10)
(x <- dim(copy))
stopifnot(identical(x, c(4L, 38L, 96L)))
}
\seealso{
  base::`[` base::`[[` base::subset

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{contains}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{minmax}},
  \code{\link{seq}}, \code{\link{well}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{subset-methods}
\alias{subset,MOPMX-method}
\alias{subset,OPMX-method}
\alias{thin_out-methods}
\alias{thin_out,MOPMX-method}
\alias{thin_out,OPM-method}
\alias{thin_out,OPMS-method}
\usage{
  \S4method{subset}{MOPMX}(x, query, ...) 
  \S4method{subset}{OPMX}(x, query, values = TRUE,
    invert = FALSE, exact = FALSE, time = FALSE,
    positive = c("ignore", "any", "all"),
    negative = c("ignore", "any", "all"),
    use = c("i", "I", "k", "K", "n", "N", "p", "P", "q", "Q", "t", "T")) 

  \S4method{thin_out}{MOPMX}(object, ...) 
  \S4method{thin_out}{OPM}(object, factor, drop = FALSE) 
  \S4method{thin_out}{OPMS}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
