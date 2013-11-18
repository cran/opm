\name{aggregated}
\alias{aggr_settings}
\alias{aggregated}
\title{Get aggregated data}
\arguments{
  \item{object}{\code{\link{OPMA}} or \code{\link{OPMS}}
  object.}

  \item{subset}{Character vector. If not \code{NULL},
  restrict to this or these parameter(s). See
  \code{\link{param_names}} for the possible values.}

  \item{ci}{Logical scalar. Include the estimates of
  confidence intervals (\acronym{CI}s) in the output?}

  \item{trim}{Character scalar. Parameter estimates from
  intrinsically negative reactions (i.e., no respiration)
  are sometimes biologically unreasonable because they are
  too large or too small, and some corrections might be
  appropriate. \describe{ \item{no}{No modification.}
  \item{full}{Negative lambda estimates are set to zero.}
  \item{medium}{Lambda estimates larger than
  \code{\link{hours}(object)} (i.e., the maximum time value
  observed) are set to that value. Negative lambda
  estimates smaller than \code{-hours(object)} are set to
  this value (i.e., the negative maximum time).}
  \item{full}{Like \sQuote{medium}, but all negative values
  are set to zero, which is a less moderate treatment.} }
  Currently the other parameters are not checked, and all
  \code{NA} values, if any, also remain unchanged.}

  \item{full}{Logical scalar passed to \code{\link{wells}}.
  This and the following arguments affect the column names
  of the resulting matrix.}

  \item{in.parens}{Logical scalar also passed to that
  function.}

  \item{max}{Numeric scalar also passed to that function.}

  \item{join}{Empty or character scalar. If empty, a list
  is returned; a nested list in the case of
  \code{\link{OPMS}} objects with one contained list per
  plate. Otherwise this nested list is converted to a
  matrix or data frame, depending on the value of
  \code{join}. The following values yield a matrix in
  character mode and differ in how they would convert
  non-scalar values in a matrix in list mode, if
  encountered: \describe{ \item{json}{Converts to a
  \acronym{JSON} string.} \item{yaml}{Converts to a
  \acronym{YAML} string.} \item{rcode}{Converts by
  deparsing, yielding an \R code string.} } All other
  values of \code{join} are passed as \code{what} argument
  to the \code{collect} method form the \pkg{pkgutils}
  package, with \code{dataframe} and \code{keep.unnamed}
  set to \code{TRUE} but \code{stringsAsFactors} to
  \code{FALSE}.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  \code{aggregated} yields a numeric matrix of aggregated
  values (a.k.a. the curve parameters). If bootstrapping
  was used, their \acronym{CI}s are included. The columns
  represent the wells, the rows the estimated parameters
  and their \acronym{CI}s.

  \code{aggr_settings} returns a named list if \code{join}
  is empty. Other values yield a matrix or data frame (or
  an error). See the description of the argument above and
  the examples below for further details.
}
\description{
  Get the aggregated kinetic data or the aggregation
  settings used. (See \code{\link{do_aggr}} for generating
  aggregated data.)
}
\details{
  Note that the conversion of the settings list to a matrix
  or data frame might not work for all combinations of
  \code{object} and \code{join}, mainly because the options
  entry can hold arbitrary content. For similar conversions
  of the metadata, see the \code{\link{OPMX}} methods of
  \code{\link{to_metadata}}.
}
\examples{
# 'OPMA' methods
# Get full matrix
(x <- aggregated(vaas_1))[, 1:3]
stopifnot(is.matrix(x), dim(x) == c(12, 96))
(y <- aggregated(vaas_1, full = TRUE))[, 1:3] # full names
stopifnot(x == y, nchar(colnames(x)) < nchar(colnames(y)))
# Subsetting
(x <- aggregated(vaas_1, "lambda"))[, 1:3]
stopifnot(is.matrix(x), dim(x) == c(3, 96), any(x < 0))
# Now with lambda correction
(x <- aggregated(vaas_1, "lambda", trim = "full"))[, 1:3]
stopifnot(is.matrix(x), dim(x) == c(3, 96), !any(x < 0))

# settings
(x <- aggr_settings(vaas_1)) # yields named list
stopifnot(is.list(x), !is.null(names(x)))
(x <- aggr_settings(vaas_1, join = "json")) # yields a matrix
stopifnot(is.matrix(x), is.character(x), nrow(x) == 1)

# 'OPMS' methods
summary(x <- aggregated(vaas_4)) # => one matrix per OPM object
stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.matrix))

# settings
summary(x <- aggr_settings(vaas_4)) # list of named lists, one per plate
stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.list))
(x <- aggr_settings(vaas_4, join = "yaml")) # matrix, one row per plate
stopifnot(is.matrix(x), is.character(x), nrow(x) == 4)
}
\seealso{
  Other getter-functions: \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{minmax}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{thin_out}}, \code{\link{well}}
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{aggr_settings-methods}
\alias{aggr_settings,MOPMX-method}
\alias{aggr_settings,OPMA-method}
\alias{aggr_settings,OPMS-method}
\alias{aggregated-methods}
\alias{aggregated,MOPMX-method}
\alias{aggregated,OPMA-method}
\alias{aggregated,OPMS-method}
\usage{
  \S4method{aggr_settings}{MOPMX}(object, join = NULL) 
  \S4method{aggr_settings}{OPMA}(object, join = NULL) 
  \S4method{aggr_settings}{OPMS}(object, join = NULL) 

  \S4method{aggregated}{MOPMX}(object, ...) 
  \S4method{aggregated}{OPMA}(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium"), full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), ...) 
  \S4method{aggregated}{OPMS}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
