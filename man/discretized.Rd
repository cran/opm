\name{discretized}
\alias{disc_settings}
\alias{discretized}
\title{Get discretised data}
\arguments{
  \item{object}{\code{\link{OPMD}} or \code{\link{OPMS}}
  object.}

  \item{full}{Logical scalar passed to \code{\link{wells}}.
  This and the following arguments affect the names of the
  resulting vector.}

  \item{in.parens}{Logical scalar also passed to that
  function.}

  \item{max}{Numeric scalar also passed to that function.}

  \item{join}{Empty or character scalar. Works like the
  eponymous argument of \code{\link{aggr_settings}}; see
  there for details.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Logical vector or matrix in the case of
  \code{discretized}, named list in the case of
  \code{disc_settings}. See the examples for details.
}
\description{
  Get the discretised kinetic data or the discretisation
  settings used. (See \code{\link{do_disc}} for generating
  discretised data.)
}
\examples{
# 'OPM' methods
(x <- discretized(vaas_1))[1:3] # => logical vector
stopifnot(is.logical(x), !is.matrix(x), length(x) == dim(x)[2L])
stopifnot(names(x) == colnames(aggregated(vaas_1)))
(x <- discretized(vaas_1, full = TRUE))[1:3] # => with full names
stopifnot(names(x) == colnames(aggregated(vaas_1, full = TRUE)))

# settings
(x <- disc_settings(vaas_1)) # => named list
stopifnot(is.list(x), !is.null(names(x)))
(x <- disc_settings(vaas_1, join = "yaml")) # matrix, one row per plate
stopifnot(is.matrix(x), is.character(x), nrow(x) == 1)

# 'OPMS' methods
(x <- discretized(vaas_4))[, 1:3] # => logical matrix
stopifnot(is.logical(x), is.matrix(x), ncol(x) == dim(x)[2L])
stopifnot(colnames(x) == colnames(aggregated(vaas_1)))

# settings
summary(x <- disc_settings(vaas_4)) # => list of named lists, one per plate
stopifnot(is.list(x), is.null(names(x)), length(x) == length(vaas_4))
stopifnot(duplicated(x)[-1])
(x <- disc_settings(vaas_4, join = "json")) # matrix, one row per plate
stopifnot(is.matrix(x), is.character(x), nrow(x) == 4)
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
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
\alias{disc_settings-methods}
\alias{disc_settings,MOPMX-method}
\alias{disc_settings,OPMD-method}
\alias{disc_settings,OPMS-method}
\alias{discretized-methods}
\alias{discretized,MOPMX-method}
\alias{discretized,OPMD-method}
\alias{discretized,OPMS-method}
\usage{
  \S4method{disc_settings}{MOPMX}(object, join = NULL) 
  \S4method{disc_settings}{OPMD}(object, join = NULL) 
  \S4method{disc_settings}{OPMS}(object, join = NULL) 

  \S4method{discretized}{MOPMX}(object, ...) 
  \S4method{discretized}{OPMD}(object, full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), ...) 
  \S4method{discretized}{OPMS}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
