\name{max}
\alias{max}
\alias{minmax}
\title{Overall or minimal maximum}
\arguments{
  \item{x}{\code{\link{OPM}} or \code{\link{OPMS}} object.}

  \item{...}{Coordinate of one to several wells. If
  missing, the maximum or smallest of all wells is
  returned. See \code{\link{well}} for details. If only as
  single well is selected, the result of \code{minmax} is
  actually identical to the one of \code{max}.}

  \item{na.rm}{Logical scalar. See \code{max} from the
  \pkg{base} package. Has no effect here because \code{NA}
  values are not allowed within the measurements.}
}
\value{
  Numeric scalar.
}
\description{
  Get the maximum of all wells or (a) specified one(s), or
  their smallest maximum. The \code{\link{OPMS}} method
  works by calling the \code{\link{OPM}} method on all
  plates and then determining the overall maximum or
  overall minimum of the maxima.
}
\examples{
# OPM method
(x <- max(vaas_1))
(y <- max(vaas_1, 1)) # this is the negative control
stopifnot(x > y) # i.e., some stronger reactions present
(x <- minmax(vaas_1))
stopifnot(max(vaas_1) > x) # obviously

# OPMS method
(x <- max(vaas_4))
(y <- max(vaas_4, 1)) # this is the negative control
stopifnot(x > y) # i.e., some stronger reactions present
(x <- minmax(vaas_4))
stopifnot(max(vaas_4) > x) # obviously
}
\seealso{
  base::max base::min

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{measurements}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{thin_out}}, \code{\link{well}}
}
\keyword{attribute}
\keyword{dplot}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{max-methods}
\alias{max,OPM-method}
\alias{max,OPMS-method}
\alias{minmax-methods}
\alias{minmax,OPM-method}
\alias{minmax,OPMS-method}
\usage{
  \S4method{max}{OPM}(x, ..., na.rm = FALSE) 
  \S4method{max}{OPMS}(x, ..., na.rm = FALSE) 

  \S4method{minmax}{OPM}(x, ..., na.rm = FALSE) 
  \S4method{minmax}{OPMS}(x, ..., na.rm = FALSE) 
}
%% END INSERTION BY repair_S4_docu.rb
