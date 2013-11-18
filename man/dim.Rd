\name{dim}
\alias{dim}
\alias{seq}
\title{Get dimensions}
\arguments{
  \item{x}{\code{\link{OPMX}} object.}

  \item{...}{\code{\link{OPMS}} objects. Several ones can
  be provided, but all but the first one are ignored. For
  reasons of comparability, the \code{\link{OPM}} method of
  \code{seq} deliberately results in an error.}
}
\value{
  For the \code{\link{OPM}} method of \code{dim}, a
  two-element numeric vector (number of time points and
  number of wells). For the \code{\link{OPMS}} method, a
  numeric vector with (i) the number of contained
  \code{\link{OPM}} objects, and (ii) and (iii) the
  dimensions of the first plate. \code{length} returns an
  integer scalar. This \code{seq} method yields an integer
  vector (starting with 1 and at least of length 2).
}
\description{
  Get the dimensions of the measurements of an
  \code{\link{OPM}} object, or get the dimensions of an
  \code{\link{OPMS}} object, or the number of plates stored
  in an \code{\link{OPMX}} object, or the indexes of all
  these plates.
}
\details{
  Note that \code{dim} cannot be used to determine the
  correspondence of the time points between all plates as
  it reports only the time points of the first plate.
  Instead the \code{\link{OPMS}} method of
  \code{\link{hours}} must be used.

  \code{seq} yields the indexes of all plates contained in
  an \code{\link{OPMS}} object. This is mainly useful for
  looping over such objects. See \code{\link{[}} for a
  loop-construct usage example, and note that
  \code{\link{oapply}} is also available.
}
\examples{
# OPM methods
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))
(x <- length(vaas_1))
stopifnot(identical(x, 1L)) # 1 plate contained
(x <- try(seq(vaas_1), silent = TRUE)) # deliberately yields an error
stopifnot(inherits(x, "try-error"))

# OPMS methods
(x <- dim(vaas_4)) # 2nd value needs not be correct for all plates
stopifnot(identical(x, c(4L, 384L, 96L)))
(x <- length(vaas_4))
stopifnot(identical(x, 4L)) # 4 plates contained
(x <- seq(vaas_4))
stopifnot(identical(x, 1:4)) # indexes for 4 plates
(y <- seq(vaas_4, letters, LETTERS)) # other arguments are ignored
stopifnot(identical(x, y))
}
\seealso{
  base::dim base::length base::seq

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{minmax}}, \code{\link{position}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{thin_out}}, \code{\link{well}}
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{dim-methods}
\alias{dim,OPM-method}
\alias{dim,OPMS-method}
\alias{length-methods}
\alias{length,OPM-method}
\alias{length,OPMS-method}
\alias{seq-methods}
\alias{seq,OPM-method}
\alias{seq,OPMS-method}
\usage{
  \S4method{dim}{OPM}(x) 
  \S4method{dim}{OPMS}(x) 

  \S4method{length}{OPM}(x) 
  \S4method{length}{OPMS}(x) 

  \S4method{seq}{OPM}(...) 
  \S4method{seq}{OPMS}(...) 
}
%% END INSERTION BY repair_S4_docu.rb
