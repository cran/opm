\name{csv_data}
\alias{csv_data}
\title{Information from input \acronym{CSV} file}
\arguments{
  \item{object}{\code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{keys}{Character vector (or other objects usable as
  vector index). An optional sub-selection. If empty (the
  default), all \acronym{CSV} data are returned. By default
  it is an error to select non-existing items. Ignored
  unless \code{what} is \sQuote{select}.}

  \item{strict}{Logical scalar indicating whether or not it
  is an error if \code{keys} are not found. Ignored unless
  \code{what} is \sQuote{select}.}

  \item{what}{Character scalar specifying a subset of the
  data. If \sQuote{select}, use \code{keys} and
  \code{strict}. If \sQuote{other}, select all
  \acronym{CSV} entries that have no special meaning (it
  makes sense to include only these in the metadata, see
  the examples).  Otherwise a shortcut for one of the more
  important \acronym{CSV} entries.}

  \item{normalize}{Logical scalar indicating whether plate
  position and setup time entries (if selected) should be
  normalised. This should always work for the plate
  positions, but for the setup times it depends on the
  values for the \code{\link{opm_opt}} keys \code{time.fmt}
  and \code{time.zone} (see also \code{\link{merge}}). For
  other entries, normalisation means replacing backslashes
  by slashes.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  For the \code{\link{OPM}} method, a named character
  vector (unnamed character scalar in the case of
  \code{filename}, \code{setup_time} and \code{filename}
  and if \code{what} is not \sQuote{select}). For the other
  methods either a named character vector (if the selection
  yields a single entry) or a character matrix with one row
  per plate. Missing entries in one of the plates yield
  \code{NA} in the output.
}
\description{
  Information about the plate as originally read from the
  input \acronym{CSV} file (see \code{\link{read_opm}} and
  \code{\link{read_single_opm}} for reading such files).
}
\details{
  It is easy to copy the \acronym{CSV} data to the
  \code{\link{metadata}}; see the examples section. Editing
  of the \acronym{CSV} data has deliberately not been
  implemented into \pkg{opm}, but the
  \code{\link{metadata}} can be modified using a plethora
  of methods, even manually with \code{\link{edit}}.
}
\examples{
## 'OPM' method

(x <- csv_data(vaas_1, "Setup Time"))
stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))
# compare this to 'what = "setup_time"'; here, names are kept
(y <- csv_data(vaas_1, "Setup Time", normalize = TRUE))
stopifnot(!is.na(y), y != x, names(y) == names(x))

(x <- csv_data(vaas_1, what = "filename")) # one file name (of course)
stopifnot(is.character(x), length(x) == 1L)

(x <- csv_data(vaas_1, what = "position")) # single position (of course)
(y <- csv_data(vaas_1, what = "position", normalize = TRUE))
stopifnot(x == " 7-B", y == "07-B") # four characters when normalised

(x <- csv_data(vaas_1, what = "setup_time")) # single setup time (of course)
(y <- csv_data(vaas_1, what = "setup_time", normalize = TRUE))
stopifnot(length(x) == 1, x != y) # converted to canonical data/time format
# WARNING: It is unlikely that all OmniLog output has the setup-time format
# defined by default in opm_opt("time.fmt")

## 'OPMS' method

(x <- csv_data(vaas_4, "Setup Time")) # one setup time per plate
stopifnot(is.character(x), length(x) == 4)

(x <- csv_data(vaas_4, what = "filename"))  # one file name per plate
stopifnot(is.character(x), length(x) == 4L)

(x <- csv_data(vaas_4, what = "position")) # one position per plate
stopifnot(is.character(x), length(x) == length(vaas_4))

(x <- csv_data(vaas_4, what = "setup_time")) # one setup time per plate
(y <- csv_data(vaas_4, what = "setup_time", normalize = TRUE))
stopifnot(length(x) == 4, x != y) # converted to canonical data/time format
# see the warning above

## Useful application: copying selected CSV data to the metadata

x <- vaas_4
# this appends the CSV data after conversion to a suitable data frame
metadata(x, -1) <- to_metadata(csv_data(x, what = "other"))
to_metadata(x)
stopifnot(sapply(metadata(x), length) > sapply(metadata(vaas_4), length))
}
\seealso{
  base::strptime

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{contains}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{minmax}}, \code{\link{seq}},
  \code{\link{subset}}, \code{\link{thin_out}},
  \code{\link{well}}
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{csv_data-methods}
\alias{csv_data,MOPMX-method}
\alias{csv_data,OPM-method}
\alias{csv_data,OPMS-method}
\usage{
  \S4method{csv_data}{MOPMX}(object, ...) 
  \S4method{csv_data}{OPM}(object,
    keys = character(), strict = TRUE,
    what = c("select", "filename", "setup_time", "position", "other"),
    normalize = FALSE) 
  \S4method{csv_data}{OPMS}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
