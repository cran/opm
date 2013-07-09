\name{wells}
\alias{wells}
\title{Available well names}
\arguments{
  \item{object}{\code{\link{OPM}} object,
  \code{\link{OPMS}} object or well name or index. If
  missing, defaults to the selection of all possible
  wells.}

  \item{full}{Logical scalar. Return the full names of the
  wells (if available) or just their coordinates on the
  plate? The following arguments have no effect if
  \code{full} is \code{FALSE}.}

  \item{in.parens}{Logical scalar. If \code{TRUE}, add the
  full name of the substrate in parentheses (or brackets)
  after the original name. If \code{FALSE}, replace by the
  full substrate name. Note that adding in parentheses (or
  brackets) is only done if the trimmed substrate names are
  not empty.}

  \item{max}{Numeric scalar. Maximum number of characters
  allowed in the names.  Longer names are truncated and the
  truncation is indicated by appending a dot.}

  \item{brackets}{Logical scalar. Use brackets instead of
  parentheses?}

  \item{clean}{Logical scalar. If \code{TRUE}, clean
  trimmed end of full substrate name from non-word
  characters; use an empty string if only the dot
  remained.}

  \item{word.wise}{Logical scalar. If \code{TRUE},
  abbreviation works by truncating each word separately,
  and removing vowels first.}

  \item{paren.sep}{Character scalar. What to insert before
  the opening parenthesis (or bracket).}

  \item{downcase}{Logical scalar indicating whether full
  names should be (carefully) converted to lower case. This
  uses \code{\link{substrate_info}} in \sQuote{downcase}
  mode; see there for details.}

  \item{plate}{Name of the plate type. Several ones can be
  given unless \code{object} is of class \code{\link{OPM}}
  or \code{\link{OPMS}}.  \code{\link{plate_type}} is
  applied before searching for the substrate names, and
  partial matching is allowed.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Character vector.
}
\description{
  Get the names of the wells contained in an
  \code{\link{OPM}} object. Optionally the full substrate
  names can be added in parentheses or brackets or used
  instead of the coordinate, and trimmed to a given length.
}
\details{
  The purpose of the \code{\link{OPM}} and
  \code{\link{OPMS}} methods should be obvious. The default
  method is intended for providing a quick overview of the
  substrates contained in one to several plates if
  \code{full} is \code{TRUE}. If \code{full} is
  \code{FALSE}, it can be used to study the effect of the
  well-index translation and well-name normalization
  approaches as used by \pkg{opm}, particularly by the
  subsetitting methods (see \code{\link{[}}).
}
\note{
  Do not confuse this with \code{\link{well}}.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- wells(vaas_1, full = FALSE))[1:10]
(y <- wells(vaas_1, full = TRUE))[1:10]
(z <- wells(vaas_1, full = TRUE, in.parens = FALSE))[1:10]
# string lengths differ depending on selection
stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))

# 'OPM' method
data(vaas_4)
(xx <- wells(vaas_4, full = FALSE))[1:10]
# wells are guaranteed to be uniform within OPMS objects
stopifnot(identical(x, xx))

# default method
x <- c("A01", "B10")
(y <- wells(x, plate = "PM1"))
stopifnot(nchar(y) > nchar(x))
(z <- wells(x, plate = "PM1", in.parens = TRUE))
stopifnot(nchar(z) > nchar(y))
# formula yields same result (except for row names)
stopifnot(y == wells(~ c(A01, B10), plate = "PM1"))
# using a sequence of well coordinates
stopifnot(nrow(wells(~ C02:C06)) == 5) # well sequence
stopifnot(nrow(wells(plate = "PM1")) == 96) # all wells by default
}
\seealso{
  base::strtrim base::abbreviate

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{subset}}, \code{\link{summary}},
  \code{\link{well}}
}
\keyword{attribute}

\docType{methods}
\alias{wells-methods}
\alias{wells,OPM-method}
\alias{wells,ANY-method}
\alias{wells,missing-method}
\alias{wells,OPMS-method}
\usage{
  \S4method{wells}{OPM}(object, full = FALSE, in.parens = TRUE,
    max = 100L, brackets = FALSE, clean = TRUE, word.wise = FALSE,
    paren.sep = " ", downcase = FALSE, plate = plate_type(object)) 

  \S4method{wells}{ANY}(object, full = TRUE, in.parens = FALSE,
    max = 100L, brackets = FALSE, clean = TRUE, word.wise = FALSE,
    paren.sep = " ", downcase = FALSE, plate = "PM01") 

  \S4method{wells}{missing}(object, ...) 

  \S4method{wells}{OPMS}(object, ...) 

}
