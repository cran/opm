\name{plate_type}
\alias{plate_type}
\title{Plate type used or normalized}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object, or character vector of original plate name(s), or
  factor.}

  \item{full}{Logical scalar. If \code{TRUE}, add (or
  replace by) the full name of the plate type (if
  available); otherwise, return it as-is.}

  \item{in.parens}{Logical scalar. This and the five next
  arguments work like the eponymous ones of
  \code{\link{wells}}, but here are applied to the plate
  name. See there for details.}

  \item{max}{Numeric scalar.}

  \item{clean}{Logical scalar.}

  \item{brackets}{Logical scalar.}

  \item{word.wise}{Logical scalar.}

  \item{paren.sep}{Character scalar.}

  \item{subtype}{Logical scalar. Keep the plate subtype
  indicator, if any?  Only relevant for the character or
  factor method.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Character scalar in the case of the \code{\link{OPM}} and
  \code{\link{OPMS}} methods, otherwise a character vector
  with the same length than \code{object}, or a
  corresponding factor.
}
\description{
  Get the type of the OmniLog(R) plate used in the
  measuring. This is a convenience function for one of the
  more important entries of \code{\link{csv_data}} with
  additional options useful for creating plot titles. The
  character method normalizes the names of OmniLog(R) PM
  plates to the internally used naming scheme. Unrecognized
  names are returned unchanged. This needs not normally be
  called by the \pkg{opm} user but might be of interest.
}
\examples{
## 'OPM' method
data(vaas_1)
(x <- plate_type(vaas_1, full = FALSE))
(y <- plate_type(vaas_1, full = TRUE))
(z <- plate_type(vaas_1, full = TRUE, in.parens = FALSE))
stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))

\dontrun{

# Splitting a list of 'OPM' objects according to the plate type is easy:
x <- split(x), sapply(x, plate_type))
# but see also opms() and read_opm() which can do this internally
}

## 'OPMS' method
data(vaas_4)
(xx <- plate_type(vaas_4, full = FALSE))
stopifnot(identical(x, xx))

## Character method

# Entirely unrecognized strings are returned as-is
x <- plate_type(letters)
stopifnot(identical(x, letters))

# Something more realistic
(x <- plate_type(y <- c("PM1", "PM-11C", "PMM04-a"), TRUE))
stopifnot(x != y)

# Factor method
(z <- plate_type(as.factor(y), TRUE))
stopifnot(is.factor(z), z == x)
}
\seealso{
  base::strtrim base::abbreviate base::gsub

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{show}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}
\keyword{character}
\keyword{utilities}

\docType{methods}
\alias{plate_type-methods}
\alias{plate_type,OPM-method}
\alias{plate_type,character-method}
\alias{plate_type,factor-method}
\alias{plate_type,OPMS-method}
\usage{
  \S4method{plate_type}{OPM}(object, full = FALSE, in.parens = TRUE,
    max = 100L, clean = TRUE, brackets = FALSE, word.wise = FALSE,
    paren.sep = " ") 

  \S4method{plate_type}{character}(object, subtype = FALSE) 

  \S4method{plate_type}{factor}(object, subtype = FALSE) 

  \S4method{plate_type}{OPMS}(object, ...) 

}
