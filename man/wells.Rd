\name{wells}
\alias{wells}
\title{Available well names}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

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
\note{
  Do not confuse this with \code{\link{well}}.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- wells(vaas_1, full = FALSE))
(y <- wells(vaas_1, full = TRUE))
(z <- wells(vaas_1, full = TRUE, in.parens = FALSE))
stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))

# 'OPM' method
data(vaas_4)
(xx <- wells(vaas_4, full = FALSE))
stopifnot(identical(x, xx))
}
\seealso{
  base::strtrim base::abbreviate

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{well}}
}
\keyword{attribute}

\docType{methods}
\alias{wells-methods}
\alias{wells,OPM-method}
\alias{wells,OPMS-method}
\usage{
  \S4method{wells}{OPM}(object, full = FALSE, in.parens = TRUE,
    max = 100L, brackets = FALSE, clean = TRUE, word.wise = FALSE,
    paren.sep = " ") 

  \S4method{wells}{OPMS}(object, ...) 

}
