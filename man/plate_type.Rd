\name{plate_type}
\alias{plate_type}
\title{Plate type used}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{full}{Logical scalar. If \code{TRUE}, add (or
  replace by) the full name of the plate type (if
  available); otherwise, return it as-is.}

  \item{in.parens}{Logical scalar. This and the following
  arguments work like the eponymous ones of
  \code{\link{wells}}, but here are applied to the plate
  name. See there for details.}

  \item{max}{Numeric scalar.}

  \item{clean}{Logical scalar.}

  \item{brackets}{Logical scalar.}

  \item{word.wise}{Logical scalar.}

  \item{paren.sep}{Character scalar.}
}
\value{
  Character scalar.
}
\description{
  Get the type of the OmniLog(R) plate used in the
  measuring. This is a convenience function for one of the
  more important entries of \code{\link{csv_data}} with
  additional options useful for creating plot titles.
}
\examples{
data(vaas_1)
(x <- plate_type(vaas_1, full = FALSE))
(y <- plate_type(vaas_1, full = TRUE))
(z <- plate_type(vaas_1, full = TRUE, in.parens = FALSE))
stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))

\dontrun{

# Splitting a list of 'OPM' objects according to the plate type is easy:
x <- split(x), sapply(x, plate_type))
}
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
  \code{\link{minmax}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{plate_type-methods}
\alias{plate_type,OPM-method}
\usage{
  \S4method{plate_type}{OPM}(object, full = FALSE, in.parens = TRUE,
    max = 100L, clean = TRUE, brackets = FALSE, word.wise = FALSE, 
    paren.sep = " ") 

}
