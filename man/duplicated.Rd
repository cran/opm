\name{duplicated}
\alias{duplicated}
\title{Determine duplicated plates}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{incomparables}{Vector passed to \code{duplicated}
  from the \pkg{base} package.}

  \item{what}{Character scalar indicating which parts of
  \code{x} should be compared. \sQuote{all} compares entire
  \code{OPM} objects; \sQuote{csv} compares the CSV data
  entries \code{\link{setup_time}} and
  \code{\link{position}}; \sQuote{metadata} compares the
  entire metadata content. If \code{what} does not match
  any of these, it is passed as \code{key} argument to
  \code{\link{metadata}}, and the resulting metadata
  subsets are compared.}

  \item{...}{Optional arguments passed to \code{duplicated}
  from the \pkg{base} package.}
}
\value{
  Logical vector.
}
\description{
  Check whether duplicated \code{\link{OPM}} or
  \code{\link{OPMA}} objects are contained within an
  \code{\link{OPMS}} object.
}
\examples{
data(vaas_4)
stopifnot(!duplicated(vaas_4))
stopifnot(duplicated(vaas_4, what = "Species") == rep(c(FALSE, TRUE), 2))
x <- vaas_4[c(1, 1)]
stopifnot(c(FALSE, TRUE) == duplicated(x))
}
\seealso{
  base::duplicate

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{duplicated-methods}
\alias{duplicated,OPMS-method}
\usage{
  \S4method{duplicated}{OPMS}(x, incomparables = FALSE,
    what = c("all", "csv", "metadata"), ...) 

}
