\name{duplicated}
\alias{duplicated}
\title{Determine duplicated plates}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{incomparables}{Vector passed to \code{duplicated}
  from the \pkg{base} package. By default this is
  \code{FALSE}.}

  \item{what}{Indicating which parts of \code{x} should be
  compared. If a character scalar, the following entries
  are special: \describe{ \item{all}{Compares entire
  \code{OPM} objects.} \item{csv}{Compares the
  \acronym{CSV} data entries \code{\link{setup_time}} and
  \code{\link{position}}.} \item{metadata}{Compares the
  entire metadata content.} } If \code{what} does not match
  any of these, or is not a character scalar at all, it is
  passed as \code{key} argument to \code{\link{metadata}},
  and the resulting metadata subsets are compared.}

  \item{...}{Optional arguments passed to \code{duplicated}
  from the \pkg{base} package.}
}
\value{
  Logical vector.
}
\description{
  Check whether duplicated \code{\link{OPM}} or
  \code{\link{OPMA}} objects are contained within an
  \code{\link{OPMS}} object. For reasons of consistency,
  the \code{\link{OPM}} method always returns \code{FALSE}.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- duplicated(vaas_1)) # 1 element => nothing duplicated
stopifnot(identical(x, FALSE))

# 'OPMS' method
data(vaas_4)
stopifnot(!duplicated(vaas_4)) # => no complete plates duplicated
stopifnot(!duplicated(vaas_4, what = list("Species", "Strain")))
# => no organisms duplicated
stopifnot(duplicated(vaas_4, what = "Species") == rep(c(FALSE, TRUE), 2))
# => species duplicated
x <- vaas_4[c(1, 1)] # => complete plate duplicated
stopifnot(c(FALSE, TRUE) == duplicated(x))
}
\seealso{
  base::duplicated

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{subset}}, \code{\link{summary}},
  \code{\link{well}}, \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{duplicated-methods}
\alias{duplicated,OPM,ANY-method}
\alias{duplicated,OPMS,missing-method}
\alias{duplicated,OPMS,ANY-method}
\usage{
  \S4method{duplicated}{OPM,ANY}(x, incomparables, ...) 

  \S4method{duplicated}{OPMS,missing}(x, incomparables, ...) 

  \S4method{duplicated}{OPMS,ANY}(x, incomparables,
    what = c("all", "csv", "metadata"), ...) 

}
