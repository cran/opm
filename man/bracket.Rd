\name{[}
\alias{[}
\title{Select subset}
\arguments{
  \item{x}{\code{\link{OPM}} object.}

  \item{i}{Row(s). Vector or missing.}

  \item{j}{Columns(s). Vector or missing.}

  \item{...}{Should currently not be set.}

  \item{drop}{Logical scalar. Has no effect here, but see
  \code{\link{[,OPMA}}.}
}
\value{
  \code{\link{OPM}} object.
}
\description{
  Select a subset of the measurements. Return other slots
  unchanged. In contrast to the usual `[` functions, always
  return a matrix (as a component of the returned OPM
  object), even if it could be simplified to a vector. The
  time column is not counted and always copied. It is an
  error to delete the entire matrix. In all other respects,
  this method behaves like the `[` functions from the
  \pkg{base} package.
}
\examples{
data(vaas_1)
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))
copy <- vaas_1[, 11:22]
(x <- dim(copy))
stopifnot(identical(x, c(384L, 12L)))
}
\seealso{
  [ [[

  Other getter-functions: \code{\link{[,OPMA}},
  \code{\link{[,OPMS}}, \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{dim,OPMS}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{max,OPMS}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{manip}

\docType{methods}
\alias{bracket-methods}
\alias{bracket,OPM-method}
\alias{[,OPM-method}
\usage{
  \S4method{[}{OPM}(x, i, j, ..., drop = FALSE) 
}
