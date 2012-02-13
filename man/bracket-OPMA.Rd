\name{[,OPMA}
\alias{[,OPMA}
\title{Select subset (OPMA version)}
\arguments{
  \item{x}{\code{\link{OPMA}} object.}

  \item{i}{Row(s). Vector or missing.}

  \item{j}{Columns(s). Vector or missing.}

  \item{...}{Should currently not be set.}

  \item{drop}{Logical scalar. Remove the aggregated data
  and return an \code{\link{OPM}} object?}
}
\value{
  \code{\link{OPMA}} or \code{\link{OPM}} object.
}
\description{
  Select a subset of the measurements. This works much like
  \code{\link{[}}, but the function applies the subsetting
  to the original and the aggregated data in parallel. The
  aggregated data may also be dropped entirely; this might
  be appropriate if a subset of the time points is
  selected, potentially yielding aggregated values that do
  not fit to the measurements anymore.
}
\examples{
data(vaas_1)

copy <- vaas_1[]
stopifnot(has_aggr(copy))
stopifnot(identical(copy, vaas_1))

copy <- vaas_1[drop = TRUE]
stopifnot(!has_aggr(copy))
stopifnot(!identical(copy, vaas_1))
}
\seealso{
  [ [[

  Other getter-functions: \code{\link{[}},
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
\alias{bracket,OPMA-method}
\alias{[,OPMA-method}
\usage{
  \S4method{[}{OPMA}(x, i, j, ..., drop = FALSE) 
}
