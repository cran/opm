\name{flatten}
\alias{flatten}
\title{Flatten matrix}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{include}{\code{NULL}, character vector or list. If
  not \code{NULL}, include this meta-information in the
  dataframe, replicated in each row.  Otherwise it
  converted to a list and passed to \code{\link{metadata}}.
  See there for details.}

  \item{fixed}{\code{NULL} or list. If not \code{NULL},
  include these items in the dataframe, replicated in each
  row.}

  \item{factors}{Logical scalar. See the
  \sQuote{stringsAsFactors} argument of \code{data.frame}
  and \code{as.data.frame}.}

  \item{exact}{Logical scalar. Passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar. Passed to
  \code{\link{metadata}}.}

  \item{full}{Logical scalar. Replace well coordinates by
  full names?}

  \item{...}{Optional other arguments passed to
  \code{\link{wells}}, or from the \code{\link{OPMS}} to
  the \code{\link{OPM}} method.}
}
\value{
  Dataframe. Column names are unchecked (not converted to
  variable names). The three last columns are:
  \sQuote{Time}, \sQuote{Well}, \sQuote{Value}, with the
  obvious meanings. The \code{\link{OPMS}} method yields an
  additional column named \sQuote{Plate}, which contains
  each plate's number within \code{object}.
}
\description{
  Convert into \sQuote{flat} dataframe, including all
  measurements in a single column (suitable, e.g., for
  \pkg{lattice}).
}
\examples{
# OPM method
data(vaas_1)
x <- flatten(vaas_1)
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 3L)))
x <- flatten(vaas_1, fixed = "TEST")
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 4L)))
x <- flatten(vaas_1, fixed = "TEST", include = "Strain")
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 5L)))

# OPMS method
data(vaas_4)
x <- flatten(vaas_4)
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 4L)))
x <- flatten(vaas_4, fixed = "TEST")
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 5L)))
x <- flatten(vaas_4, fixed = "TEST", include = "Strain")
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 6L)))
}
\seealso{
  stats::reshape

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{to_yaml}}
}
\keyword{dplot}
\keyword{manip}

\docType{methods}
\alias{flatten-methods}
\alias{flatten,OPM-method}
\alias{flatten,OPMS-method}
\usage{
  \S4method{flatten}{OPM}(object, include = NULL, fixed = NULL,
    factors = TRUE, exact = TRUE, strict = TRUE, full = TRUE, ...) 

  \S4method{flatten}{OPMS}(object, include = NULL, fixed = list(),
    ...) 

}
