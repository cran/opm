\name{flatten}
\alias{flatten}
\title{Flatten measurements from OPM or OPMS objects}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object or list.}

  \item{include}{\code{NULL}, character vector, list or
  formula. If not empty, include this meta-information in
  the data frame, replicated in each row.  Otherwise it
  converted to a list and passed to \code{\link{metadata}}.
  See there for details.}

  \item{fixed}{\code{NULL} or list. If not \code{NULL},
  include these items in the data frame, replicated in each
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
  Data frame. Column names are unchecked (not converted to
  variable names). The three last columns are coding for
  time, well and value, with the exact spelling of the
  column names given by \code{\link{param_names}}.

  The \code{\link{OPMS}} method yields an additional column
  for the plate, the exact spelling of its name also being
  available via \code{\link{param_names}}. This column
  contains the position of each plate within \code{object}.
}
\description{
  Convert into \sQuote{flat} data frame, including all
  measurements in a single column (suitable, e.g., for
  \pkg{lattice}).
}
\examples{
# OPM method
data(vaas_1)
# distinct numbers of columns due to distinct selection settings
head(x <- flatten(vaas_1))
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 3L)))
head(x <- flatten(vaas_1, fixed = "TEST"))
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 4L)))
head(x <- flatten(vaas_1, fixed = "TEST", include = "Strain"))
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 5L)))

# OPMS method
data(vaas_4)
# distinct numbers of columns due to distinct selection settings
head(x <- flatten(vaas_4))
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 4L)))
head(x <- flatten(vaas_4, fixed = "TEST"))
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 5L)))
head(x <- flatten(vaas_4, fixed = "TEST", include = ~ Strain))
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 6L)))
}
\seealso{
  stats::reshape

  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{extract}}, \code{\link{extract_columns}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
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
