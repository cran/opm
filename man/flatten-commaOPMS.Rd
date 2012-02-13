\name{flatten,OPMS}
\alias{flatten,OPMS}
\title{Flatten matrix (OPMS version)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{include}{See \code{\link{flatten}}.}

  \item{fixed}{See \code{\link{flatten}}.}

  \item{...}{Parameters passed to \code{\link{flatten}}.}
}
\value{
  Dataframe. See \code{\link{flatten}}. An additional
  column name is \sQuote{Plate}, which contains each
  plate's number within \code{object}.
}
\description{
  Convert into \sQuote{flat} dataframe, including all
  measurements in a single column (suitable for
  \pkg{lattice}).
}
\examples{
data(vaas_4)
x <- flatten(vaas_4)
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 4L)))
x <- flatten(vaas_4, fixed = "TEST")
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 5L)))
x <- flatten(vaas_4, fixed = "TEST", include = "Strain")
stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 6L)))
}
\seealso{
  reshape

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{plates}}, \code{\link{to_yaml}}
}
\keyword{dplot}
\keyword{manip}

\docType{methods}
\alias{flatten,OPMS-method}
\usage{
  \S4method{flatten}{OPMS}(object, include = NULL, fixed = list(),
    ...) 
}
