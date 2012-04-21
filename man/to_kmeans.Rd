\name{to_kmeans}
\alias{to_kmeans}
\alias{to_kmeans.Ckmeans.1d.dp}
\alias{to_kmeans.kmeans}
\alias{to_kmeans.kmeanss}
\title{Convert to kmeans}
\usage{
  to_kmeans(x, ...)

  \method{to_kmeans}{kmeans} (x, ...)

  \method{to_kmeans}{kmeanss} (x, y, ...)

  \method{to_kmeans}{Ckmeans.1d.dp} (x, y, ...)
}
\arguments{
  \item{x}{Object to be converted.}

  \item{y}{Original numeric vector that was used to create
  a \sQuote{Ckmeans.1d.dp} object, or index of an element
  of a \sQuote{kmeanss} object.}

  \item{...}{Optional arguments passed to and from other
  methods, and/or between the methods.}
}
\value{
  Object of class \sQuote{kmeans}.
}
\description{
  Convert an object to one of class \sQuote{kmeans}.
}
\examples{
x <- c(1, 2, 4, 5, 7, 8)
summary(y <- kmeans(x, 3))
stopifnot(identical(y, to_kmeans(y)))
# see particularly run_kmeans() which uses this internally if clustering is
# done with Ckmeans.1d.dp::Ckmeans.1d.dp()
}
\seealso{
  Ckmeans.1d.dp::Ckmeans.1d.dp

  Other kmeans-functions: \code{\link{borders}},
  \code{\link{borders.Ckmeans.1d.dp}},
  \code{\link{borders.kmeans}},
  \code{\link{borders.kmeanss}}, \code{\link{calinski}},
  \code{\link{calinski.Ckmeans.1d.dp}},
  \code{\link{calinski.kmeans}},
  \code{\link{calinski.kmeanss}},
  \code{\link{hist.Ckmeans.1d.dp}},
  \code{\link{hist.kmeans}}, \code{\link{hist.kmeanss}},
  \code{\link{plot.kmeanss}}, \code{\link{run_kmeans}}
}
\keyword{manip}

