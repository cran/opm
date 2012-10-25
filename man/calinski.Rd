\name{calinski}
\alias{calinski}
\alias{calinski.Ckmeans.1d.dp}
\alias{calinski.kmeans}
\alias{calinski.kmeanss}
\alias{plot.kmeanss}
\title{Calinski-Harabasz statistics}
\usage{
  calinski(x, ...)

  \method{calinski}{kmeans} (x, ...)

  \method{calinski}{Ckmeans.1d.dp} (x, y, ...)

  \method{calinski}{kmeanss} (x, ...)

  \method{plot}{kmeanss} (x, xlab = "Number of clusters",
    ylab = "Calinski-Harabasz statistics", ...)
}
\arguments{
  \item{x}{Object of class \sQuote{kmeans},
  \sQuote{Ckmeans.1d.dp} or \sQuote{kmeanss}. For
  \code{plot}, only the latter.}

  \item{xlab}{Character scalar passed to \code{plot} from
  the \pkg{graphics} package.}

  \item{ylab}{Character scalar passed to \code{plot}.}

  \item{...}{Optional arguments passed to and from other
  methods, and/or between the methods.}

  \item{y}{Original numeric vector that was used to create
  a \sQuote{Ckmeans.1d.dp} object, or index of an element
  of a \sQuote{kmeanss} object.}
}
\value{
  \code{calinksi} returns a numeric vector with one element
  per \sQuote{kmeans} object. \code{plot} returns it
  invisibly. Its \sQuote{names} attribute indicates the
  original numbers of clusters requested.
}
\description{
  Calculate or plot the Calinski-Harabasz statistics from
  \code{kmeans} results. The result of \code{plot} is a
  simple scatterplot which can be modified with arguments
  passed to \code{plot} from the \pkg{graphics} package.
}
\examples{
data(vaas_4)
x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
x.km <- run_kmeans(x, k = 1:10)
# the usual arguments of plot() are available
show(y <- plot(x.km, col = "blue", pch = 19))
stopifnot(is.numeric(y), names(y) == 1:10)
}
\seealso{
  Other kmeans-functions: \code{\link{borders}},
        \code{\link{hist.Ckmeans.1d.dp}},
  \code{\link{hist.kmeans}}, \code{\link{hist.kmeanss}},
  \code{\link{run_kmeans}}, \code{\link{to_kmeans}},
      }
\keyword{cluster}
\keyword{hplot}

