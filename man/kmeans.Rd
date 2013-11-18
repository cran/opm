\name{to_kmeans}
\alias{borders}
\alias{borders.Ckmeans.1d.dp}
\alias{borders.kmeans}
\alias{borders.kmeanss}
\alias{calinski}
\alias{calinski.Ckmeans.1d.dp}
\alias{calinski.kmeans}
\alias{calinski.kmeanss}
\alias{hist.Ckmeans.1d.dp}
\alias{hist.kmeans}
\alias{hist.kmeanss}
\alias{kmeans}
\alias{plot.kmeanss}
\alias{to_kmeans}
\alias{to_kmeans.Ckmeans.1d.dp}
\alias{to_kmeans.kmeans}
\alias{to_kmeans.kmeanss}
\title{Work with k-means results}
\usage{
  to_kmeans(x, ...)

  \method{to_kmeans}{kmeans} (x, ...)

  \method{to_kmeans}{kmeanss} (x, y, ...)

  \method{to_kmeans}{Ckmeans.1d.dp} (x, y, ...)

  calinski(x, ...)

  \method{calinski}{kmeans} (x, ...)

  \method{calinski}{Ckmeans.1d.dp} (x, y, ...)

  \method{calinski}{kmeanss} (x, ...)

  \method{plot}{kmeanss} (x, xlab = "Number of clusters",
    ylab = "Calinski-Harabasz statistics", ...)

  borders(x, ...)

  \method{borders}{kmeans} (x, y, ...)

  \method{borders}{Ckmeans.1d.dp} (x, y, ...)

  \method{borders}{kmeanss} (x, ...)

  \method{hist}{kmeans} (x, y, col = "black", lwd = 1L,
    lty = 1L, main = NULL, xlab = "Clustered values", ...)

  \method{hist}{Ckmeans.1d.dp} (x, y, ...)

  \method{hist}{kmeanss} (x, k = NULL, col = "black",
    lwd = 1L, lty = 1L, main = NULL,
    xlab = "Clustered values", ...)
}
\arguments{
  \item{x}{Object of class \code{kmeans},
  \sQuote{Ckmeans.1d.dp} or \code{kmeanss}. For
  \code{plot}, only the latter.}

  \item{y}{Vector of original data subjected to clustering.
  Automatically determined for the \code{kmeanss} methods.
  For \code{to_kmeans}, original numeric vector that was
  used to create a \sQuote{Ckmeans.1d.dp} object, or index
  of an element of a \code{kmeanss} object.}

  \item{k}{Numeric vector or \code{NULL}. If non-empty, it
  indicates the number of groups (previously used as input
  for \code{kmeans}) for which vertical lines should be
  drawn in the plot that represent the cluster borders. If
  empty, the smallest non-trivial number of clusters is
  chosen.}

  \item{col}{Graphical parameter passed to \code{abline}.
  If several values of \code{k} are given, \code{col} is
  recycled as necessary.}

  \item{lwd}{Like \code{col}.}

  \item{lty}{Like \code{col}.}

  \item{main}{Passed to \code{hist.default}.}

  \item{xlab}{Character scalar passed to
  \code{hist.default} or to \code{plot} from the
  \pkg{graphics} package.}

  \item{ylab}{Character scalar passed to \code{plot} from
  the \pkg{graphics} package.}

  \item{...}{Optional arguments passed to and from other
  methods. For the \code{hist} method, optional arguments
  passed to \code{hist.default}.}
}
\value{
  \code{to_kmeans} creates an object of class
  \code{kmeans}.

  \code{borders} creates a numeric vector or list of such
  vectors.

  The return value of the \code{hist} method is like
  \code{hist.default}; see there for details.

  \code{calinksi} returns a numeric vector with one element
  per \code{kmeans} object. \code{plot} returns it
  invisibly. Its \sQuote{names} attribute indicates the
  original numbers of clusters requested.
}
\description{
  Calculate or plot the Calinski-Harabasz statistics from
  \code{kmeans} results. The result of \code{plot} is a
  simple scatter plot which can be modified with arguments
  passed to \code{plot} from the \pkg{graphics} package.
  Alternatively, determine the borders between clusters of
  one-dimensional data, create a histogram in which these
  borders are plotted, or convert an object to one of class
  \code{kmeans}.
}
\details{
  The borders are calculated as the mean of the maximum of
  the cluster with the lower values and the minimum of the
  neighbouring cluster with the higher values. The
  \code{hist} method plots a histogram of one-dimensional
  data subjected to k-means partitioning in which these
  borders can be drawn.

  \code{y} must also be in the order it has been when
  subjected to clustering, but this is not checked. Using
  \code{kmeanss} objects thus might preferable in most
  cases because they contain a copy of the input data.
}
\examples{
x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
x.km <- run_kmeans(x, k = 1:10)

# plot() method
# the usual arguments of plot() are available
show(y <- plot(x.km, col = "blue", pch = 19))
stopifnot(is.numeric(y), names(y) == 1:10)

# borders() method
(x.b <- borders(x.km)) # => list of numeric vectors
stopifnot(is.list(x.b), length(x.b) == 10, sapply(x, is.numeric))
stopifnot(sapply(x.b, length) == as.numeric(names(x.b)) - 1)

# hist() methods
y <- hist(x.km[[2]], x, col = "blue", lwd = 2)
stopifnot(inherits(y, "histogram"))
y <- hist(x.km, 3:4, col = c("blue", "red"), lwd = 2)
stopifnot(inherits(y, "histogram"))

# to_kmeans() methods
x <- c(1, 2, 4, 5, 7, 8)
summary(y <- kmeans(x, 3))
stopifnot(identical(y, to_kmeans(y)))
# see particularly run_kmeans() which uses this internally if clustering is
# done with Ckmeans.1d.dp::Ckmeans.1d.dp()
}
\seealso{
  graphics::hist graphics::abline
  Ckmeans.1d.dp::Ckmeans.1d.dp

  Other kmeans-functions: \code{\link{run_kmeans}}
}
\keyword{cluster}
\keyword{hplot}
\keyword{manip}

