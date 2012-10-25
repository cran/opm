\name{borders}
\alias{borders}
\alias{borders.Ckmeans.1d.dp}
\alias{borders.kmeans}
\alias{borders.kmeanss}
\alias{hist.Ckmeans.1d.dp}
\alias{hist.kmeans}
\alias{hist.kmeanss}
\title{Cluster borders}
\usage{
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
  \item{x}{Object of class \sQuote{kmeans},
  \sQuote{Ckmeans.1d.dp} or \sQuote{kmeanss}.}

  \item{y}{Vector of original data subjected to clustering.
  Automatically determined for the \sQuote{kmeanss}
  methods.}

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

  \item{xlab}{Passed to \code{hist.default}.}

  \item{...}{Optional arguments passed to and from other
  methods. For the \code{hist} method, optional arguments
  passed to \code{hist.default}.}
}
\value{
  Numeric vector or list of such vectors. For the
  \code{hist} method, like \code{hist.default}; see there
  for details.
}
\description{
  Determine the borders between clusters of one-dimensional
  data. They are calculated as the mean of the maximum of
  the cluster with the lower values and the minimum of the
  neighboring cluster with the higher values. The
  \code{hist} method plots a histogram of one-dimensional
  data subjected to k-means partitioning in which these
  borders can be drawn.
}
\note{
  \code{y} must also be in the order it has been when
  subjected to clustering, but this is not checked. Using
  \sQuote{kmeanss} objects thus might preferable in most
  cases because they contain a copy of the input data.
}
\examples{
data(vaas_4)
x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
x.km <- run_kmeans(x, k = 1:10)

# borders() method
(x.b <- borders(x.km))
stopifnot(is.list(x.b), length(x.b) == 10, sapply(x, is.numeric))
stopifnot(sapply(x.b, length) == as.numeric(names(x.b)) - 1)

# hist() methods
y <- hist(x.km[[2]], x, col = "blue", lwd = 2)
stopifnot(inherits(y, "histogram"))
y <- hist(x.km, 3:4, col = c("blue", "red"), lwd = 2)
stopifnot(inherits(y, "histogram"))
}
\seealso{
  graphics::hist graphics::abline

  Other kmeans-functions: \code{\link{calinski}},
        \code{\link{plot.kmeanss}}, \code{\link{run_kmeans}},
  \code{\link{to_kmeans}},
      }
\keyword{cluster}
\keyword{hplot}

