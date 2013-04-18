\name{run_kmeans}
\alias{run_kmeans}
\title{Conduct k-means partitioning}
\arguments{
  \item{object}{Numeric vector or matrix.}

  \item{k}{Numeric vector. Number of clusters requested.}

  \item{nstart}{Numeric scalar. Ignored if
  \sQuote{Ckmeans.1d.dp} is called.  Otherwise passed to
  \sQuote{kmeans} from the \pkg{stats} package.}

  \item{...}{List of optional arguments passed to
  \sQuote{kmeans} from the \pkg{stats} package.}
}
\value{
  S3 object of class \sQuote{kmeanss}.
}
\description{
  Run a k-means partitioning analysis. This function is
  used by \code{\link{discrete}} in \sQuote{gap} mode to
  automatically determine the range of ambiguous data. If
  applied to such one-dimensional data, it uses an exact
  algorithm from the \pkg{Ckmeans.1d.dp} package.
}
\examples{
data(vaas_4)
x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
summary(x.km <- run_kmeans(x, k = 1:10)) # => 'kmeanss' object
stopifnot(inherits(x.km, "kmeanss"), length(x.km) == 10)
stopifnot(sapply(x.km, class) == "kmeans", names(x.km) == 1:10)
}
\references{
  Wang, H., Song, M. 2011 Ckmeans.1d.dp: Optimal k-means
  clustering in one dimension by dynamic programming.
  \emph{The R Journal} \strong{3}, p. 29--33.
}
\seealso{
  stats::kmeans Ckmeans.1d.dp::Ckmeans.1d.dp

  Other kmeans-functions: \code{\link{borders}},
      \code{\link{calinski}},
        \code{\link{hist.Ckmeans.1d.dp}},
  \code{\link{hist.kmeans}}, \code{\link{hist.kmeanss}},
  \code{\link{plot.kmeanss}}, \code{\link{to_kmeans}},
      }
\keyword{cluster}

\docType{methods}
\alias{run_kmeans-methods}
\alias{run_kmeans,numeric,numeric-method}
\alias{run_kmeans,matrix,numeric-method}
\usage{
  \S4method{run_kmeans}{numeric,numeric}(object, k) 

  \S4method{run_kmeans}{matrix,numeric}(object, k,
    nstart = 10L, ...) 

}
