\name{run_kmeans}
\alias{run_kmeans}
\title{Conduct k-means partitioning}
\arguments{
  \item{object}{Numeric vector or matrix.}

  \item{k}{Numeric vector. Number of clusters requested.}

  \item{program}{Character scalar. The underlying
  clustering program to use.  \sQuote{Ckmeans.1d.dp} is
  recommended because it exactly solves the k-means
  optimization problem for one-dimensional data, but it
  requires the installation of the eponymous package and
  cannot be applied to matrix input.}

  \item{kmeans.args}{List of optional arguments passed to
  \sQuote{kmeans} from the \pkg{stats} package.}
}
\value{
  S3 object of class \sQuote{kmeanss}.
}
\description{
  Run a k-means partitioning analysis. This function is
  currently only useable for \strong{one-dimensional} data.
  It is used by \code{\link{discrete}} in \sQuote{gap} mode
  to automatically determine the range of ambiguous data.
}
\examples{
data(vaas_4)
x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
summary(x.km <- run_kmeans(x, k = 1:10))
stopifnot(inherits(x.km, "kmeanss"), length(x.km) == 10)
stopifnot(sapply(x.km, class) == "kmeans", names(x.km) == 1:10)
}
\seealso{
  stats::kmeans Ckmeans.1d.dp::Ckmeans.1d.dp

  Other kmeans-functions: \code{\link{borders}},
  \code{\link{borders.Ckmeans.1d.dp}},
  \code{\link{borders.kmeans}},
  \code{\link{borders.kmeanss}}, \code{\link{calinski}},
  \code{\link{calinski.Ckmeans.1d.dp}},
  \code{\link{calinski.kmeans}},
  \code{\link{calinski.kmeanss}},
  \code{\link{hist.Ckmeans.1d.dp}},
  \code{\link{hist.kmeans}}, \code{\link{hist.kmeanss}},
  \code{\link{plot.kmeanss}}, \code{\link{to_kmeans}},
  \code{\link{to_kmeans.Ckmeans.1d.dp}},
  \code{\link{to_kmeans.kmeans}},
  \code{\link{to_kmeans.kmeanss}}
}
\keyword{cluster}

\docType{methods}
\alias{run_kmeans-methods}
\alias{run_kmeans,numeric,numeric-method}
\alias{run_kmeans,matrix,numeric-method}
\usage{
  \S4method{run_kmeans}{numeric,numeric}(object, k,
    program = c("Ckmeans.1d.dp", "kmeans"), kmeans.args = list()) 

  \S4method{run_kmeans}{matrix,numeric}(object, k, 
    kmeans.args = list(nstart = 10L)) 

}
