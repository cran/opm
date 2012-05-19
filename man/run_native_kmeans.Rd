\name{run_native_kmeans}
\alias{run_native_kmeans}
\title{Run native k-means}
\usage{
  run_native_kmeans(x, k, args)
}
\arguments{
  \item{x}{Numeric vector or matrix.}

  \item{k}{Numeric vector.}

  \item{args}{Argument list passed to \code{kmeans} form
  the \pkg{stats} package.}
}
\value{
  Named integer vector.
}
\description{
  Auxiliary function for checking and slightly adapting k
  for k-means partitioning.
}
\keyword{internal}

