\name{assert_splittable_matrix}
\alias{assert_splittable_matrix}
\title{Check presence of split column}
\usage{
  assert_splittable_matrix(x, split.at)
}
\arguments{
  \item{x}{Data frame, matrix or array.}

  \item{split.at}{Names of columns at which \code{x} should
  be split.}
}
\value{
  Integer scalar indicating the split position. An error is
  raised if this is missing or non-unique.
}
\description{
  Check whether a certain column is present and not at the
  end of a data frame or matrix.
}
\keyword{internal}

