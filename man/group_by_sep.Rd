\name{group_by_sep}
\alias{group_by_sep}
\title{Grouping using a separator}
\arguments{
  \item{object}{Logical vector.}

  \item{pattern}{Character scalar passed to \code{grepl}.}

  \item{...}{Optional other arguments passed to
  \code{grepl}.}
}
\value{
  Factor, its length being the one of \code{object}. The
  levels correspond to a groups whose indices correspond to
  the index of a \code{TRUE} value in \code{object} plus
  the indices of the \code{FALSE} values immediately
  following it. The positions of \code{TRUE} values that
  are followed by \code{TRUE} values are set to \code{NA}.
}
\description{
  Treat a logical vector by regarding \code{TRUE} as
  indicating seperating. Create a factor that could be used
  with \code{split} to split the logical vector, or any
  equal-length object from which it was created, into
  according groups.
}
\seealso{
  split
}
\keyword{internal}

\docType{methods}
\alias{group_by_sep-methods}
\alias{group_by_sep,logical-method}
\usage{
  \S4method{group_by_sep}{logical}(object) 
}
