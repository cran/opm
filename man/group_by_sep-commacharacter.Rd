\name{group_by_sep,character}
\alias{group_by_sep,character}
\title{Grouping using a separator (character version)}
\arguments{
  \item{object}{Character vector.}

  \item{pattern}{Character scalar passed to \code{grepl}.}

  \item{invert}{Logical scalar. Invert the result of
  pattern matching with \code{grepl}? If so, unmatched
  lines are treated as seperators.}

  \item{...}{Optional arguments passed to \code{grepl}.}
}
\value{
  Factor, its length being the one of \code{object}.
}
\description{
  Grep for a pattern in a a character vector, thus creating
  a logical vector indicating the matches. Then use this to
  construct a factor with \code{\link{group_by_sep}}.
}
\seealso{
  split
}
\keyword{internal}

\docType{methods}
\alias{group_by_sep,character-method}
\usage{
  \S4method{group_by_sep}{character}(object, pattern, 
    invert = FALSE, ...) 
}
