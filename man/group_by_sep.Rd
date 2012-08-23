\name{group_by_sep}
\alias{group_by_sep}
\title{Grouping using a separator}
\arguments{
  \item{object}{Logical or character vector, or factor.}

  \item{include}{Logical scalar indicating whether the
  separator positions should also be included in the factor
  levels instead of being coded as \code{NA}.}

  \item{pattern}{Character scalar passed to \code{grepl}.}

  \item{invert}{Logical scalar. Invert the result of
  pattern matching with \code{grepl}? If so, unmatched
  lines are treated as separators.}

  \item{...}{Optional arguments passed to \code{grepl} or
  between the methods.}
}
\value{
  Factor, its length being the one of \code{object}. The
  levels correspond to a groups whose indices correspond to
  the index of a \code{TRUE} value in \code{object} plus
  the indices of the \code{FALSE} values immediately
  following it. The positions of \code{TRUE} values that
  are followed by \code{TRUE} values are set to \code{NA}
  (irrespective of \code{include}).
}
\description{
  For the \sQuote{logical} method, treat a logical vector
  by regarding \code{TRUE} as indicating separating. Create
  a factor that could be used with \code{split} to split
  the logical vector, or any equal-length object from which
  it was created, into according groups. For the character
  method, grep for a pattern in a character vector, thus
  creating a logical vector indicating the matches. Then
  use this to construct a factor with the \sQuote{logical}
  method.
}
\seealso{
  base::split base::grepl
}
\keyword{internal}

\docType{methods}
\alias{group_by_sep-methods}
\alias{group_by_sep,logical-method}
\alias{group_by_sep,character-method}
\alias{group_by_sep,factor-method}
\usage{
  \S4method{group_by_sep}{logical}(object, include = TRUE) 

  \S4method{group_by_sep}{character}(object, pattern,
    invert = FALSE, include = TRUE, ...) 

  \S4method{group_by_sep}{factor}(object, ...) 

}
