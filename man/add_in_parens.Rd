\name{add_in_parens}
\alias{add_in_parens}
\title{Add note in parentheses}
\usage{
  add_in_parens(str.1, str.2, max = 1000L, append = ".",
    clean = TRUE, brackets = FALSE, word.wise = FALSE,
    paren.sep = " ")
}
\arguments{
  \item{str.1}{Character vector or convertible to such.}

  \item{str.2}{Character vector or convertible to such, to
  be added in parentheses. Trimming only affects
  \code{str.2}, and not the parentheses.}

  \item{brackets}{Logical scalar. Should brackets instead
  of parentheses be used?}

  \item{word.wise}{Logical scalar. Do abbreviation per
  word?}

  \item{paren.sep}{Character scalar. What to insert before
  the opening parenthesis (or bracket).}

  \item{max}{Numeric scalar. Maximum allowed length.}

  \item{append}{Character scalar. To be appended to strings
  that needed to be trimmed, for indicating just that.}

  \item{clean}{Logical scalar. If \code{TRUE}, clean
  trimmed end from non-word characters, and return empty
  string if only \code{append} remains.}
}
\value{
  Character vector.
}
\description{
  Append an annotation in parentheses to a string; trim it
  if necessary.
}
\keyword{internal}

