\name{trim_string}
\alias{trim_string}
\title{Trim string}
\usage{
  trim_string(str, max, append = ".", clean = TRUE,
    word.wise = FALSE)
}
\arguments{
  \item{str}{Character vector or convertible to such.}

  \item{max}{Numeric scalar. Maximum allowed length.}

  \item{append}{Character scalar. To be appended to strings
  that needed to be trimmed, for indicating just that.}

  \item{clean}{Logical scalar. If \code{TRUE}, clean
  trimmed end from non-word characters, and return empty
  string if only \code{append} remains.}

  \item{word.wise}{Logical scalar. If \code{TRUE},
  abbreviate words separately, deleting vowels first.}
}
\value{
  Character vector.
}
\description{
  Trim a string to a given length, but by default append an
  indicator of whether something has been trimmed.
}
\keyword{internal}

