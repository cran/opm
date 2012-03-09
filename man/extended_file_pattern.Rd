\name{extended_file_pattern}
\alias{extended_file_pattern}
\title{File pattern}
\usage{
  extended_file_pattern(arg, wildcard)
}
\arguments{
  \item{arg}{If a list, call \code{\link{file_pattern}}
  with it as arguments.  If \code{NULL}, return it
  unchanged. Otherwise convert it to character and
  optionally using \code{\link{glob_to_regex}}, depending
  on \code{wildcard}.}

  \item{wildcard}{Logical scalar. Convert \code{arg} using
  \code{\link{glob_to_regex}}?}
}
\value{
  Character scalar, holding a regular expression, or any
  object, unchanged.
}
\description{
  Create regexp matching certain file extensions.
}
\seealso{
  tools::file_ext
}
\keyword{internal}

