\name{repair_na_strings}
\alias{repair_na_strings}
\alias{repair_na_strings.character}
\alias{repair_na_strings.list}
\title{Repair NAs}
\usage{
  repair_na_strings(object, ...)

  \method{repair_na_strings}{character} (object, ...)

  \method{repair_na_strings}{list} (object,
    type = c("double", "integer", "complex", "logical", "character"),
    ...)
}
\arguments{
  \item{object}{Character vector or list.}

  \item{type}{Character scalar denoting the type to which
  input character vectors shall be tried to be converted.}
}
\value{
  Character vector or list.
}
\description{
  Replace \sQuote{NA} by \code{NA_character_}. When reading
  \acronym{YAML} input previously output by \R, \sQuote{NA}
  values cause numeric vectors to be interpreted as
  character. This function fixes this problem.
}
\details{
  This problem does not occur anymore with the \pkg{yaml}
  package of at least version 2.1.7, but for legacy
  \acronym{YAML} (and \acronym{JSON}) files it is necessary
  to conduct the conversions implemented here.
}
\references{
  \url{http://www.yaml.org/}
}
\seealso{
  utils::type.convert
}
\keyword{internal}

