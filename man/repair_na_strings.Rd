\name{repair_na_strings}
\alias{repair_na_strings}
\title{Repair NAs}
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
  YAML input previously output by R, \sQuote{NA} values
  cause numeric vectors to be interpreted as character.
  This function fixes this problem and also takes care of
  misinterpreted numbers in exponential notation.
}
\references{
  \url{http://www.yaml.org/}
}
\keyword{internal}

\docType{methods}
\alias{repair_na_strings-methods}
\alias{repair_na_strings,character-method}
\alias{repair_na_strings,list-method}
\usage{
  \S4method{repair_na_strings}{character}(object) 

  \S4method{repair_na_strings}{list}(object,
    type = c("numeric", "integer", "complex")) 

}
