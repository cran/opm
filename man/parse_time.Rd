\name{parse_time}
\alias{parse_time}
\title{Parse time strings}
\arguments{
  \item{object}{Character vector.}
}
\value{
  Object of class \sQuote{POSIXlt}.
}
\description{
  Parse time strings by trying potentially many formats in
  turn. Each subsequent format is only applied to the
  \code{NA} values created in the previous attempt, if any.
  A warning is issued if final \code{NA} values remain.
}
\seealso{
  base::strptime
}
\keyword{internal}

\docType{methods}
\alias{parse_time-methods}
\alias{parse_time,character,missing-method}
\alias{parse_time,character,character-method}
\usage{
  \S4method{parse_time}{character,missing}(object, format,
    tz = opm_opt("time.zone")) 

  \S4method{parse_time}{character,character}(object, format,
    tz = "") 

}
