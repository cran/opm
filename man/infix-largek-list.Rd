\name{\%K\%,list}
\alias{\%K\%,list}
\alias{infix-largek,list}
\title{Search in metadata keys (list version)}
\arguments{
  \item{x}{List.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a list as query, this function behaves like
  \code{\link{infix-k}}.
}
\note{
  There is also a character-based version,
  \code{\link{infix-largek}}.
}
\examples{
# See \%k\% -- the behaviour is identical for lists.
}
\keyword{attribute}

\docType{methods}
\alias{infix-largek,list,WMD-method}
\alias{\%K\%,list,WMD-method}
\usage{
  \S4method{\%K\%}{list,WMD}(x, table) 
}
