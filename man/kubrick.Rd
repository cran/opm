\name{kubrick}
\alias{kubrick}
\title{Stanley Kubrick}
\usage{
  kubrick(movie = character())
}
\arguments{
  \item{movie}{Character scalar (name of the movie) or
  convertible to such. If empty, the quotation is chosen
  randomly. Otherwise, partial matching is allowed.}
}
\value{
  Character scalar (quotation), returned invisibly.
}
\description{
  Stanley Kubrick memorial function. Prints a quote from
  one of his movies.
}
\examples{
x <- kubrick()
stopifnot(is.character(x), length(x) == 1L)
}
\keyword{utilities}

