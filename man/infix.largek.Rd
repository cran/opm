\name{\%K\%}
\alias{\%K\%}
\alias{infix.largek}
\title{Search in metadata keys}
\arguments{
  \item{x}{Character vector or list.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a character vector as query, this method tests
  whether a given key is present in the metadata and
  fetches an object that is not \code{NULL}. If the key has
  a length > 1, sublists are queried. An empty vector
  results in \code{TRUE}. Note that the values of the
  character vector, not its names, if any, are used for
  querying the metadata. Using a list as query, this
  function behaves like \code{\link{infix.k}}.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' but
# neither 'Trial' nor 'Organism' nor 'Run':
data(vaas_1)

# Character method

# Single-element queries
stopifnot("Experiment" \%K\% vaas_1)
stopifnot("Species" \%K\% vaas_1)
stopifnot(!"Run" \%K\% vaas_1)
stopifnot(!"Trial" \%K\% vaas_1)
stopifnot(!"Organism" \%k\% vaas_1)

# Zero-element queries
stopifnot(character() \%K\% vaas_1)

# Querying with vectors of length > 1 mean nested queries; compare this to
# the behavior of \%k\%!
stopifnot(!c("Species", "Experiment") \%K\% vaas_1)

# List method
# See \%k\% -- the behavior is identical for lists.
}
\keyword{attribute}

\docType{methods}
\alias{infix.largek-methods}
\alias{infix.largek,character,WMD-method}
\alias{\%K\%,character,WMD-method}
\alias{infix.largek,list,WMD-method}
\alias{\%K\%,list,WMD-method}
\usage{
  \S4method{\%K\%}{character,WMD}(x, table) 

  \S4method{\%K\%}{list,WMD}(x, table) 

}
