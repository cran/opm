\name{\%k\%}
\alias{\%k\%}
\alias{infix-k}
\title{Search in metadata keys}
\arguments{
  \item{x}{Character vector.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a character vector as query, this method tests
  whether all given keys are present in the top-level names
  of the metadata (these may be nested, but all sublists
  are ignored here). An empty query vector results in
  \code{TRUE}. Note that the values of the character
  vector, not its names, if any, are used for querying the
  metadata.
}
\note{
  There is also a list-based version,
  \code{\link{infix-k,list}}.
}
\examples{
data(vaas_1)
# The dataset contains the metadata keys 'Species' and 'Experiment' but
# neither 'Trial' nor 'Organism' nor 'Run':
stopifnot("Experiment" \%k\% vaas_1)
stopifnot("Species" \%k\% vaas_1)
stopifnot(!"Run" \%k\% vaas_1)
stopifnot(c("Species", "Experiment") \%k\% vaas_1)
stopifnot(!c("Species", "Trial") \%k\% vaas_1)
stopifnot(!c("Organism", "Experiment") \%k\% vaas_1)
stopifnot(character() \%k\% vaas_1)
}
\keyword{attribute}

\docType{methods}
\alias{infix-k-methods}
\alias{infix-k,character,WMD-method}
\alias{\%k\%,character,WMD-method}
\usage{
  \S4method{\%k\%}{character,WMD}(x, table) 
}
