\name{\%k\%,list}
\alias{\%k\%,list}
\alias{infix-k,list}
\title{Search in metadata keys (list version)}
\arguments{
  \item{x}{List.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a list as query, this method tests whether all
  given keys are present in the names of the metadata. This
  works like \code{\link{infix-k}}, but because a query
  list is given, the comparison of keys can be applied
  recursively (by using, of course, a nested query list).
  This is based on \code{\link{contains}} with the
  \code{values} argument set to \code{FALSE}.
}
\note{
  There is also a character-based version,
  \code{\link{infix-k}}.
}
\examples{
data(vaas_1)

# The dataset contains the metadata keys 'Species' and 'Experiment' but
# neither 'Trial' nor 'Organism' nor 'Run':
stopifnot(list(Experiment = "whatever") \%k\% vaas_1)
stopifnot(list(Species = "ignored") \%k\% vaas_1)

# This fails because we query with a named sublist but 'Species' is not
# even a list
stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) \%k\% vaas_1)

# This is OK because we query with an unnamed sublist: it has no names that
# one would fail to find
stopifnot(list(Species = list("X", "Y")) \%k\% vaas_1)

# More non-nested query examples
stopifnot(!list(Run = 99) \%k\% vaas_1)
stopifnot(list(Species = "?", Experiment = NA) \%k\% vaas_1)
stopifnot(!list(Species = "?", Trial = NA) \%k\% vaas_1)
stopifnot(!list(Organism = "?", Experiment = NA) \%k\% vaas_1)
stopifnot(list() \%k\% vaas_1)
}
\keyword{attribute}

\docType{methods}
\alias{infix-k,list,WMD-method}
\alias{\%k\%,list,WMD-method}
\usage{
  \S4method{\%k\%}{list,WMD}(x, table) 
}
