\name{\%Q\%,list}
\alias{\%Q\%,list}
\alias{infix-largeq,list}
\title{Query metadata (strict list version)}
\arguments{
  \item{x}{A list used as query. The comparison is applied
  recursively using \code{\link{contains}} with the
  arguments \code{values} and \code{exact} set to
  \code{TRUE}. This might be too strict for most
  applications. The main advantage of using a list over the
  character-based search is that it allows one a nested
  query.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Conduct an exact query with a query list.
}
\note{
  There is also a character-based version,
  \code{\link{infix-largeq}}.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' with the
# values 'Escherichia coli' and 'First replicate':
data(vaas_1)

stopifnot(list(Experiment = "First replicate") \%Q\% vaas_1)

# Choice among alternatives is not done here: this query fails unless this
# two-element vector is contained. Compare to \%q\%.
stopifnot(!list(Experiment = c("First replicate",
  "Second replicate")) \%Q\% vaas_1)

stopifnot(list() \%Q\% vaas_1) # Empty query
}
\keyword{attribute}

\docType{methods}
\alias{infix-largeq,list,WMD-method}
\alias{\%Q\%,list,WMD-method}
\usage{
  \S4method{\%Q\%}{list,WMD}(x, table) 
}
