\name{\%Q\%}
\alias{\%Q\%}
\alias{infix.largeq}
\title{Query metadata (strict version)}
\arguments{
  \item{x}{Character vector or list used as query.  If a
  character vector, the result is identical to the one of
  \code{\link{infix.q}} except for the fact that metadata
  elements are not coerced to \sQuote{character} mode,
  making the query more strict.  If a list, the comparison
  is applied recursively using \code{\link{contains}} with
  the arguments \code{values} and \code{exact} set to
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
  Using a character vector as query, test whether all given
  query keys are present in the top-level names of the
  metadata and refer to the same query elements (without
  coercion to character). Using a list, conduct an exact
  query with this query list.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' with the
# values 'Escherichia coli' and 'First replicate':
data(vaas_1)

# Character method
stopifnot(c(Experiment = "First replicate") \%Q\% vaas_1)

# This does not work because the value has the wrong type
stopifnot(!c(`Plate number` = "6") \%Q\% vaas_1)
# Compare to \%q\%
stopifnot(c(`Plate number` = "6") \%q\% vaas_1)

# Combined query
stopifnot(c(Species = "Escherichia coli",
  Experiment = "First replicate") \%Q\% vaas_1)

stopifnot(character() \%Q\% vaas_1) # Empty query

# List method
stopifnot(list(Experiment = "First replicate") \%Q\% vaas_1)

# Choice among alternatives is not done here: this query fails unless this
# two-element vector is contained. Compare to \%q\%.
stopifnot(!list(Experiment = c("First replicate",
  "Second replicate")) \%Q\% vaas_1)

stopifnot(list() \%Q\% vaas_1) # Empty query
}
\keyword{attribute}

\docType{methods}
\alias{infix.largeq-methods}
\alias{infix.largeq,character,WMD-method}
\alias{\%Q\%,character,WMD-method}
\alias{infix.largeq,list,WMD-method}
\alias{\%Q\%,list,WMD-method}
\usage{
  \S4method{\%Q\%}{character,WMD}(x, table) 

  \S4method{\%Q\%}{list,WMD}(x, table) 

}
