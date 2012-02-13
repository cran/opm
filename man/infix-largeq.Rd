\name{\%Q\%}
\alias{\%Q\%}
\alias{infix-largeq}
\title{Query metadata (strict version)}
\arguments{
  \item{x}{A character vector used as query. The result is
  identical to the one of \code{\link{infix-q}} except for
  the fact that metadata elements are not coerced to
  \sQuote{character} mode, making the query more strict.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a character vector as query, test whether all given
  query keys are present in the top-level names of the
  metadata and refer to the same query elements (without
  coercion to character).
}
\note{
  There is also a list-based version,
  \code{\link{infix-largeq,list}}.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' with the
# values 'Escherichia coli' and 'First replicate':
data(vaas_1)

stopifnot(c(Experiment = "First replicate") \%Q\% vaas_1)

# This does not work because the value has the wrong type
stopifnot(!c(`Plate number` = "6") \%Q\% vaas_1)
# Compare to \%q\%
stopifnot(c(`Plate number` = "6") \%q\% vaas_1)

# Combined query
stopifnot(c(Species = "Escherichia coli",
  Experiment = "First replicate") \%Q\% vaas_1)

stopifnot(character() \%Q\% vaas_1) # Empty query
}
\keyword{attribute}

\docType{methods}
\alias{infix-largeq-methods}
\alias{infix-largeq,character,WMD-method}
\alias{\%Q\%,character,WMD-method}
\usage{
  \S4method{\%Q\%}{character,WMD}(x, table) 
}
