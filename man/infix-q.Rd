\name{\%q\%}
\alias{\%q\%}
\alias{infix-q}
\title{Query metadata (non-exact version)}
\arguments{
  \item{x}{Character vector. Its \code{names} are used to
  select elements from the top level of the metadata. These
  elements are then converted to \sQuote{character} mode
  before comparison with the values of \code{x}. A
  non-empty vector without a \code{names} attribute is
  accepted but will always yield \code{FALSE}. In contrast,
  an entirely empty vector yields \code{TRUE}.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a character vector, test whether all given query
  keys are present in the top-level names of the metadata
  and refer to the same query elements.
}
\note{
  There is also a list-based version,
  \code{\link{infix-q,list}}.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' with the
# values 'Escherichia coli' and 'First replicate':
data(vaas_1)

stopifnot(!"Experiment" \%q\% vaas_1) # wrong query here; compare to \%k\%
stopifnot(!"First replicate" \%q\% vaas_1) # again wrong query
stopifnot(c(Experiment = "First replicate") \%q\% vaas_1) # right query

stopifnot(!"Species" \%q\% vaas_1)
stopifnot(!"Escherichia coli" \%q\% vaas_1)
stopifnot(c(Species = "Escherichia coli") \%q\% vaas_1)

stopifnot(c(Species = "Escherichia coli",
  Experiment = "First replicate") \%q\% vaas_1) # Combined query

stopifnot(character() \%q\% vaas_1) # Empty query
}
\keyword{attribute}

\docType{methods}
\alias{infix-q-methods}
\alias{infix-q,character,WMD-method}
\alias{\%q\%,character,WMD-method}
\usage{
  \S4method{\%q\%}{character,WMD}(x, table) 
}
