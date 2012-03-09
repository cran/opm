\name{\%q\%}
\alias{\%q\%}
\alias{infix.q}
\title{Query metadata (non-exact version)}
\arguments{
  \item{x}{Character vector or list used as query.  If a
  character vector, its \code{names} are used to select
  elements from the top level of the metadata. These
  elements are then converted to \sQuote{character} mode
  before comparison with the values of \code{x}. A
  non-empty vector without a \code{names} attribute is
  accepted but will always yield \code{FALSE}. In contrast,
  an entirely empty vector yields \code{TRUE}.  If a list,
  the comparison is applied recursively using
  \code{\link{contains}} with the \code{values} argument
  set to \code{TRUE} but \code{exact} set to \code{FALSE}.
  The main advantage of using a list over the
  character-based search is that it allows one a nested
  query.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Using a character vector, test whether all given query
  keys are present in the top-level names of the metadata
  and refer to the same query elements. Using a list,
  conduct a non-exact query with a query list.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' with the
# values 'Escherichia coli' and 'First replicate':
data(vaas_1)

# Character method
stopifnot(!"Experiment" \%q\% vaas_1) # wrong query here; compare to \%k\%
stopifnot(!"First replicate" \%q\% vaas_1) # again wrong query
stopifnot(c(Experiment = "First replicate") \%q\% vaas_1) # right query

stopifnot(!"Species" \%q\% vaas_1)
stopifnot(!"Escherichia coli" \%q\% vaas_1)
stopifnot(c(Species = "Escherichia coli") \%q\% vaas_1)

stopifnot(c(Species = "Escherichia coli",
  Experiment = "First replicate") \%q\% vaas_1) # Combined query

stopifnot(character() \%q\% vaas_1) # Empty query

# List method
stopifnot(list(Experiment = "First replicate") \%q\% vaas_1)

# Choice among alternatives
stopifnot(list(Experiment = c("First replicate",
  "Second replicate")) \%q\% vaas_1)
stopifnot(!list(Experiment = c("Second replicate",
  "Third replicate")) \%q\% vaas_1)

# Combined query together with choice among alternatives
stopifnot(list(Experiment = c("First replicate", "Second replicate"),
  Species = c("Escherichia coli", "Bacillus subtilis")) \%q\% vaas_1)

stopifnot(list() \%q\% vaas_1) # Empty query
}
\keyword{attribute}

\docType{methods}
\alias{infix.q-methods}
\alias{infix.q,character,WMD-method}
\alias{\%q\%,character,WMD-method}
\alias{infix.q,list,WMD-method}
\alias{\%q\%,list,WMD-method}
\usage{
  \S4method{\%q\%}{character,WMD}(x, table) 

  \S4method{\%q\%}{list,WMD}(x, table) 

}
