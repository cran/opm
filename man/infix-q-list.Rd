\name{\%q\%,list}
\alias{\%q\%,list}
\alias{infix-q,list}
\title{Query metadata (non-exact list version)}
\arguments{
  \item{x}{A list, used as query. The comparison is applied
  recursively using \code{\link{contains}} with the
  \code{values} argument set to \code{TRUE} but
  \code{exact} set to \code{FALSE}. The main advantage of
  using a list over the character-based search is that it
  allows one a nested query.}

  \item{table}{\code{\link{WMD}} object.}
}
\value{
  Logical scalar.
}
\description{
  Conduct a non-exact query with a query list.
}
\note{
  There is also a character-based version,
  \code{\link{infix-k}}.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' with the
# values 'Escherichia coli' and 'First replicate':
data(vaas_1)

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
\alias{infix-q,list,WMD-method}
\alias{\%q\%,list,WMD-method}
\usage{
  \S4method{\%q\%}{list,WMD}(x, table) 
}
