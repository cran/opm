\name{\%q\%}
\alias{\%q\%}
\alias{infix.q}
\title{Query metadata (non-exact version)}
\arguments{
  \item{x}{Character vector, factor, list or formula used
  as query. See \sQuote{Details}.}

  \item{table}{\code{\link{WMD}} or \code{\link{OPMS}}
  object.}
}
\value{
  Logical vector of the length of the \code{\link{WMD}} or
  \code{\link{OPMS}} object.
}
\description{
  Search for the presence of metadata values for given
  keys, either using a vector, factor, list or formula.
  Depending on the arguments, the behaviour differs from
  \code{\link{infix.largeq}}.
}
\details{
  The behaviour of these methods depends on the object used
  as query. \itemize{ \item Using a character vector as
  query, this tests whether all given query keys are
  present in the top-level names of the metadata and refer
  to the same query elements. The \code{names} of the
  vector are used to select elements from the top level of
  the metadata. These elements are then converted to
  \sQuote{character} mode before comparison with the values
  of \code{x}. A non-empty vector without a \code{names}
  attribute is accepted but will always yield \code{FALSE}.
  In contrast, an entirely empty vector yields \code{TRUE}.
  \item Using a list, a non-exact query with a query list
  is conducted.  The comparison is applied recursively
  using \code{\link{contains}} with the \code{values}
  argument set to \code{TRUE} but \code{exact} set to
  \code{FALSE}. The main advantage of using a list over the
  character-based search is that it allows for a nested
  query.  \item The factor method first converts \code{x}
  to \sQuote{character} mode.  \item The formula method
  attempts to evaluate the right side of the formula in the
  context of the metadata of \code{table} and returns the
  result. For the \code{\link{WMD}} method, is is up to the
  user to ensure that the result is a logical scalar, but
  the method would succeed anyway. The \code{\link{OPMS}}
  yields an error unless each plate yields a logical
  scalar. Symbols that are not found within the metadata
  are looked up in the enclosing environment. This is less
  strict than \code{\link{infix.largeq}}.  Because of
  missing objects and other reasons the method might
  nevertheless fail. } See \code{\link{subset}} for usage
  examples with \code{\link{OPMS}} objects.
}
\note{
  The two arguments can swap their places.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' with the
# values 'Escherichia coli' and 'First replicate':
data(vaas_1)

# Character method
stopifnot(!"Experiment" \%q\% vaas_1) # wrong query here; compare to \%k\%
stopifnot(!"First replicate" \%q\% vaas_1) # again wrong query
stopifnot(c(Experiment = "First replicate") \%q\% vaas_1) # correct query

stopifnot(!"Species" \%q\% vaas_1) # wrong query
stopifnot(!"Escherichia coli" \%q\% vaas_1) # wrong query
stopifnot(c(Species = "Escherichia coli") \%q\% vaas_1) # correct query

stopifnot(c(Species = "Escherichia coli",
  Experiment = "First replicate") \%q\% vaas_1) # combined query, all TRUE

stopifnot(character() \%q\% vaas_1) # empty query always results

# List method
stopifnot(list(Experiment = "First replicate") \%q\% vaas_1)

# Choice among alternatives
stopifnot(list(Experiment = c("First replicate",
  "Second replicate")) \%q\% vaas_1) # one of them TRUE
stopifnot(!list(Experiment = c("Second replicate",
  "Third replicate")) \%q\% vaas_1) # none of them TRUE

# Combined query together with choice among alternatives
stopifnot(list(Experiment = c("First replicate", "Second replicate"),
  Species = c("Escherichia coli", "Bacillus subtilis")) \%q\% vaas_1)

stopifnot(list() \%q\% vaas_1) # empty query

# Formulas for querying, compare with \%Q\%
stopifnot((~ Experiment == "First replicate") \%q\% vaas_1)
stopifnot(vaas_1 \%q\% ~ Experiment == "First replicate")
stopifnot(vaas_1 \%q\% ~ Species == "Escherichia coli")
stopifnot(vaas_1 \%q\% ~ Species != "Bacillus subtilis")
x <- try(vaas_1 \%q\% ~ missing.name == "abc", silent = TRUE) # fails
stopifnot(inherits(x, "try-error"))
missing.name <- "abc"  # enclosing environment considered
stopifnot(vaas_1 \%q\% ~ missing.name == "abc")
rm(missing.name) # tidy up

# examples for OPMS methods are given under subset()
}
\keyword{attribute}

\docType{methods}
\alias{infix.q-methods}
\alias{infix.q,character,WMD-method}
\alias{\%q\%,character,WMD-method}
\alias{infix.q,list,WMD-method}
\alias{\%q\%,list,WMD-method}
\alias{infix.q,formula,WMD-method}
\alias{\%q\%,formula,WMD-method}
\alias{infix.q,factor,WMD-method}
\alias{\%q\%,factor,WMD-method}
\alias{infix.q,list,OPMS-method}
\alias{\%q\%,list,OPMS-method}
\alias{infix.q,character,OPMS-method}
\alias{\%q\%,character,OPMS-method}
\alias{infix.q,factor,OPMS-method}
\alias{\%q\%,factor,OPMS-method}
\alias{infix.q,formula,OPMS-method}
\alias{\%q\%,formula,OPMS-method}
\alias{infix.q,WMD,ANY-method}
\alias{\%q\%,WMD,ANY-method}
\alias{infix.q,OPMS,ANY-method}
\alias{\%q\%,OPMS,ANY-method}
\usage{
  \S4method{\%q\%}{character,WMD}(x, table) 

  \S4method{\%q\%}{list,WMD}(x, table) 

  \S4method{\%q\%}{formula,WMD}(x, table) 

  \S4method{\%q\%}{factor,WMD}(x, table) 

  \S4method{\%q\%}{list,OPMS}(x, table) 

  \S4method{\%q\%}{character,OPMS}(x, table) 

  \S4method{\%q\%}{factor,OPMS}(x, table) 

  \S4method{\%q\%}{formula,OPMS}(x, table) 

  \S4method{\%q\%}{WMD,ANY}(x, table) 

  \S4method{\%q\%}{OPMS,ANY}(x, table) 

}
