\name{\%q\%}
\alias{\%Q\%}
\alias{\%q\%}
\alias{infix.largeq}
\alias{infix.q}
\title{Query metadata}
\arguments{
  \item{x}{Character vector, factor, list, formula,
  expression or \code{\link{WMD}} object used as query. See
  \sQuote{Details}. \code{x} and \code{table} can swap
  their places.}

  \item{table}{\code{\link{WMD}}, \code{\link{WMDS}} or
  \code{\link{MOPMX}} object. \code{x} and \code{table} can
  swap their places.}
}
\value{
  Logical vector of the length of the \code{\link{WMD}} or
  \code{\link{WMDS}} object. For \code{\link{MOPMX}}
  objects, a list of such vectors.
}
\description{
  Search for the presence of metadata values for given
  keys, either using a vector, factor, list, formula,
  expression or \code{\link{WMD}} object.
}
\details{
  The behaviour of these methods depends on the object used
  as query.  \code{infix.largeq} is usually stricter than
  \code{infix.q}, sometimes equivalent. \itemize{

  \item Using a character vector as query, this tests
  whether all given query keys are present in the top-level
  names of the metadata and refer to the same query
  elements. The \code{names} of the vector are used to
  select elements from the top level of the metadata. When
  using \code{infix.q}, these elements are then converted
  to \sQuote{character} mode before comparison with the
  values of \code{x}. A non-empty vector without a
  \code{names} attribute is accepted but will always yield
  \code{FALSE}. In contrast, an entirely empty vector
  yields \code{TRUE}.

  \item Using a list, a non-exact query with a query list
  is conducted.  The comparison is applied recursively
  using \code{\link{contains}} with the \code{values}
  argument set to \code{TRUE} and \code{exact} set to
  either \code{FALSE} (\code{infix.q}) or \code{TRUE}
  (\code{infix.largeq}). The latter might be too strict for
  most applications. The main advantage of using a list
  over the character-based search is that it allows for a
  nested query.

  \item When supplying a \code{\link{WMD}} object as query,
  its metadata will be used in a list query.

  \item The factor method first converts \code{x} to
  \sQuote{character} mode.

  \item The formula method attempts to evaluate the right
  side of the formula in the context of the metadata of
  \code{table} and returns the result. For the
  \code{\link{WMD}} method, it is up to the user to ensure
  that the result is a logical scalar, but the method would
  succeed anyway. The \code{\link{WMDS}} method yields an
  error unless each plate yields a logical scalar. Symbols
  that are not found within the metadata are looked up in
  the enclosing environment (\code{infix.q}) or only in the
  base environment (\code{infix.largeq}). The former
  approach is less strict.  Because of missing objects and
  other reasons the method might nevertheless fail.

  \item The expression method works like the formula
  method, using the entire expression in place of the right
  side of the formula. } See \code{\link{subset}} for usage
  examples with \code{\link{OPMS}} objects.
}
\examples{
# The data set vaas_1 contains the metadata keys 'Species' and 'Experiment'
# with the values 'Escherichia coli' and 'First replicate'.

## Character methods

stopifnot(!"Experiment" \%q\% vaas_1) # wrong query here; compare to \%k\%
stopifnot(!"First replicate" \%q\% vaas_1) # again wrong query
stopifnot(c(Experiment = "First replicate") \%q\% vaas_1) # correct query
stopifnot(c(Experiment = "First replicate") \%Q\% vaas_1)

stopifnot(!"Species" \%q\% vaas_1) # wrong query
stopifnot(!"Escherichia coli" \%q\% vaas_1) # wrong query
stopifnot(c(Species = "Escherichia coli") \%q\% vaas_1) # correct query

# This does not work because the value has the wrong type
stopifnot(!c(`Plate number` = "6") \%Q\% vaas_1)
# Compare to \%q\%
stopifnot(c(`Plate number` = "6") \%q\% vaas_1)

stopifnot(c(Species = "Escherichia coli",
  Experiment = "First replicate") \%q\% vaas_1) # combined query, all TRUE
stopifnot(c(Species = "Escherichia coli",
  Experiment = "First replicate") \%Q\% vaas_1) # all present

stopifnot(character() \%q\% vaas_1) # empty query always results
stopifnot(character() \%Q\% vaas_1)

## List methods

stopifnot(list(Experiment = "First replicate") \%q\% vaas_1)
stopifnot(list(Experiment = "First replicate") \%Q\% vaas_1) # present

# Choice among alternatives
stopifnot(list(Experiment = c("First replicate",
  "Second replicate")) \%q\% vaas_1) # one of them TRUE
stopifnot(!list(Experiment = c("Second replicate",
  "Third replicate")) \%q\% vaas_1) # none of them TRUE

# Combined query together with choice among alternatives
stopifnot(list(Experiment = c("First replicate", "Second replicate"),
  Species = c("Escherichia coli", "Bacillus subtilis")) \%q\% vaas_1)

# Choice among alternatives is not done here: this query fails unless this
# two-element vector is contained. Compare to \%q\%.
stopifnot(!list(Experiment = c("First replicate",
  "Second replicate")) \%Q\% vaas_1)

stopifnot(list() \%q\% vaas_1) # empty query always results
stopifnot(list() \%Q\% vaas_1)

stopifnot(vaas_1 \%q\% vaas_1) # obviously
stopifnot(vaas_1 \%Q\% vaas_1)

## Formulae for querying

stopifnot((~ Experiment == "First replicate") \%q\% vaas_1)
stopifnot((~ Experiment == "First replicate") \%Q\% vaas_1)
stopifnot(vaas_1 \%q\% ~ Experiment == "First replicate")
stopifnot(vaas_1 \%Q\% ~ Experiment == "First replicate")
stopifnot(vaas_1 \%q\% ~ Species == "Escherichia coli")
stopifnot(vaas_1 \%Q\% ~ Species == "Escherichia coli")
stopifnot(vaas_1 \%q\% ~ Species != "Bacillus subtilis")
stopifnot(vaas_1 \%Q\% ~ Species != "Bacillus subtilis")

x <- try(vaas_1 \%q\% ~ missing.name == "abc", silent = TRUE) # fails
stopifnot(inherits(x, "try-error"))
x <- try(vaas_1 \%Q\% ~ missing.name == "abc", silent = TRUE) # also fails
stopifnot(inherits(x, "try-error"))
missing.name <- "abc"  # enclosing environment considered or ignored
stopifnot(vaas_1 \%q\% ~ missing.name == "abc")
x <- try(vaas_1 \%Q\% ~ missing.name == "abc", silent = TRUE) # still fails
stopifnot(inherits(x, "try-error"))
rm(missing.name) # tidy up

# examples for OPMS methods are given under subset()
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{infix.largeq-methods}
\alias{infix.largeq,ANY,MOPMX-method}
\alias{\%Q\%,ANY,MOPMX-method}
\alias{infix.largeq,MOPMX,ANY-method}
\alias{infix.largeq,MOPMX-method}
\alias{\%Q\%,MOPMX,ANY-method}
\alias{\%Q\%,MOPMX-method}
\alias{infix.largeq,WMD,ANY-method}
\alias{infix.largeq,WMD-method}
\alias{\%Q\%,WMD,ANY-method}
\alias{\%Q\%,WMD-method}
\alias{infix.largeq,WMD,WMD-method}
\alias{\%Q\%,WMD,WMD-method}
\alias{infix.largeq,WMD,WMDS-method}
\alias{\%Q\%,WMD,WMDS-method}
\alias{infix.largeq,WMDS,ANY-method}
\alias{infix.largeq,WMDS-method}
\alias{\%Q\%,WMDS,ANY-method}
\alias{\%Q\%,WMDS-method}
\alias{infix.largeq,character,WMD-method}
\alias{\%Q\%,character,WMD-method}
\alias{infix.largeq,character,WMDS-method}
\alias{\%Q\%,character,WMDS-method}
\alias{infix.largeq,expression,WMD-method}
\alias{\%Q\%,expression,WMD-method}
\alias{infix.largeq,expression,WMDS-method}
\alias{\%Q\%,expression,WMDS-method}
\alias{infix.largeq,factor,WMD-method}
\alias{\%Q\%,factor,WMD-method}
\alias{infix.largeq,factor,WMDS-method}
\alias{\%Q\%,factor,WMDS-method}
\alias{infix.largeq,formula,WMD-method}
\alias{\%Q\%,formula,WMD-method}
\alias{infix.largeq,formula,WMDS-method}
\alias{\%Q\%,formula,WMDS-method}
\alias{infix.largeq,list,WMD-method}
\alias{\%Q\%,list,WMD-method}
\alias{infix.largeq,list,WMDS-method}
\alias{\%Q\%,list,WMDS-method}
\alias{infix.q-methods}
\alias{infix.q,ANY,MOPMX-method}
\alias{\%q\%,ANY,MOPMX-method}
\alias{infix.q,MOPMX,ANY-method}
\alias{infix.q,MOPMX-method}
\alias{\%q\%,MOPMX,ANY-method}
\alias{\%q\%,MOPMX-method}
\alias{infix.q,WMD,ANY-method}
\alias{infix.q,WMD-method}
\alias{\%q\%,WMD,ANY-method}
\alias{\%q\%,WMD-method}
\alias{infix.q,WMD,WMD-method}
\alias{\%q\%,WMD,WMD-method}
\alias{infix.q,WMD,WMDS-method}
\alias{\%q\%,WMD,WMDS-method}
\alias{infix.q,WMDS,ANY-method}
\alias{infix.q,WMDS-method}
\alias{\%q\%,WMDS,ANY-method}
\alias{\%q\%,WMDS-method}
\alias{infix.q,character,WMD-method}
\alias{\%q\%,character,WMD-method}
\alias{infix.q,character,WMDS-method}
\alias{\%q\%,character,WMDS-method}
\alias{infix.q,expression,WMD-method}
\alias{\%q\%,expression,WMD-method}
\alias{infix.q,expression,WMDS-method}
\alias{\%q\%,expression,WMDS-method}
\alias{infix.q,factor,WMD-method}
\alias{\%q\%,factor,WMD-method}
\alias{infix.q,factor,WMDS-method}
\alias{\%q\%,factor,WMDS-method}
\alias{infix.q,formula,WMD-method}
\alias{\%q\%,formula,WMD-method}
\alias{infix.q,formula,WMDS-method}
\alias{\%q\%,formula,WMDS-method}
\alias{infix.q,list,WMD-method}
\alias{\%q\%,list,WMD-method}
\alias{infix.q,list,WMDS-method}
\alias{\%q\%,list,WMDS-method}
\usage{
  \S4method{\%Q\%}{ANY,MOPMX}(x, table) 
  \S4method{\%Q\%}{MOPMX,ANY}(x, table) 
  \S4method{\%Q\%}{WMD,ANY}(x, table) 
  \S4method{\%Q\%}{WMD,WMD}(x, table) 
  \S4method{\%Q\%}{WMD,WMDS}(x, table) 
  \S4method{\%Q\%}{WMDS,ANY}(x, table) 
  \S4method{\%Q\%}{character,WMD}(x, table) 
  \S4method{\%Q\%}{character,WMDS}(x, table) 
  \S4method{\%Q\%}{expression,WMD}(x, table) 
  \S4method{\%Q\%}{expression,WMDS}(x, table) 
  \S4method{\%Q\%}{factor,WMD}(x, table) 
  \S4method{\%Q\%}{factor,WMDS}(x, table) 
  \S4method{\%Q\%}{formula,WMD}(x, table) 
  \S4method{\%Q\%}{formula,WMDS}(x, table) 
  \S4method{\%Q\%}{list,WMD}(x, table) 
  \S4method{\%Q\%}{list,WMDS}(x, table) 

  \S4method{\%q\%}{ANY,MOPMX}(x, table) 
  \S4method{\%q\%}{MOPMX,ANY}(x, table) 
  \S4method{\%q\%}{WMD,ANY}(x, table) 
  \S4method{\%q\%}{WMD,WMD}(x, table) 
  \S4method{\%q\%}{WMD,WMDS}(x, table) 
  \S4method{\%q\%}{WMDS,ANY}(x, table) 
  \S4method{\%q\%}{character,WMD}(x, table) 
  \S4method{\%q\%}{character,WMDS}(x, table) 
  \S4method{\%q\%}{expression,WMD}(x, table) 
  \S4method{\%q\%}{expression,WMDS}(x, table) 
  \S4method{\%q\%}{factor,WMD}(x, table) 
  \S4method{\%q\%}{factor,WMDS}(x, table) 
  \S4method{\%q\%}{formula,WMD}(x, table) 
  \S4method{\%q\%}{formula,WMDS}(x, table) 
  \S4method{\%q\%}{list,WMD}(x, table) 
  \S4method{\%q\%}{list,WMDS}(x, table) 
}
%% END INSERTION BY repair_S4_docu.rb
