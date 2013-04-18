\name{\%Q\%}
\alias{\%Q\%}
\alias{infix.largeq}
\title{Query metadata (strict version)}
\arguments{
  \item{x}{Character vector, factor, list for formula used
  as query. See \sQuote{Details}.}

  \item{table}{\code{\link{WMD}} or \code{\link{OPMS}}
  object.}
}
\value{
  Logical scalar.
}
\description{
  Search for the presence of metadata values for given
  keys, either using a vector, factor, list or formula.
  Depending on the arguments, the behaviour differs from
  \code{\link{infix.q}}.
}
\details{
  The behaviour of these methods depends on the object used
  as query. \itemize{ \item Using a character vector as
  query, this tests whether all given query keys are
  present in the top-level names of the metadata and refer
  to the same query elements (without coercion to
  character). The result is identical to the one of
  \code{\link{infix.q}} except for the fact that metadata
  elements are not coerced to \sQuote{character} mode,
  making the query more strict.  \item Using a list, this
  conducts an exact query with this query list. The
  comparison is applied recursively using
  \code{\link{contains}} with the arguments \code{values}
  and \code{exact} set to \code{TRUE}. This might be too
  strict for most applications. The main advantage of using
  a list over the character-based search is that it allows
  one a nested query.  \item The factor method first
  converts \code{x} to \sQuote{character} mode.  \item The
  formula method attempts to evaluate the right side of the
  formula in the context of the metadata of \code{table}
  and returns the result. For the \code{\link{WMD}} method,
  is is up to the user to ensure that the result is a
  logical scalar, but the method would succeed anyway. The
  \code{\link{OPMS}} yields an error unless each plate
  yield a logical scalar. Symbols that are not found within
  the metadata are looked up in the only in the base
  environment. This is stricter than \code{\link{infix.q}}.
  Because of missing objects and other reasons the method
  might fail. } See \code{\link{subset}} for usage examples
  with \code{\link{OPMS}} objects.
}
\note{
  The two arguments can swap their places.
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
  Experiment = "First replicate") \%Q\% vaas_1) # all present

stopifnot(character() \%Q\% vaas_1) # empty query always results

# List method
stopifnot(list(Experiment = "First replicate") \%Q\% vaas_1) # present

# Choice among alternatives is not done here: this query fails unless this
# two-element vector is contained. Compare to \%q\%.
stopifnot(!list(Experiment = c("First replicate",
  "Second replicate")) \%Q\% vaas_1)

stopifnot(list() \%Q\% vaas_1) # empty query always result

# Formulas for querying, compare with \%q\%
stopifnot((~ Experiment == "First replicate") \%Q\% vaas_1)
stopifnot(vaas_1 \%Q\% ~ Experiment == "First replicate")
stopifnot(vaas_1 \%Q\% ~ Species == "Escherichia coli")
stopifnot(vaas_1 \%Q\% ~ Species != "Bacillus subtilis")
x <- try(vaas_1 \%Q\% ~ missing.name == "abc", silent = TRUE) # fails
stopifnot(inherits(x, "try-error"))
missing.name <- "abc" # enclosing environment will be ignored
x <- try(vaas_1 \%Q\% ~ missing.name == "abc", silent = TRUE) # still fails
stopifnot(inherits(x, "try-error"))
rm(missing.name) # tidy up
}
\keyword{attribute}

\docType{methods}
\alias{infix.largeq-methods}
\alias{infix.largeq,character,WMD-method}
\alias{\%Q\%,character,WMD-method}
\alias{infix.largeq,list,WMD-method}
\alias{\%Q\%,list,WMD-method}
\alias{infix.largeq,formula,WMD-method}
\alias{\%Q\%,formula,WMD-method}
\alias{infix.largeq,factor,WMD-method}
\alias{\%Q\%,factor,WMD-method}
\alias{infix.largeq,list,OPMS-method}
\alias{\%Q\%,list,OPMS-method}
\alias{infix.largeq,character,OPMS-method}
\alias{\%Q\%,character,OPMS-method}
\alias{infix.largeq,factor,OPMS-method}
\alias{\%Q\%,factor,OPMS-method}
\alias{infix.largeq,formula,OPMS-method}
\alias{\%Q\%,formula,OPMS-method}
\alias{infix.largeq,WMD,ANY-method}
\alias{\%Q\%,WMD,ANY-method}
\alias{infix.largeq,OPMS,ANY-method}
\alias{\%Q\%,OPMS,ANY-method}
\usage{
  \S4method{\%Q\%}{character,WMD}(x, table) 

  \S4method{\%Q\%}{list,WMD}(x, table) 

  \S4method{\%Q\%}{formula,WMD}(x, table) 

  \S4method{\%Q\%}{factor,WMD}(x, table) 

  \S4method{\%Q\%}{list,OPMS}(x, table) 

  \S4method{\%Q\%}{character,OPMS}(x, table) 

  \S4method{\%Q\%}{factor,OPMS}(x, table) 

  \S4method{\%Q\%}{formula,OPMS}(x, table) 

  \S4method{\%Q\%}{WMD,ANY}(x, table) 

  \S4method{\%Q\%}{OPMS,ANY}(x, table) 

}
