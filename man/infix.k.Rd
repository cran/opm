\name{\%k\%}
\alias{\%K\%}
\alias{\%k\%}
\alias{infix.k}
\alias{infix.largek}
\title{Query metadata keys}
\arguments{
  \item{x}{Character vector, factor, list, formula,
  expression or \code{\link{WMD}} object used as query. See
  \sQuote{Details}. \code{x} and \code{table} can swap
  their places.}

  \item{table}{\code{\link{WMD}} or \code{\link{OPMS}}
  object. \code{x} and \code{table} can swap their places.}
}
\value{
  Logical vector of the length of the \code{\link{WMD}} or
  \code{\link{OPMS}} object.
}
\description{
  Search for the presence of metadata keys, either using a
  vector, factor, list, formula, expression or
  \code{\link{WMD}} object.
}
\details{
  The behaviour of these methods depends on the object used
  as query.  \code{infix.largek} is usually stricter than
  \code{infix.k}, sometimes equivalent. \itemize{ \item
  Using a character vector as query, \code{infix.k} tests
  whether all given keys are present in the top-level names
  of the metadata (these may be nested, but all contained
  lists are ignored here). An empty query vector results in
  \code{TRUE}. Note that the values of the character
  vector, not its names, if any, are used for querying the
  metadata. In contrast, \code{infix.largek} tests whether
  a given key is present in the metadata and fetches an
  object that is not \code{NULL}. If the key has a length >
  1, contained lists are queried.

  \item Using a list as query, both methods tests whether
  all given keys are present in the names of the metadata.
  This works like the character method, but because a query
  list is given, the comparison of keys can be applied
  recursively (by using, of course, a nested query list).
  This is based on \code{\link{contains}} with the
  \code{values} argument set to \code{FALSE}.

  \item When supplying a \code{\link{WMD}} object as query,
  its metadata will be used in a list query.

  \item The factor method first converts \code{x} to
  \sQuote{character} mode.

  \item The formula method attempts to evaluate the right
  side of the formula in the context of the metadata of
  \code{table} and returns whether or not this fails
  (yields an error). Symbols that are not found within the
  metadata are looked up in the enclosing environment
  \code{infix.k} or only in the base environment
  \code{infix.largek}. But note also that missing objects
  are not the only potential reason of failure.

  \item The expression method works like the formula
  method, using the entire expression in place of the right
  side of the formula. } See \code{\link{subset}} for usage
  examples with \code{\link{OPMS}} objects.
}
\examples{
# The data set contains the metadata keys 'Species' and 'Experiment' but
# neither 'Trial' nor 'Organism' nor 'Run':
# In the following we use stopifnot(), which fails unless all arguments
# passed are TRUE.

## Character methods

# Zero-element queries
stopifnot(character() \%k\% vaas_1) # always results
stopifnot(character() \%K\% vaas_1)

# Single-element queries
stopifnot("Experiment" \%k\% vaas_1) # present
stopifnot("Experiment" \%K\% vaas_1) # present
stopifnot("Species" \%k\% vaas_1) # present
stopifnot("Species" \%K\% vaas_1)  # present
stopifnot(!"Run" \%k\% vaas_1) # missing
stopifnot(!"Run" \%K\% vaas_1) # missing
stopifnot(!"Organism" \%k\% vaas_1) # missing
stopifnot(!"Trial" \%K\% vaas_1) # missing

# Multi-element queries
stopifnot(!c("Species", "Trial") \%k\% vaas_1) # only one present
stopifnot(!c("Organism", "Experiment") \%k\% vaas_1) # only one present
stopifnot(c("Species", "Experiment") \%k\% vaas_1) # all present
# querying with \%K\% and vectors of length > 1 mean nested queries; compare
# this to the behaviour of \%k\%!
stopifnot(!c("Species", "Experiment") \%K\% vaas_1)
# i.e. "Experiment" is not within "Species".

## List methods

stopifnot(list(Experiment = "whatever") \%k\% vaas_1) # key present
stopifnot(list(Species = "ignored") \%k\% vaas_1) # key present

stopifnot(vaas_1 \%k\% vaas_1) # obviously
stopifnot(vaas_1 \%K\% vaas_1)

# This fails because we query with a named 2nd-order list but the 'Species'
# metadata entry is not even a list.
stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) \%k\% vaas_1)

# This is OK because we query with an unnamed 2nd-order list: it has no
# names that one would fail to find.
stopifnot(list(Species = list("X", "Y")) \%k\% vaas_1)

# More non-nested query examples
stopifnot(!list(Run = 99) \%k\% vaas_1) # key not present
stopifnot(list(Species = "?", Experiment = NA) \%k\% vaas_1) # keys present
stopifnot(!list(Species = "?", Trial = NA) \%k\% vaas_1) # one key missing
stopifnot(!list(Organism = "?", Experiment = NA) \%k\% vaas_1) # likewise
stopifnot(list() \%k\% vaas_1) # empty query always results

# Formulae for querying, compare with list examples above
stopifnot((~ Experiment) \%k\% vaas_1) # key present
stopifnot((~ Experiment) \%K\% vaas_1)
stopifnot(vaas_1 \%k\% ~ Experiment) # key present, no parens needed
stopifnot(vaas_1 \%K\% ~ Experiment)
stopifnot(vaas_1 \%k\% ~ Species) # key present, no parens needed
stopifnot(vaas_1 \%K\% ~ Species)
stopifnot(!vaas_1 \%k\% ~ Species$Epithet) # nested key not present
stopifnot(!vaas_1 \%K\% ~ Species$Epithet)
stopifnot(!vaas_1 \%k\% ~ missing.name) # key not present
stopifnot(!vaas_1 \%K\% ~ missing.name)
missing.name <- "abc"
stopifnot(vaas_1 \%k\% ~ missing.name) # key found in enclosing environment
stopifnot(!vaas_1 \%K\% ~ missing.name) # enclosing environment ignored
rm(missing.name) # tidy up
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{infix.largek-methods}
\alias{infix.largek,OPMS,ANY-method}
\alias{\%K\%,OPMS,ANY-method}
\alias{infix.largek,WMD,ANY-method}
\alias{\%K\%,WMD,ANY-method}
\alias{infix.largek,WMD,OPMS-method}
\alias{\%K\%,WMD,OPMS-method}
\alias{infix.largek,WMD,WMD-method}
\alias{\%K\%,WMD,WMD-method}
\alias{infix.largek,character,OPMS-method}
\alias{\%K\%,character,OPMS-method}
\alias{infix.largek,character,WMD-method}
\alias{\%K\%,character,WMD-method}
\alias{infix.largek,expression,OPMS-method}
\alias{\%K\%,expression,OPMS-method}
\alias{infix.largek,expression,WMD-method}
\alias{\%K\%,expression,WMD-method}
\alias{infix.largek,factor,OPMS-method}
\alias{\%K\%,factor,OPMS-method}
\alias{infix.largek,factor,WMD-method}
\alias{\%K\%,factor,WMD-method}
\alias{infix.largek,formula,OPMS-method}
\alias{\%K\%,formula,OPMS-method}
\alias{infix.largek,formula,WMD-method}
\alias{\%K\%,formula,WMD-method}
\alias{infix.largek,list,OPMS-method}
\alias{\%K\%,list,OPMS-method}
\alias{infix.largek,list,WMD-method}
\alias{\%K\%,list,WMD-method}
\alias{infix.k-methods}
\alias{infix.k,OPMS,ANY-method}
\alias{\%k\%,OPMS,ANY-method}
\alias{infix.k,WMD,ANY-method}
\alias{\%k\%,WMD,ANY-method}
\alias{infix.k,WMD,OPMS-method}
\alias{\%k\%,WMD,OPMS-method}
\alias{infix.k,WMD,WMD-method}
\alias{\%k\%,WMD,WMD-method}
\alias{infix.k,character,OPMS-method}
\alias{\%k\%,character,OPMS-method}
\alias{infix.k,character,WMD-method}
\alias{\%k\%,character,WMD-method}
\alias{infix.k,expression,OPMS-method}
\alias{\%k\%,expression,OPMS-method}
\alias{infix.k,expression,WMD-method}
\alias{\%k\%,expression,WMD-method}
\alias{infix.k,factor,OPMS-method}
\alias{\%k\%,factor,OPMS-method}
\alias{infix.k,factor,WMD-method}
\alias{\%k\%,factor,WMD-method}
\alias{infix.k,formula,OPMS-method}
\alias{\%k\%,formula,OPMS-method}
\alias{infix.k,formula,WMD-method}
\alias{\%k\%,formula,WMD-method}
\alias{infix.k,list,OPMS-method}
\alias{\%k\%,list,OPMS-method}
\alias{infix.k,list,WMD-method}
\alias{\%k\%,list,WMD-method}
\usage{
  \S4method{\%K\%}{OPMS,ANY}(x, table) 
  \S4method{\%K\%}{WMD,ANY}(x, table) 
  \S4method{\%K\%}{WMD,OPMS}(x, table) 
  \S4method{\%K\%}{WMD,WMD}(x, table) 
  \S4method{\%K\%}{character,OPMS}(x, table) 
  \S4method{\%K\%}{character,WMD}(x, table) 
  \S4method{\%K\%}{expression,OPMS}(x, table) 
  \S4method{\%K\%}{expression,WMD}(x, table) 
  \S4method{\%K\%}{factor,OPMS}(x, table) 
  \S4method{\%K\%}{factor,WMD}(x, table) 
  \S4method{\%K\%}{formula,OPMS}(x, table) 
  \S4method{\%K\%}{formula,WMD}(x, table) 
  \S4method{\%K\%}{list,OPMS}(x, table) 
  \S4method{\%K\%}{list,WMD}(x, table) 

  \S4method{\%k\%}{OPMS,ANY}(x, table) 
  \S4method{\%k\%}{WMD,ANY}(x, table) 
  \S4method{\%k\%}{WMD,OPMS}(x, table) 
  \S4method{\%k\%}{WMD,WMD}(x, table) 
  \S4method{\%k\%}{character,OPMS}(x, table) 
  \S4method{\%k\%}{character,WMD}(x, table) 
  \S4method{\%k\%}{expression,OPMS}(x, table) 
  \S4method{\%k\%}{expression,WMD}(x, table) 
  \S4method{\%k\%}{factor,OPMS}(x, table) 
  \S4method{\%k\%}{factor,WMD}(x, table) 
  \S4method{\%k\%}{formula,OPMS}(x, table) 
  \S4method{\%k\%}{formula,WMD}(x, table) 
  \S4method{\%k\%}{list,OPMS}(x, table) 
  \S4method{\%k\%}{list,WMD}(x, table) 
}
%% END INSERTION BY repair_S4_docu.rb
