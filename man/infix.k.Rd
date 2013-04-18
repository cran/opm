\name{\%k\%}
\alias{\%k\%}
\alias{infix.k}
\title{Search in metadata keys}
\arguments{
  \item{x}{Character vector, factor, list or formula. See
  \sQuote{Details}.}

  \item{table}{\code{\link{WMD}} or \code{\link{OPMS}}
  object.}
}
\value{
  Logical vector of the length of the \code{\link{WMD}} or
  \code{\link{OPMS}} object.
}
\description{
  Search for the presence of metadata keys, either using a
  vector, factor, list or formula. Depending on the
  arguments, the behaviour differs from
  \code{\link{infix.largek}}.
}
\details{
  The behaviour of these methods depends on the object used
  as query. \itemize{ \item Using a character vector as
  query, this method tests whether all given keys are
  present in the top-level names of the metadata (these may
  be nested, but all sublists are ignored here). An empty
  query vector results in \code{TRUE}. Note that the values
  of the character vector, not its names, if any, are used
  for querying the metadata.  \item Using a list as query,
  this method tests whether all given keys are present in
  the names of the metadata. This works like the character
  method, but because a query list is given, the comparison
  of keys can be applied recursively (by using, of course,
  a nested query list). This is based on
  \code{\link{contains}} with the \code{values} argument
  set to \code{FALSE}.  \item The factor method first
  converts \code{x} to \sQuote{character} mode.  \item The
  formula method attempts to evaluate the right side of the
  formula in the context of the metadata of \code{table}
  and returns whether or not this fails (yields an error).
  But symbols that are not found within the metadata are
  looked up in the enclosing environment. Note also that
  missing objects are not the only potential reason of
  failure. } See \code{\link{subset}} for usage examples
  with \code{\link{OPMS}} objects.
}
\note{
  The two arguments can swap their places.
}
\examples{
# The dataset contains the metadata keys 'Species' and 'Experiment' but
# neither 'Trial' nor 'Organism' nor 'Run':
data(vaas_1)
# In the following we use stopifnot(), which fails unless all arguments
# passed are TRUE.

# Character method
stopifnot("Experiment" \%k\% vaas_1) # present
stopifnot("Species" \%k\% vaas_1) # present
stopifnot(!"Run" \%k\% vaas_1) # missing
stopifnot(c("Species", "Experiment") \%k\% vaas_1) # all present
stopifnot(!c("Species", "Trial") \%k\% vaas_1) # only one present
stopifnot(!c("Organism", "Experiment") \%k\% vaas_1) # only one present
stopifnot(character() \%k\% vaas_1) # empty query always results

# List method
stopifnot(list(Experiment = "whatever") \%k\% vaas_1) # key present
stopifnot(list(Species = "ignored") \%k\% vaas_1) # key present

# This fails because we query with a named sublist but the 'Species'
# metadata entry is not even a list.
stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) \%k\% vaas_1)

# This is OK because we query with an unnamed sublist: it has no names that
# one would fail to find.
stopifnot(list(Species = list("X", "Y")) \%k\% vaas_1)

# More non-nested query examples
stopifnot(!list(Run = 99) \%k\% vaas_1) # key not present
stopifnot(list(Species = "?", Experiment = NA) \%k\% vaas_1) # keys present
stopifnot(!list(Species = "?", Trial = NA) \%k\% vaas_1) # one key missing
stopifnot(!list(Organism = "?", Experiment = NA) \%k\% vaas_1) # likewise
stopifnot(list() \%k\% vaas_1) # empty query always results

# Formulas for querying, compare with list examples above
stopifnot((~ Experiment) \%k\% vaas_1) # key present
stopifnot(vaas_1 \%k\% ~ Experiment) # key present, no parens needed
stopifnot(vaas_1 \%k\% ~ Species) # key present, no parens needed
stopifnot(!vaas_1 \%k\% ~ Species$Epithet) # nested key not present
stopifnot(!vaas_1 \%k\% ~ missing.name) # key not present
missing.name <- "abc"
stopifnot(vaas_1 \%k\% ~ missing.name) # key found in enclosing environment
rm(missing.name) # tidy up
}
\keyword{attribute}

\docType{methods}
\alias{infix.k-methods}
\alias{infix.k,character,WMD-method}
\alias{\%k\%,character,WMD-method}
\alias{infix.k,list,WMD-method}
\alias{\%k\%,list,WMD-method}
\alias{infix.k,formula,WMD-method}
\alias{\%k\%,formula,WMD-method}
\alias{infix.k,factor,WMD-method}
\alias{\%k\%,factor,WMD-method}
\alias{infix.k,list,OPMS-method}
\alias{\%k\%,list,OPMS-method}
\alias{infix.k,character,OPMS-method}
\alias{\%k\%,character,OPMS-method}
\alias{infix.k,factor,OPMS-method}
\alias{\%k\%,factor,OPMS-method}
\alias{infix.k,formula,OPMS-method}
\alias{\%k\%,formula,OPMS-method}
\alias{infix.k,WMD,ANY-method}
\alias{\%k\%,WMD,ANY-method}
\alias{infix.k,OPMS,ANY-method}
\alias{\%k\%,OPMS,ANY-method}
\usage{
  \S4method{\%k\%}{character,WMD}(x, table) 

  \S4method{\%k\%}{list,WMD}(x, table) 

  \S4method{\%k\%}{formula,WMD}(x, table) 

  \S4method{\%k\%}{factor,WMD}(x, table) 

  \S4method{\%k\%}{list,OPMS}(x, table) 

  \S4method{\%k\%}{character,OPMS}(x, table) 

  \S4method{\%k\%}{factor,OPMS}(x, table) 

  \S4method{\%k\%}{formula,OPMS}(x, table) 

  \S4method{\%k\%}{WMD,ANY}(x, table) 

  \S4method{\%k\%}{OPMS,ANY}(x, table) 

}
