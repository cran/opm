\name{\%K\%}
\alias{\%K\%}
\alias{infix.largek}
\title{Search in metadata keys (strict version)}
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
  \code{\link{infix.k}}.
}
\details{
  The behaviour of these methods depends on the object used
  as query. \itemize{ \item Using a character vector as
  query, this method tests whether a given key is present
  in the metadata and fetches an object that is not
  \code{NULL}. If the key has a length > 1, sublists are
  queried. An empty vector results in \code{TRUE}. Note
  that the values of the character vector, not its names,
  if any, are used for querying the metadata.  \item Using
  a list as query, this function behaves like
  \code{\link{infix.k}}.  \item The factor method first
  converts \code{x} to \sQuote{character} mode.  \item The
  formula method attempts to evaluate the right side of the
  formula in the context of the metadata of \code{table}
  and returns whether or not this fails (yields an error).
  In contrast to \code{infix.k}, symbols that are not found
  within the metadata are looked up in the base
  environment.  But note that missing objects are not the
  only potential reason of failure. } See
  \code{\link{subset}} for usage examples with
  \code{\link{OPMS}} objects.
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

# Single-element queries
stopifnot("Experiment" \%K\% vaas_1) # present
stopifnot("Species" \%K\% vaas_1)  # present
stopifnot(!"Run" \%K\% vaas_1) # not present
stopifnot(!"Trial" \%K\% vaas_1) # not present
stopifnot(!"Organism" \%k\% vaas_1) # not present

# Zero-element queries
stopifnot(character() \%K\% vaas_1) # always results

# Querying with vectors of length > 1 mean nested queries; compare this to
# the behavior of \%k\%!
stopifnot(!c("Species", "Experiment") \%K\% vaas_1)
# I.e. "Experiment" is not within "Species".

# List method
# See \%k\% -- the behavior is identical for lists.

# Formulas for querying, compare with \%k\%
stopifnot((~ Experiment) \%K\% vaas_1) # key present
stopifnot(vaas_1 \%K\% ~ Experiment) # key present, no parens needed
stopifnot(vaas_1 \%K\% ~ Species) # key present, no parens needed
stopifnot(!vaas_1 \%K\% ~ Species$Epithet) # nested key not present
stopifnot(!vaas_1 \%K\% ~ missing.name) # key not present
missing.name <- "abc"
stopifnot(!vaas_1 \%K\% ~ missing.name) # enclosing environment ignored
rm(missing.name) # tidy up
}
\keyword{attribute}

\docType{methods}
\alias{infix.largek-methods}
\alias{infix.largek,character,WMD-method}
\alias{\%K\%,character,WMD-method}
\alias{infix.largek,list,WMD-method}
\alias{\%K\%,list,WMD-method}
\alias{infix.largek,formula,WMD-method}
\alias{\%K\%,formula,WMD-method}
\alias{infix.largek,factor,WMD-method}
\alias{\%K\%,factor,WMD-method}
\alias{infix.largek,list,OPMS-method}
\alias{\%K\%,list,OPMS-method}
\alias{infix.largek,character,OPMS-method}
\alias{\%K\%,character,OPMS-method}
\alias{infix.largek,factor,OPMS-method}
\alias{\%K\%,factor,OPMS-method}
\alias{infix.largek,formula,OPMS-method}
\alias{\%K\%,formula,OPMS-method}
\alias{infix.largek,WMD,ANY-method}
\alias{\%K\%,WMD,ANY-method}
\alias{infix.largek,OPMS,ANY-method}
\alias{\%K\%,OPMS,ANY-method}
\usage{
  \S4method{\%K\%}{character,WMD}(x, table) 

  \S4method{\%K\%}{list,WMD}(x, table) 

  \S4method{\%K\%}{formula,WMD}(x, table) 

  \S4method{\%K\%}{factor,WMD}(x, table) 

  \S4method{\%K\%}{list,OPMS}(x, table) 

  \S4method{\%K\%}{character,OPMS}(x, table) 

  \S4method{\%K\%}{factor,OPMS}(x, table) 

  \S4method{\%K\%}{formula,OPMS}(x, table) 

  \S4method{\%K\%}{WMD,ANY}(x, table) 

  \S4method{\%K\%}{OPMS,ANY}(x, table) 

}
