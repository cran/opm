\name{find_substrate}
\alias{find_substrate}
\title{Identify substrates}
\arguments{
  \item{x}{Query character vector.}

  \item{search}{Character scalar indicating the search
  mode. If \sQuote{exact}, query names must exactly match
  (parts of) the well annotations. If \sQuote{glob}, shell
  globbing is used. If \sQuote{approx}, approximate
  matching is used; the number or proportion of errors
  allowed is set using \code{max.dev}, and neither globbing
  or regular-expression matching is done in that case. If
  \sQuote{regex}, regular-expression matching is used.  All
  matching is case-insensitive except for \sQuote{exact}
  search mode.}

  \item{max.dev}{Numeric scalar indicating the maximum
  allowed deviation. If < 1, the proportion of characters
  that might deviate, otherwise their absolute number. It
  can also be a list; see the \sQuote{max.distance}
  argument of \code{agrep} in the \pkg{base} package for
  details. Has an effect only if \sQuote{approx} is chosen
  as search mode (see the \code{search} argument).}
}
\value{
  List of character vectors (empty if nothing was found),
  with duplicates removed and the rest sorted. The names of
  the list correspond to \code{names}.
}
\description{
  Identify the names of substrates as used in the stored
  plate annotations. Exact or error-tolerant matching can
  be used, as well as globbing and regular-expression
  matching.
}
\note{
  See \code{\link{glob_to_regex}} for a description of
  globbing patterns.
}
\examples{
# Note that 'exact' search uses partial matching, whereas globbing is exact
# if there are no wildcards
(x <- find_substrate("a-D-Glucose", search = "exact"))
(y <- find_substrate("a-D-Glucose", search = "glob"))
stopifnot(length(x[[1]]) > length(y[[1]]))

# Now allowing mismatches
(z <- find_substrate("a-D-Glucose", search = "approx"))
stopifnot(length(z[[1]]) > length(x[[1]]))
}
\seealso{
  base::grep base::agrep

  Other naming-functions: \code{\link{find_positions}},
  \code{\link{normalize_plate_name}},
  \code{\link{well_to_substrate}}
}
\keyword{character}
\keyword{utilities}

\docType{methods}
\alias{find_substrate-methods}
\alias{find_substrate,character-method}
\usage{
  \S4method{find_substrate}{character}(x,
    search = c("exact", "glob", "approx", "regex"), max.dev = 0.2) 

}
