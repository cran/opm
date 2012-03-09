\name{contains}
\alias{contains}
\title{Query a list with a list}
\arguments{
  \item{object}{List containing the data.}

  \item{other}{List used as query.}

  \item{values}{Logical scalar. Compare also the values or
  only the keys? If \code{FALSE}, \code{exact} is ignored.}

  \item{exact}{Logical scalar. If \code{FALSE}, the data
  value(s) might by any of the query value(s), and some
  coercion is done before comparing (see \code{match} for
  details. If \code{TRUE}, the data value(s) must exactly
  correspond to the query value(s), and no coercion is done
  (see \code{identical}) for details). This might be too
  strict for most applications.}
}
\value{
  Logical scalar.
}
\description{
  Test whether all names of a query list occur as names in
  a data list and optionally also whether they point to the
  same elements; apply this principle recursively to all
  sublists. Non-list elements are ignored if \code{values}
  is \code{FALSE}. Otherwise the comparison is done using
  \code{identical} if \code{exact} is \code{TRUE}. If
  \code{exact} is \code{FALSE}, the value(s) in the data
  list can be any of the values at the corresponding
  position in the query list, and the comparison is done by
  coercion to character vectors. An empty query list
  results in \code{TRUE}. Missing names in a non-empty
  query list result in \code{FALSE}.
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user but might be useful in other contexts. It
  forms the basis of a number of metadata query functions.
}
\examples{
x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
stopifnot(contains(x, y))
stopifnot(!contains(x, y, exact = TRUE))
stopifnot(contains(x, y, exact = TRUE, values = FALSE))
# see particularly infix-q and infix-k for more examples
}
\seealso{
  base::list base::as.list base::`[` base::`[[` base::match

  Other list-functions: \code{\link{map_names}},
  \code{\link{map_values}}
}
\keyword{attribute}
\keyword{list}

\docType{methods}
\alias{contains-methods}
\alias{contains,list,list-method}
\usage{
  \S4method{contains}{list,list}(object, other,
    values = TRUE, exact = FALSE) 

}
