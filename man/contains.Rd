\name{contains}
\alias{contains}
\title{Query a list with a list}
\arguments{
  \item{object}{List containing the data, or
  \code{\link{OPMS}} object.}

  \item{other}{For the list method, a list used as query;
  for the \code{\link{OPMS}} method, an \code{\link{OPM}}
  object used as query.}

  \item{values}{Logical scalar. Compare also the values or
  only the keys? If \code{FALSE}, \code{exact} is ignored.}

  \item{exact}{Logical scalar. If \code{FALSE}, the data
  value(s) might by any of the query value(s), and some
  coercion is done before comparing (see \code{match} for
  details. If \code{TRUE}, the data value(s) must exactly
  correspond to the query value(s), and no coercion is done
  (see \code{identical}) for details). This might be too
  strict for most applications.}

  \item{...}{Optional arguments passed to \code{identical}
  from the \pkg{base} package, allowing for fine-control of
  identity. Has no effect unless \code{exact} is
  \code{TRUE}.}
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
  query list result in \code{FALSE}. There is also an
  \code{\link{OPMS}} method, which tests whether an
  \code{\link{OPM}} object is contained.
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user but might be useful in other contexts. It
  forms the basis of a number of metadata query functions.
}
\examples{
# List/list method
x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
stopifnot(contains(x, y))
stopifnot(!contains(x, y, exact = TRUE))
stopifnot(contains(x, y, exact = TRUE, values = FALSE))
# see particularly infix-q and infix-k for more examples

# OPMS/OPM method
data(vaas_4)
data(vaas_1)
stopifnot(contains(vaas_4, vaas_1))
}
\seealso{
  base::list base::as.list base::`[` base::`[[` base::match

  base::identity

  Other list-functions: \code{\link{map_names}},
  \code{\link{map_values}}
}
\keyword{attribute}
\keyword{list}

\docType{methods}
\alias{contains-methods}
\alias{contains,list,list-method}
\alias{contains,OPMS,OPM-method}
\usage{
  \S4method{contains}{list,list}(object, other,
    values = TRUE, exact = FALSE, ...) 

  \S4method{contains}{OPMS,OPM}(object, other, ...) 

}
