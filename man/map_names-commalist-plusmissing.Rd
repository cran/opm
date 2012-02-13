\name{map_names,list+missing}
\alias{map_names,list+missing}
\title{Map names}
\arguments{
  \item{object}{List.}
}
\value{
  Character vector.
}
\description{
  Like the methods \code{\link{map_names}} and
  \code{\link{map_names,list+character}}, but instead of
  mapping the names, they are collected and returned as a
  single character vector, sorted and with duplicates
  removed. The collected names are added as their own
  \code{names} attribute; this might be useful if the
  result is later on used for some mapping (see
  \code{\link{map_values,character}}).
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user because \code{\link{metadata_chars}} is
  available.
}
\examples{
x <- list(a = 1:8, c = 9, d = 'x')
(y <- map_names(x))
stopifnot(identical(as.vector(y), names(x)))
stopifnot(identical(names(y), names(x)))
# Now a recursive list
x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
(y <- map_names(x))
stopifnot(length(y) > length(names(x)))
}
\seealso{
  rapply list as.list

  Other list-functions: \code{\link{contains}},
  \code{\link{map_names}},
  \code{\link{map_names,list+character}},
  \code{\link{map_values}},
  \code{\link{map_values,character}}
}
\keyword{attribute}
\keyword{list}

\docType{methods}
\alias{map_names,list,missing-method}
\usage{
  \S4method{map_names}{list,missing}(object) 
}
