\name{map_names,list+character}
\alias{map_names,list+character}
\title{Map names}
\arguments{
  \item{object}{List.}

  \item{mapping}{Character vector used for mapping from its
  names to its values.}
}
\value{
  List.
}
\description{
  Use a character vector for mapping list names.
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user because \code{\link{map_metadata}} is
  available.
}
\examples{
x <- list(a = 1:8, c = 9, d = 'x')
map <- c(a = "b", e = "f", x = "y")
(y <- map_names(x, map))
stopifnot(identical(as.character(x), as.character(y)))
stopifnot(!identical(names(x), names(y)))
# compare with the map_values() example
}
\seealso{
  rapply list as.list

  Other list-functions: \code{\link{contains}},
  \code{\link{map_names}},
  \code{\link{map_names,list+missing}},
  \code{\link{map_values}},
  \code{\link{map_values,character}}
}
\keyword{list}
\keyword{manip}

\docType{methods}
\alias{map_names,list,character-method}
\usage{
  \S4method{map_names}{list,character}(object, mapping) 
}
