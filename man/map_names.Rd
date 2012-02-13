\name{map_names}
\alias{map_names}
\title{Map names}
\arguments{
  \item{object}{List.}

  \item{mapping}{Mapping function that takes a character
  vector as first argument.}

  \item{...}{Optional further arguments to \code{mapping}.}
}
\value{
  List.
}
\description{
  Recursively apply a mapping function to all names of a
  list. Note that the function is not applied to list
  elements which are not themselves lists, even if they
  have a \sQuote{names} attribute. Such elements and their
  names, if any, are returned unchanged.
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user because
  \code{\link{map_metadata,WMD+function}} is available.
}
\examples{
x <- list(a = 1:8, c = 9, d = 'x')
map <- function(x) sprintf("\%s\%s", x, x)
(y <- map_names(x, map))
stopifnot(identical(as.character(x), as.character(y)))
stopifnot(!identical(names(x), names(y)))
}
\seealso{
  rapply list as.list

  Other list-functions: \code{\link{contains}},
  \code{\link{map_names,list+character}},
  \code{\link{map_names,list+missing}},
  \code{\link{map_values}},
  \code{\link{map_values,character}}
}
\keyword{list}
\keyword{manip}

\docType{methods}
\alias{map_names-methods}
\alias{map_names,list,function-method}
\usage{
  \S4method{map_names}{list,function}(object, mapping, ...) 
}
