\name{map_values}
\alias{map_values}
\title{Map values}
\arguments{
  \item{object}{List (may be nested).}

  \item{mapping}{Character vector used as a mapping from
  its names to its values.}

  \item{coerce}{Character vector with the names of classes
  that are coerced to \sQuote{character} to allow the
  mapping. Other classes are returned unchanged. Note that
  the coerced data are \strong{not} converted back to their
  original datatype.}
}
\value{
  List.
}
\description{
  Recursively apply a mapping function to all
  \sQuote{character} values within a list. Optionally
  coerce other datatypes to \sQuote{character}; return
  remaing ones unchanged.
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user because \code{\link{map_metadata}} is
  available.
}
\examples{
x <- list(a = 1:8, c = 9, d = 'x')
map <- c(a = "b", e = "f", x = "y")
(y <- map_values(x, map))
stopifnot(identical(x[1:2], y[1:2]))
stopifnot(!identical(x[3], y[3]))
# compare with the map_names() example
}
\seealso{
  rapply list as.list

  Other list-functions: \code{\link{contains}},
  \code{\link{map_names}},
  \code{\link{map_names,list+character}},
  \code{\link{map_names,list+missing}},
  \code{\link{map_values,character}}
}
\keyword{list}
\keyword{manip}

\docType{methods}
\alias{map_values-methods}
\alias{map_values,list,character-method}
\usage{
  \S4method{map_values}{list,character}(object, mapping,
    coerce = character()) 
}
