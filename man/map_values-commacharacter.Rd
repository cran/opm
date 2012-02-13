\name{map_values,character}
\alias{map_values,character}
\title{Map values}
\arguments{
  \item{object}{Character vector. If it has names, they are
  preserved.}

  \item{mapping}{Mapping character vector. Values from
  \code{object} are searched for in the \code{names}
  attribute of \code{mapping}; those found are replaced by
  the corresponding values of \code{mapping}.}
}
\value{
  Character vector.
}
\description{
  Map \sQuote{character} data using another
  \sQuote{character} vector.
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user because \code{\link{map_metadata}} is
  available.
}
\examples{
map <- letters
names(map) <- rev(LETTERS)
got <- map_values(LETTERS, map)
stopifnot(rev(got) == letters)
}
\seealso{
  Other list-functions: \code{\link{contains}},
  \code{\link{map_names}},
  \code{\link{map_names,list+character}},
  \code{\link{map_names,list+missing}},
  \code{\link{map_values}}
}
\keyword{character}
\keyword{manip}

\docType{methods}
\alias{map_values,character,character-method}
\usage{
  \S4method{map_values}{character,character}(object, mapping) 
}
