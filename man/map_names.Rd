\name{map_names}
\alias{map_names}
\title{Map names}
\arguments{
  \item{object}{Any \R object. The default method applies
  the mapping to the \sQuote{names} attribute. The
  behaviour is special for lists, which are traversed
  recursively to also consider sublists with names. Data
  frames and \code{\link{MOA}} objects (that is, including
  matrices and arrays) are also treated specially because
  the \sQuote{dimnames} attribute, not the \sQuote{names}
  attribute is considered.}

  \item{mapping}{Mapping function that takes a character
  vector as first argument, or character vector used for
  mapping from its names to its values, or missing. It is
  guaranteed that \code{NULL} input remains \code{NULL},
  irrespective of the value of \code{mapping}.}

  \item{...}{Optional further arguments to \code{mapping}
  (if it is a function).}
}
\value{
  Character vector if \code{mapping} is missing, otherwise
  an \R object of the same class than \code{object}.
}
\description{
  Use a character vector or a function for recursively
  mapping list names, or mapping the \sQuote{colnames} and
  \sQuote{rownames} attributes of a data frame. In the case
  of lists, the function is not applied to list elements
  which are not themselves lists, even if they have a
  \sQuote{names} attribute. Such elements and their names,
  if any, are returned unchanged. If a \sQuote{names},
  \sQuote{colnames} or \sQuote{rownames} attribute is
  \code{NULL}, it is ignored. Alternatively, instead of
  mapping the names, collect them and return them as a
  single character vector, sorted and with duplicates
  removed. The collected names are added as their own
  \code{names} attribute; this might be useful if the
  result is later on used for some mapping (using this
  function or \code{\link{map_values}}).
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user because \code{\link{map_metadata}} is
  available.
}
\examples{
# List/function method
x <- list(a = 1:8, c = 9, d = 'x')
map <- function(x) sprintf("\%s\%s", x, x)
(y <- map_names(x, map))
stopifnot(identical(as.character(x), as.character(y)))
stopifnot(!identical(names(x), names(y)))

# List/character method
x <- list(a = 1:8, c = 9, d = 'x')
map <- c(a = "b", e = "f", x = "y")
(y <- map_names(x, map))
stopifnot(identical(as.character(x), as.character(y)))
stopifnot(!identical(names(x), names(y)))
# compare with the map_values() example

# List/missing method
x <- list(a = 1:8, c = 9, d = 'x')
(y <- map_names(x))
stopifnot(identical(as.vector(y), names(x)))
stopifnot(identical(names(y), names(x)))
# Now a recursive list
x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
(y <- map_names(x))
stopifnot(length(y) > length(names(x)))

# Data frame/function method
x <- data.frame(a = 1:3, b = letters[1:3])
(y <- map_names(x, toupper))
stopifnot(identical(y[[1]], x[[1]]), identical(y[[2]], x[[2]]))
stopifnot(identical(names(y), c("A", "B")))

# Data frame/character method
(y <- map_names(x, c(a = "b", b = "a")))
stopifnot(x == y, names(y) == c("b", "a"))

# Data frame/missing method
(y <- map_names(x))
stopifnot(is.character(y), y == names(y), length(y) == 5)

# Matrix/function method
x <- as.matrix(x)
(y <- map_names(x, toupper))
stopifnot(x == y, toupper(colnames(x)) == colnames(y))

# Matrix/character method
(y <- map_names(x, c(a = "b", b = "a")))
stopifnot(x == y, colnames(y) == c("b", "a"))

# Matrix/missing method
(y <- map_names(x))
stopifnot(y == c("a", "b"), names(y) == y)
}
\seealso{
  base::rapply base::list base::as.list

  Other auxiliary-functions: \code{\link{contains}},
  \code{\link{glob_to_regex}}, \code{\link{kubrick}},
  \code{\link{map_values}}, \code{\link{opm_opt}},
  \code{\link{separate}}
}
\keyword{list}
\keyword{manip}

\docType{methods}
\alias{map_names-methods}
\alias{map_names,list,function-method}
\alias{map_names,list,character-method}
\alias{map_names,list,missing-method}
\alias{map_names,data.frame,function-method}
\alias{map_names,data.frame,character-method}
\alias{map_names,data.frame,missing-method}
\alias{map_names,MOA,function-method}
\alias{map_names,MOA,character-method}
\alias{map_names,MOA,missing-method}
\alias{map_names,ANY,function-method}
\alias{map_names,ANY,character-method}
\alias{map_names,ANY,missing-method}
\usage{
  \S4method{map_names}{list,function}(object, mapping, ...) 

  \S4method{map_names}{list,character}(object, mapping) 

  \S4method{map_names}{list,missing}(object) 

  \S4method{map_names}{data.frame,function}(object, mapping,
    ...) 

  \S4method{map_names}{data.frame,character}(object, mapping) 

  \S4method{map_names}{data.frame,missing}(object) 

  \S4method{map_names}{MOA,function}(object, mapping, ...) 

  \S4method{map_names}{MOA,character}(object, mapping) 

  \S4method{map_names}{MOA,missing}(object) 

  \S4method{map_names}{ANY,function}(object, mapping, ...) 

  \S4method{map_names}{ANY,character}(object, mapping) 

  \S4method{map_names}{ANY,missing}(object) 

}
