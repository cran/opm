\name{map_values}
\alias{map_values}
\title{Map values}
\arguments{
  \item{object}{List (may be nested), dataframe or
  character vector. If it has names, they are preserved.
  \code{NULL} can also be given and yields \code{NULL} or
  an empty named character vector (if \code{mapping} is
  missing). \code{object} may also belong to the virtual
  class \code{\link{MOA}}, comprising matrices and arrays.}

  \item{mapping}{Character vector used as a mapping from
  its names to its values. Values from \code{object} are
  searched for in the \code{names} attribute of
  \code{mapping}; those found are replaced by the
  corresponding values of \code{mapping}. If \code{mapping}
  is missing, a character vector is returned (sorted and
  with duplicates removed) whose names are identical to the
  values. This eases the construction of mapping vectors
  specific for \code{object}. If \code{mapping} is missing,
  the \code{coerce} argument must be named. \code{mapping}
  changes its usage if \code{coerce} is \code{TRUE}. For
  \code{\link{MOA}} objects, \code{mapping} can also be a
  function. The function would be applied to \code{object}
  after conversion with \code{as.vector}, and it would be
  attempted to add the original attributes (particularly
  important are \sQuote{dim} and \sQuote{dimnames} back to
  the result. For \code{\link{MOA}} objects, if
  \code{mapping} is the usual character vector, it then is
  used for mapping the \code{storage.mode}, not the
  \code{class} of \code{object}.}

  \item{coerce}{Character vector with the names of classes
  that are coerced to \sQuote{character} to allow the
  mapping. Other classes are returned unchanged. Note that
  the coerced data are \strong{not} converted back to their
  original data type. \sQuote{ANY} can be used to indicate
  that all classes will be considered.  Alternatively,
  \code{coerce} can be \code{TRUE}. \code{mapping} is then
  interpreted as a mapping between the names of classes,
  and \code{as} from the \pkg{methods} package is used for
  conducting the requested coercions.  Attempting an
  undefined coercion will result in an error.}

  \item{...}{Optional further arguments to \code{mapping}
  (if it is a function).}
}
\value{
  List, data frame, character vector or \code{NULL}.
}
\description{
  Map \sQuote{character} data using another
  \sQuote{character} vector, or recursively apply a mapping
  function to all \sQuote{character} values within a list,
  or non-recursively to a dataframe. Optionally coerce
  other data types to \sQuote{character}; return remaining
  ones unchanged. It is also possible to map between
  classes using coercion functions. For convenience in
  programming, methods for the \sQuote{NULL} class are also
  available.
}
\note{
  This function is not normally directly called by an
  \pkg{opm} user because \code{\link{map_metadata}} is
  available.
}
\examples{
# Character+character method
map <- letters
names(map) <- rev(LETTERS)
(x <- map_values(LETTERS, map))
stopifnot(rev(x) == letters)

# Character+missing method
(x <- map_values(letters))
stopifnot(x == letters, names(x) == letters)

# List+character method
x <- list(a = 1:8, c = 9, d = 'x')
map <- c(a = "b", e = "f", x = "y")
(y <- map_values(x, map))
stopifnot(identical(x[1:2], y[1:2]), !identical(x[3], y[3]))
(y <- map_values(x, map, coerce = "integer"))
stopifnot(identical(x[2], y[2]), !identical(x[1], y[1]),
  !identical(x[3], y[3]))
(y <- map_values(x, map, coerce = TRUE))
stopifnot(identical(x, y))
(y <- map_values(x, c(numeric = "character"), coerce = TRUE))
stopifnot(identical(x[1], y[1]), !identical(x[2], y[2]),
  identical(x[3], y[3]))

# List+missing method
(y <- map_values(x))
stopifnot(y == "x", names(y) == y)
(y <- map_values(x, coerce = "integer"))
stopifnot(length(y) == 9, names(y) == y)
(y <- map_values(x, coerce = c("integer", "numeric")))
stopifnot(length(y) == 10, names(y) == y)
(y <- map_values(x, coerce = "ANY")) # same effect
stopifnot(length(y) == 10, names(y) == y)
(y <- map_values(x, coerce = TRUE))
stopifnot(y == c("character", "integer", "numeric"), names(y) == y)

# Dataframe+character method
x <- data.frame(a = 1:3, b = letters[1:3])
stopifnot(sapply(x, class) == c("integer", "factor"))
map <- c(a = "A", b = "B", c = "C", `1` = "5")
(y <- map_values(x, map))
stopifnot(identical(x, y))
(y <- map_values(x, map, coerce = "factor"))
stopifnot(!identical(x, y), y[[2]] == c("A", "B", "C"))
(y <- map_values(x, map, coerce = "ANY"))
stopifnot(y[[1]] == c("5", "2", "3"), y[[2]] == c("A", "B", "C"))
(y <- map_values(x, map, coerce = TRUE))
stopifnot(identical(x, y))
map <- c(factor = "character", integer = "complex")
(y <- map_values(x, map, coerce = TRUE))
stopifnot(sapply(y, class) == c("complex", "character"))

# Dataframe+missing method
(y <- map_values(x))
stopifnot(is.character(y), length(y) == 0)
(y <- map_values(x, coerce = "factor"))
stopifnot(is.character(y), y == letters[1:3], names(y) == y)
(y <- map_values(x, coerce = "ANY"))
stopifnot(is.character(y), length(y) == 6, names(y) == y)
(y <- map_values(x, coerce = TRUE))
stopifnot(is.character(y), y == c("factor", "integer"), names(y) == y)
}
\seealso{
  base::rapply base::list base::as.list methods::as
  base::class base::storage.mode base::as.vector

  Other list-functions: \code{\link{contains}},
  \code{\link{map_names}}
}
\keyword{list}
\keyword{manip}

\docType{methods}
\alias{map_values-methods}
\alias{map_values,list,character-method}
\alias{map_values,list,function-method}
\alias{map_values,list,missing-method}
\alias{map_values,data.frame,function-method}
\alias{map_values,data.frame,character-method}
\alias{map_values,data.frame,missing-method}
\alias{map_values,MOA,character-method}
\alias{map_values,MOA,missing-method}
\alias{map_values,MOA,function-method}
\alias{map_values,character,function-method}
\alias{map_values,character,character-method}
\alias{map_values,character,missing-method}
\alias{map_values,NULL,character-method}
\alias{map_values,NULL,missing-method}
\usage{
  \S4method{map_values}{list,character}(object, mapping,
    coerce = character()) 

  \S4method{map_values}{list,function}(object, mapping,
    coerce = character(), ...) 

  \S4method{map_values}{list,missing}(object, 
    coerce = character()) 

  \S4method{map_values}{data.frame,function}(object, mapping,
    coerce = character(), ...) 

  \S4method{map_values}{data.frame,character}(object, mapping,
    coerce = character()) 

  \S4method{map_values}{data.frame,missing}(object, 
    coerce = character()) 

  \S4method{map_values}{MOA,character}(object, mapping,
    coerce = TRUE) 

  \S4method{map_values}{MOA,missing}(object, coerce = TRUE) 

  \S4method{map_values}{MOA,function}(object, mapping, ...) 

  \S4method{map_values}{character,function}(object, mapping, 
    ...) 

  \S4method{map_values}{character,character}(object, mapping) 

  \S4method{map_values}{character,missing}(object) 

  \S4method{map_values}{NULL,character}(object, mapping) 

  \S4method{map_values}{NULL,missing}(object, mapping) 

}
