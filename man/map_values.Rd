\name{map_values}
\alias{map_values}
\title{Map values}
\arguments{
  \item{object}{List (may be nested), data frame or
  character vector. If it has names, they are preserved.
  \code{NULL} can also be given and yields \code{NULL} or
  an empty named character vector (if \code{mapping} is
  missing). \code{object} may also belong to the virtual
  class \code{\link{MOA}}, comprising matrices and arrays.}

  \item{mapping}{Character vector, function, formula, or
  missing.  If a character vector used as a mapping from
  its names to its values. Values from \code{object} are
  searched for in the \code{names} attribute of
  \code{mapping}; those found are replaced by the
  corresponding values of \code{mapping}.  If
  \code{mapping} is missing, a character vector is returned
  (sorted and with duplicates removed) whose names are
  identical to the values. This eases the construction of
  mapping vectors specific for \code{object}. If
  \code{mapping} is missing, the \code{coerce} argument
  must be named.  \code{mapping} changes its usage if
  \code{coerce} is \code{TRUE}. For \code{\link{MOA}}
  objects, if \code{mapping} was a function, it would be
  applied to \code{object} after conversion with
  \code{as.vector}, and it would be attempted to add the
  original attributes (particularly important are
  \sQuote{dim} and \sQuote{dimnames} back to the result.
  For \code{\link{MOA}} objects, if \code{mapping} is the
  usual character vector, it then is used for mapping the
  \code{storage.mode}, not the \code{class} of
  \code{object}.  \code{mapping} can also be a formula, it
  is then used to compute on lists.  The see examples
  below.}

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
  or non-recursively to a data frame. Optionally coerce
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
# Character/character method
map <- letters
names(map) <- rev(LETTERS)
(x <- map_values(LETTERS, map))
stopifnot(rev(x) == letters)

# Character/missing method
(x <- map_values(letters))
stopifnot(x == letters, names(x) == letters)

# Character/function method
x <- letters[1:4]
names(x) <- LETTERS[1:4]
(y <- map_values(x, function(z) sprintf("\%s\%s", z, z)))
stopifnot(names(y) == names(x), y != x)

# List/character method
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

# List/function method
(y <- map_values(x, identity, coerce = "ANY"))
stopifnot(identical(x, y))
(y <- map_values(x, class, coerce = "ANY"))
stopifnot(sapply(y, class) == "character", names(y) == names(x))

# List/missing method
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

# List/formula method
(y <- map_values(x, ~ a + c))
stopifnot(is.numeric(y), y == c(10:17))
(y <- map_values(x, b ~ a + c))
stopifnot(is.list(y), y$b == c(10:17))

# List/formula method applied to a data frame
x <- data.frame(a = 1:5, b = 6:10)
(y <- map_values(x, c ~ a + b))
stopifnot(is.data.frame(y), dim(y) == c(5, 3))
(z <- map_values(x, ~ a + b))
stopifnot(identical(z, y$c))

# Data frame/character method
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

# Data frame/function method
(y <- map_values(x, `+`, coerce = "integer", y = 1L))
stopifnot(y$a == x$a + 1L)
(y <- map_values(x, as.character, coerce = "factor"))
stopifnot(sapply(y, class) == c("integer", "character"))

# Data frame/missing method
(y <- map_values(x))
stopifnot(is.character(y), length(y) == 0)
(y <- map_values(x, coerce = "factor"))
stopifnot(is.character(y), y == letters[1:3], names(y) == y)
(y <- map_values(x, coerce = "ANY"))
stopifnot(is.character(y), length(y) == 6, names(y) == y)
(y <- map_values(x, coerce = TRUE))
stopifnot(is.character(y), y == c("factor", "integer"), names(y) == y)

# Matrix/character method
(x <- matrix(1:6, nrow = 2))
(y <- map_values(x, c(integer = "numeric"), coerce = TRUE))
stopifnot(storage.mode(x) != storage.mode(y))
(y <- map_values(x, c(`1` = "7"), coerce = "integer"))
stopifnot(is.character(y), y[-1] == x[-1])

# Matrix/function method
(y <- map_values(x, identity))
stopifnot(identical(x, y))
(y <- map_values(x, `+`, y = 1)) # useless because '+' is directly available
stopifnot(dim(y) == dim(x), y == x + 1)

# Matrix/missing method
(y <- map_values(x))
stopifnot(y == "integer", names(y) == y)
(y <- map_values(x, coerce = "ANY"))
stopifnot(is.character(y), y == 1:6, names(y) == y)

# Factor/function method
x <- as.factor(c("a", "b", "a"))
(y <- map_values(x, toupper))
stopifnot(is.factor(y), y == toupper(x))

# Factor/character method
(y <- map_values(x, c(b = "c", k = "h")))
stopifnot(is.factor(y), levels(y) == c("a", "c"))

# Factor/missing method
(y <- map_values(x))
stopifnot(levels(x) == y, names(y) == y)
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
\alias{map_values,list,formula-method}
\alias{map_values,data.frame,function-method}
\alias{map_values,data.frame,character-method}
\alias{map_values,data.frame,missing-method}
\alias{map_values,MOA,character-method}
\alias{map_values,MOA,missing-method}
\alias{map_values,MOA,function-method}
\alias{map_values,character,function-method}
\alias{map_values,character,character-method}
\alias{map_values,character,missing-method}
\alias{map_values,factor,function-method}
\alias{map_values,factor,character-method}
\alias{map_values,factor,missing-method}
\alias{map_values,NULL,function-method}
\alias{map_values,NULL,character-method}
\alias{map_values,NULL,missing-method}
\usage{
  \S4method{map_values}{list,character}(object, mapping,
    coerce = character()) 

  \S4method{map_values}{list,function}(object, mapping,
    coerce = character(), ...) 

  \S4method{map_values}{list,missing}(object,
    coerce = character()) 

  \S4method{map_values}{list,formula}(object, mapping) 

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

  \S4method{map_values}{factor,function}(object, mapping,
    ...) 

  \S4method{map_values}{factor,character}(object, mapping) 

  \S4method{map_values}{factor,missing}(object) 

  \S4method{map_values}{NULL,function}(object, mapping, ...) 

  \S4method{map_values}{NULL,character}(object, mapping) 

  \S4method{map_values}{NULL,missing}(object, mapping) 

}
