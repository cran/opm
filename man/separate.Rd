\name{separate}
\alias{separate}
\title{Regularly split character vectors if possible}
\arguments{
  \item{object}{Character vector to be split, or data frame
  in which character vectors (or factors) shall be
  attempted to be split, or factor.}

  \item{split}{Character vector or \code{TRUE}. If a
  character vector, used as container of the splitting
  characters and converted to a vector containing only
  non-duplicated single-character strings. For instance,
  the default \code{split} argument \code{".-_"} yields
  \code{c(".", "-", "_")}. If a vector of only empty
  strings or \code{TRUE}, strings with substrings
  representing fixed-width fields are assumed, and
  splitting is done at whitespace-only columns. Beforehand,
  equal-length strings are created by padding with spaces
  at the right.  After splitting in fixed-width mode,
  whitespace characters are trimmed from both ends of the
  resulting strings.}

  \item{simplify}{Logical scalar indicating whether a
  resulting matrix with one column should be simplified to
  a vector (or such a data frame to a factor).}

  \item{keep.const}{Logical scalar indicating whether
  constant columns should be kept or removed. Ignored if
  only a single column is present.}

  \item{coerce}{Logical scalar indicating whether factors
  should be coerced to \sQuote{character} mode and then
  also be attempted to be split. The resulting columns will
  be coerced back to factors.}

  \item{name.sep}{Character scalar to be inserted in the
  constructed column names. If more than one column results
  from splitting, the names will contain (i) the original
  column name, (ii) \code{name.sep} and (iii) their index,
  thus creating unique column names (if the original ones
  were unique).}
}
\value{
  Character matrix, its number of rows being equal to the
  length of \code{object}, or data frame with the same
  number of rows as \code{object} but potentially more
  columns. May be character vector of factor with character
  or factor input and \code{simplify} set to \code{TRUE}.
}
\description{
  From a given set of splitting characters select the ones
  that split a character vector in a regular way, yielding
  the same number of parts for all vector elements. Then
  apply these splitting characters to create a matrix. The
  data frame method applies this to all character vectors
  (and optionally also all factors) within a data frame.
}
\examples{
# Splitting by characters
x <- c("a-b-cc", "d-ff-g")
(y <- separate(x, ".")) # a split character that does not occur
stopifnot(is.matrix(y), y[, 1L] == x)
(y <- separate(x, "-")) # a split character that does occur
stopifnot(is.matrix(y), dim(y) == c(2, 3))

# Fixed-with splitting
x <- c("  abd  efgh", " ABCD EFGH ", " xyz")
(y <- separate(x, TRUE))
stopifnot(is.matrix(y), dim(y) == c(3, 2))

# Applied to factors
xx <- as.factor(x)
(yy <- separate(xx, TRUE))
stopifnot(identical(yy, as.data.frame(y)))

# Data frame method
x <- data.frame(a = 1:2, b = c("a-b-cc", "a-ff-g"))
(y <- separate(x, coerce = FALSE))
stopifnot(identical(x, y))
(y <- separate(x))
stopifnot(is.data.frame(y), dim(y) == c(2, 4))
stopifnot(sapply(y, class) == c("integer", "factor", "factor", "factor"))
(y <- separate(x, keep.const = FALSE))
stopifnot(is.data.frame(y), dim(y) == c(2, 3))
stopifnot(sapply(y, class) == c("integer", "factor", "factor"))
}
\seealso{
  base::strsplit utils::read.fwf

  Other auxiliary-functions: \code{\link{glob_to_regex}}
}
\keyword{character}
\keyword{manip}

\docType{methods}
\alias{separate-methods}
\alias{separate,character-method}
\alias{separate,factor-method}
\alias{separate,data.frame-method}
\usage{
  \S4method{separate}{character}(object, split = "/.-_",
    simplify = FALSE, keep.const = TRUE) 

  \S4method{separate}{factor}(object, split = "/.-_",
    simplify = FALSE, keep.const = TRUE) 

  \S4method{separate}{data.frame}(object, split = "/.-_",
    keep.const = TRUE, coerce = TRUE, name.sep = ".") 

}
