\name{extract_columns}
\alias{extract_columns}
\title{Create data frame or vector from metadata}
\arguments{
  \item{object}{\code{\link{OPMS}} object or data frame.}

  \item{what}{For the \code{\link{OPMS}} method, a list of
  metadata keys to consider, or single such key; passed to
  \code{\link{metadata}}. A formula is also possible; see
  there for details. A peculiarity of
  \code{extract_columns} is that including \code{J} as a
  pseudo-function call in the formula triggers the
  combination of metadata entries to new factors
  immediately after selecting them, as long as \code{join}
  is \code{FALSE}.

  For the data-frame method, just the names of the columns
  to extract, or their indices, as vector, if \code{direct}
  is \code{TRUE}. Alternatively, the name of the class to
  extract from the data frame to form the matrix values.

  In the \sQuote{direct} mode, \code{what} can also be a
  named list of vectors used for indexing. In that case a
  data frame is returned that contains the columns from
  \code{object} together with new columns that result from
  pasting the selected columns together.}

  \item{join}{Logical scalar. Join each row together to
  yield a character vector? Otherwise it is just attempted
  to construct a data frame.}

  \item{sep}{Character scalar. Used as separator between
  the distinct metadata entries if these are to be pasted
  together. Ignored unless \code{join} is \code{TRUE}. The
  data-frame method always joins the data unless
  \code{what} is a list.}

  \item{dups}{Character scalar specifying what to do in the
  case of duplicate labels: either \sQuote{warn},
  \sQuote{error} or \sQuote{ignore}. Ignored unless
  \code{join} is \code{TRUE}.}

  \item{factors}{Logical scalar determining whether strings
  should be converted to factors. Note that this would only
  affect newly created data-frame columns.}

  \item{exact}{Logical scalar. Also passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar. Also passed to
  \code{\link{metadata}}.}

  \item{as.labels}{Character vector. See
  \code{\link{extract}}.}

  \item{as.groups}{Character vector. See
  \code{\link{extract}}.}

  \item{direct}{Logical scalar. Extract column names
  directly, or search for columns of one to several given
  classes?}
}
\value{
  For the \code{OPMS} method, a data frame or character
  vector, depending on the \code{join} argument. The
  data-frame method returns a character vector or a data
  frame, too, but depending on the \code{what} argument.
}
\description{
  Extract selected metadata entries for use as additional
  columns in a data frame or (after joining) as character
  vector with labels.
}
\details{
  This function is not normally directly called by an
  \pkg{opm} user because \code{\link{extract}} is
  available, which uses this function, but can be used for
  testing the applied metadata selections beforehand.

  The data-frame method is partially trivial (extract the
  selected columns and join them to form a character vector
  or new data-frame columns), partially more useful
  (extract columns with data of a specified class).
}
\examples{
# 'OPMS' method
data(vaas_4)

# Create data frame
(x <- extract_columns(vaas_4, what = list("Species", "Strain")))
stopifnot(is.data.frame(x), dim(x) == c(4, 2))
(y <- extract_columns(vaas_4, what = ~ Species + Strain))
stopifnot(identical(x, y)) # same result using a formula
(y <- extract_columns(vaas_4, what = ~ J(Species + Strain)))
stopifnot(is.data.frame(y), dim(y) == c(4, 3)) # additional column created
stopifnot(identical(x, y[, -3]))

# Create a character vector
(x <- extract_columns(vaas_4, what = list("Species", "Strain"), join = TRUE))
stopifnot(is.character(x), length(x) == 4L)
(x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
  dups = "error"), silent = TRUE)) # duplicates yield error
stopifnot(inherits(x, "try-error"))
(x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
  dups = "warn"), silent = TRUE)) # duplicates yield warning only
stopifnot(is.character(x), length(x) == 4L)

# data-frame method, 'direct' running mode
x <- data.frame(a = 1:26, b = letters, c = LETTERS)
(y <- extract_columns(x, I(c("a", "b")), sep = "-"))
stopifnot(grepl("^\\\s*\\\d+-[a-z]$", y)) # pasted columns 'a' and 'b'

# data-frame method, using class name
(y <- extract_columns(x, as.labels = "b", what = "integer", as.groups = "c"))
stopifnot(is.matrix(y), dim(y) == c(26, 1), rownames(y) == x$b)
stopifnot(identical(attr(y, "row.groups"), x$c))
}
\seealso{
  base::data.frame base::as.data.frame base::cbind

  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{extract}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{dplot}
\keyword{manip}

\docType{methods}
\alias{extract_columns-methods}
\alias{extract_columns,OPMS-method}
\alias{extract_columns,data.frame-method}
\usage{
  \S4method{extract_columns}{OPMS}(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) 

  \S4method{extract_columns}{data.frame}(object, what,
    as.labels = NULL, as.groups = NULL, sep = opm_opt("comb.value.join"),
    factors = is.list(what), direct = inherits(what, "AsIs")) 

}
