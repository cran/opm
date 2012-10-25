\name{extract}
\alias{extract}
\title{Extract aggregated values}
\arguments{
  \item{object}{\code{\link{OPMS}} object or data frame.}

  \item{as.labels}{List. Metadata to be joined and used as
  row names (if \code{dataframe} is \code{FALSE}) or
  additional columns (if otherwise).  Ignored if
  \code{NULL}.}

  \item{subset}{Character vector. The parameter(s) to put
  in the matrix. If it is \sQuote{disc}, discretized data
  are returned, and \code{ci} is ignored.}

  \item{ci}{Logical scalar. Also return the CIs?}

  \item{trim}{Character scalar. See
  \code{\link{aggregated}} for details.}

  \item{dataframe}{Logical scalar. Return data frame or
  matrix?}

  \item{as.groups}{List. Metadata to be joined and used as
  \sQuote{row.groups} attribute of the output matrix. See
  \code{\link{heat_map}} for its usage.  Ignored if
  \code{NULL} and if \code{dataframe} is \code{FALSE}. For
  the data-frame method, a vector.}

  \item{sep}{Character scalar. See
  \code{\link{extract_columns}}.}

  \item{dups}{Character scalar. See
  \code{\link{extract_columns}}.}

  \item{exact}{Logical scalar. See
  \code{\link{extract_columns}}.}

  \item{strict}{Logical scalar. See
  \code{\link{extract_columns}}.}

  \item{full}{Logical scalar indicating whether full
  substrate names shall be used. This is passed to
  \code{\link{wells}}, but in contrast to what
  \code{\link{flatten}} is doing the argument here refers
  to the generation of the column names.}

  \item{max}{Numeric scalar. Passed to
  \code{\link{wells}}.}

  \item{...}{Optional other arguments passed to
  \code{\link{wells}}.}

  \item{what}{Character scalar. The name of the class to
  extract from the data frame to form the matrix values.}
}
\value{
  Numeric matrix or data frame; always a numeric matrix for
  the data-frame method.
}
\description{
  Extract selected aggregated or discretized values into
  common matrix or data frame. The data-frame method
  creates a matrix by extracting the numeric columns and
  optionally adds row names.
}
\examples{
# 'OPMS' method
data(vaas_4)
# Matrix
(x <- extract(vaas_4, as.labels = list("Species", "Strain")))
stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)), is.numeric(x))
# Data frame
(x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  dataframe = TRUE))
stopifnot(is.data.frame(x), identical(dim(x), c(4L, 99L)))
# All parameters in a single data frame
x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
  as.labels = list("Species", "Strain"), dataframe = TRUE))
x <- do.call(rbind, x)
# Get discretized data
(x <- extract(vaas_4, subset = "disc", as.labels = list("Strain")))
stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)), is.logical(x))

# data-frame method
x <- data.frame(a = 1:26, b = letters, c = LETTERS)
(y <- extract(x, as.labels = "b", what = "integer", as.groups = "c"))
stopifnot(is.matrix(y), dim(y) == c(26, 1), rownames(y) == x$b)
stopifnot(identical(attr(y, "row.groups"), x$c))
}
\seealso{
  base::data.frame base::as.data.frame base::matrix
  base::as.matrix

  Other conversion-functions:
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{dplot}
\keyword{manip}

\docType{methods}
\alias{extract-methods}
\alias{extract,OPMS-method}
\alias{extract,data.frame-method}
\usage{
  \S4method{extract}{OPMS}(object, as.labels, subset = "A",
    ci = FALSE, trim = "full", dataframe = FALSE, as.groups = NULL, sep = " ",
    dups = "warn", exact = TRUE, strict = TRUE, full = TRUE, max = 10000L,
    ...) 

  \S4method{extract}{data.frame}(object, as.labels,
    as.groups = NULL, sep = " ", what = "numeric") 

}
