\name{extract}
\alias{extract}
\title{Extract aggregated values}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{as.labels}{List. Metadata to be joined and used as
  row names (if \code{dataframe} is \code{FALSE}) or
  additional columns (if otherwise).  Ignored if
  \code{NULL}.}

  \item{subset}{Character vector. The parameter(s) to put
  in the matrix.}

  \item{ci}{Logical scalar. Also return the CIs?}

  \item{trim}{Character scalar. See
  \code{\link{aggregated}} for details.}

  \item{dataframe}{Logical scalar. Return dataframe or
  matrix?}

  \item{as.groups}{List. Metadata to be joined and used as
  \sQuote{row.groups} attribute of the output matrix. See
  \code{\link{heat_map}} for its usage.  Ignored if
  \code{NULL} and if \code{dataframe} is \code{FALSE}.}

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
}
\value{
  Numeric matrix or dataframe.
}
\description{
  Extract selected aggregated values into common matrix or
  dataframe.
}
\examples{
data(vaas_4)
# Matrix
(x <- extract(vaas_4, as.labels = list("Species", "Strain")))
stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)))
# Data frame
(x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  dataframe = TRUE))
stopifnot(is.data.frame(x), identical(dim(x), c(4L, 99L)))
# All parameters in a single dataframe
x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
  as.labels = list("Species", "Strain"), dataframe = TRUE))
x <- do.call(rbind, x)
}
\seealso{
  base::data.frame base::as.data.frame base::matrix
  base::as.matrix

  Other conversion-functions:
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{to_yaml}}
}
\keyword{dplot}
\keyword{manip}

\docType{methods}
\alias{extract-methods}
\alias{extract,OPMS-method}
\usage{
  \S4method{extract}{OPMS}(object, as.labels, subset = "A", 
    ci = FALSE, trim = "full", dataframe = FALSE, as.groups = NULL, sep = " ",
    dups = "warn", exact = TRUE, strict = TRUE, full = TRUE, max = 10000L, 
    ...) 

}
