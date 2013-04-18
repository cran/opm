\name{extract}
\alias{extract}
\title{Extract aggregated values}
\arguments{
  \item{object}{\code{\link{OPMS}} object or data frame
  with one column named as indicated by \code{split.at}
  (default given by \code{\link{param_names}("split.at")}),
  columns with factor variables before that column and
  columns with numeric vectors after that column.}

  \item{as.labels}{List, character vector or formula
  indicating the metadata to be joined and used as row
  names (if \code{dataframe} is \code{FALSE}) or additional
  columns (if otherwise). Ignored if \code{NULL}.}

  \item{subset}{Character vector. The parameter(s) to put
  in the matrix. If it is \sQuote{disc}, discretized data
  are returned, and \code{ci} is ignored.}

  \item{ci}{Logical scalar. Also return the confidence
  intervals?}

  \item{trim}{Character scalar. See
  \code{\link{aggregated}} for details.}

  \item{dataframe}{Logical scalar. Return data frame or
  matrix?}

  \item{as.groups}{For the \code{\link{OPMS}} method, a
  list, character vector or formula indicating the metadata
  to be joined and used as \sQuote{row.groups} attribute of
  the output matrix. See \code{\link{heat_map}} for its
  usage. Ignored if \code{NULL} and if \code{dataframe} is
  \code{FALSE}.

  For the data-frame method, a logical, character or
  numeric vector indicating according to which columns
  (before the \code{split.at} column) the data should be
  aggregated by calculating means and confidence intervals.
  If \code{FALSE}, such an aggregation does not take place.
  If \code{TRUE}, all those columns are used for grouping.}

  \item{sep}{Character scalar. See
  \code{\link{extract_columns}}.}

  \item{dups}{Character scalar. See
  \code{\link{extract_columns}}. For the data-frame method,
  a character scalar defining the action to conduct if
  \code{as.groups} contains duplicates.}

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

  \item{norm.per}{Character scalar indicating the presence
  and direction of a normalization step.  \describe{
  \item{none}{No normalization.} \item{row}{Normalization
  per row. By default, this would subtract the mean of each
  plate from each of its values (over all wells of that
  plate).} \item{column}{Normalization per column. By
  default, this would subtract the mean of each well from
  each of its values (over all plates in which this well is
  present).} } This step can further by modified by the
  next three arguments.}

  \item{norm.by}{Vector indicating which wells (columns) or
  plates (rows) are used to calculate means used for the
  normalization. By default, the mean is calculated over
  all rows or columns if normalization is requested using
  \code{norm.per}. But if \code{direct} is \code{TRUE},
  \code{norm.by} is directly interpreted as numeric vector
  used for normalization.}

  \item{direct}{Logical scalar indicating how to use
  \code{norm.by}. See there for details.}

  \item{subtract}{Logical scalar indicating whether
  normalization (if any) is done by subtracting or
  dividing.}

  \item{split.at}{Character vector defining alternative
  names of the column at which the data frame shall be
  divided. Exactly one must match.}
}
\value{
  Numeric matrix or data frame; always a data frame for the
  data-frame method with the same column structure as
  \code{object} and, if grouping was used, a triplet
  structure of the rows, as indicated in the new
  \code{split.at} column: (i) group mean, (ii) lower and
  (iii) upper boundary of the group confidence interval.
  The data could then be visualized using
  \code{\link{ci_plot}}. See the examples.
}
\description{
  Extract selected aggregated and/or discretized values
  into common matrix or data frame. The data-frame method
  conducts normalisation and/or computes normalized
  point-estimates and respective confidence intervals for
  user-defined experimental groups. It is mainly a helper
  function for \code{\link{ci_plot}}.
}
\examples{
## 'OPMS' method
data(vaas_4)
opm_opt("curve.param") # default parameter

# generate matrix (containing the parameter given above)
(x <- extract(vaas_4, as.labels = list("Species", "Strain")))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)), is.numeric(x))
# Using a formula also works
(y <- extract(vaas_4, as.labels = ~ Species + Strain))[, 1:3]
stopifnot(identical(x, y))

# generate data frame
(x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  dataframe = TRUE))[, 1:3]
stopifnot(is.data.frame(x), identical(dim(x), c(4L, 99L)))

# put all parameters in a single data frame
x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
  as.labels = list("Species", "Strain"), dataframe = TRUE))
x <- do.call(rbind, x)

# get discretized data
(x <- extract(vaas_4, subset = "disc", as.labels = list("Strain")))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)), is.logical(x))

## data-frame method

# extract data from OPMS-object as primary data frame
# second call to extract() then applied to this one
(x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  dataframe = TRUE))[, 1:3]

# no normalisation, but grouping for 'Species'
y <- extract(x, as.groups = "Species",  norm.per = "none")
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 350, y = 1)

# normalisation by plate means
y <- extract(x, as.groups = "Species",  norm.per = "row")
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 130, y = 1)

# normalisation by well means
y <- extract(x, as.groups = "Species",  norm.per = "column")
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 20, y = 1)

# normalisation by subtraction of the well means of well A10 only
y <- extract(x, as.groups = "Species",  norm.per = "row", norm.by = 10,
  subtract = TRUE)
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 0, y = 0)
}
\author{
  Lea A.I. Vaas, Markus Goeker
}
\seealso{
  \code{\link{aggregated}} for the extraction of aggregated
  values from a single \code{OPMA} objects.

  boot::norm base::data.frame base::as.data.frame
  base::matrix base::as.matrix

  Other conversion-functions:
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{dplot}
\keyword{htest}
\keyword{manip}

\docType{methods}
\alias{extract-methods}
\alias{extract,OPMS-method}
\alias{extract,data.frame-method}
\usage{
  \S4method{extract}{OPMS}(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, sep = " ", dups = "warn",
    exact = TRUE, strict = TRUE, full = TRUE, max = 10000L, ...) 

  \S4method{extract}{data.frame}(object, as.groups = TRUE,
    norm.per = c("row", "column", "none"), norm.by = TRUE, subtract = TRUE,
    direct = inherits(norm.by, "AsIs"), dups = c("warn", "error", "ignore"),
    split.at = param_names("split.at")) 

}
