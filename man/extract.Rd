\name{extract}
\alias{extract}
\alias{extract_columns}
\title{Extract aggregated values and/or metadata}
\arguments{
  \item{object}{\code{\link{OPMS}} object,
  \code{\link{MOPMX}} object or data frame, for
  \code{extract} with one column named as indicated by
  \code{split.at} (default given by
  \code{\link{param_names}("split.at")}), columns with
  factor variables before that column and columns with
  numeric vectors after that column. For
  \code{extract_columns} optionally an \code{\link{OPM}}
  object.}

  \item{as.labels}{List, character vector or formula
  indicating the metadata to be joined and used as row
  names (if \code{dataframe} is \code{FALSE}) or additional
  columns (if otherwise). Ignored if \code{NULL}.

  If a \code{as.labels} is a formula and \code{dataframe}
  is \code{TRUE}, the pseudo-function \code{J} within the
  formula can be used to trigger combination of factors
  immediately after selecting them as data-frame columns,
  much like \code{as.groups}.}

  \item{subset}{Character vector. The parameter(s) to put
  in the matrix. One of the values of
  \code{\link{param_names}()}. Alternatively, if it is
  \code{\link{param_names}("disc.name")}, discretised data
  are returned, and \code{ci} is ignored..}

  \item{ci}{Logical scalar. Also return the confidence
  intervals?}

  \item{trim}{Character scalar. See
  \code{\link{aggregated}} for details.}

  \item{dataframe}{Logical scalar. Return data frame or
  matrix?}

  \item{as.groups}{For the \code{\link{OPMS}} method, a
  list, character vector or formula indicating the metadata
  to be joined and either used as \sQuote{row.groups}
  attribute of the output matrix or as additional columns
  of the output data frame. See \code{\link{heat_map}} for
  its usage. Ignored if empty.

  If a \code{as.groups} is a formula and \code{dataframe}
  is \code{TRUE}, the pseudo-function \code{J} within the
  formula can be used to trigger combination of factors
  immediately after selecting them as data-frame columns,
  much like \code{as.labels}.

  If \code{as.groups} is a logical scalar, \code{TRUE}
  yields a trivial group that contains all elements,
  \code{FALSE} yields one group per element, and \code{NA}
  yields an error. The column name in which this factor is
  placed if \code{dataframe} is \code{TRUE} is determined
  using \code{opm_opt("group.name")}.

  For the data-frame method, a logical, character or
  numeric vector indicating according to which columns
  (before the \code{split.at} column) the data should be
  aggregated by calculating means and confidence intervals.
  If \code{FALSE}, such an aggregation does not take place.
  If \code{TRUE}, all those columns are used for grouping.}

  \item{sep}{Character scalar. Used as separator between
  the distinct metadata entries if these are to be pasted
  together. \code{extract_columns} ignores this unless
  \code{join} is \code{TRUE}. The data-frame method always
  joins the data unless \code{what} is a list.}

  \item{dups}{Character scalar specifying what to do in the
  case of duplicate labels: either \sQuote{warn},
  \sQuote{error} or \sQuote{ignore}. Ignored unless
  \code{join} is \code{TRUE} and if \code{object} is an
  \code{\link{OPM}} object. For the data-frame method of
  \code{extract}, a character scalar defining the action to
  conduct if \code{as.groups} contains duplicates.}

  \item{exact}{Logical scalar. Passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar. Also passed to
  \code{\link{metadata}}.}

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
  and direction of a normalisation step.  \describe{
  \item{none}{No normalisation.} \item{row}{Normalisation
  per row. By default, this would subtract the mean of each
  plate from each of its values (over all wells of that
  plate).} \item{column}{Normalisation per column. By
  default, this would subtract the mean of each well from
  each of its values (over all plates in which this well is
  present).} } This step can further by modified by the
  next three arguments.}

  \item{norm.by}{Vector indicating which wells (columns) or
  plates (rows) are used to calculate means used for the
  normalisation. By default, the mean is calculated over
  all rows or columns if normalisation is requested using
  \code{norm.per}. But if \code{direct} is \code{TRUE},
  \code{norm.by} is directly interpreted as numeric vector
  used for normalisation.}

  \item{direct}{Logical scalar. For \code{extract},
  indicating how to use \code{norm.by}. See there for
  details. For \code{extract_columns}, indicating whether
  to extract column names directly, or search for columns
  of one to several given classes.}

  \item{subtract}{Logical scalar indicating whether
  normalisation (if any) is done by subtracting or
  dividing.}

  \item{split.at}{Character vector defining alternative
  names of the column at which the data frame shall be
  divided. Exactly one must match.}

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
  to extract, or their indexes, as vector, if \code{direct}
  is \code{TRUE}. Alternatively, the name of the class to
  extract from the data frame to form the matrix values.

  In the \sQuote{direct} mode, \code{what} can also be a
  named list of vectors used for indexing. In that case a
  data frame is returned that contains the columns from
  \code{object} together with new columns that result from
  pasting the selected columns together. If \code{what} is
  named, its names are used as the new column names.
  Otherwise each name is created by joining the respective
  value within \code{what} with the \code{"comb.key.join"}
  entry of \code{\link{opm_opt}} as separator.}

  \item{join}{Logical scalar. Join each row together to
  yield a character vector? Otherwise it is just attempted
  to construct a data frame.}

  \item{factors}{Logical scalar determining whether strings
  should be converted to factors. Note that this would only
  affect newly created data-frame columns.}
}
\value{
  Numeric matrix or data frame from \code{extract}; always
  a data frame for the data-frame method with the same
  column structure as \code{object} and, if grouping was
  used, a triplet structure of the rows, as indicated in
  the new \code{split.at} column: (i) group mean, (ii)
  lower and (iii) upper boundary of the group confidence
  interval. The data could then be visualised using
  \code{\link{ci_plot}}. See the examples.

  For the \code{OPMS} method of \code{extract_columns}, a
  data frame or character vector, depending on the
  \code{join} argument. The data-frame method of
  \code{extract_columns} returns a character vector or a
  data frame, too, but depending on the \code{what}
  argument.
}
\description{
  Extract selected aggregated and/or discretised values
  into common matrix or data frame. The \code{extract}
  data-frame method conducts normalisation and/or computes
  normalised point-estimates and respective confidence
  intervals for user-defined experimental groups. It is
  mainly a helper function for \code{\link{ci_plot}}.
  \code{extract_columns} extracts only selected metadata
  entries for use as additional columns in a data frame or
  (after joining) as character vector with labels.
}
\details{
  \code{extract_columns} is not normally directly called by
  an \pkg{opm} user because \code{extract} is available,
  which uses this function, but can be used for testing the
  applied metadata selections beforehand.

  The \code{extract_columns} data-frame method is partially
  trivial (extract the selected columns and join them to
  form a character vector or new data-frame columns),
  partially more useful (extract columns with data of a
  specified class).

  Not all \code{\link{MOPMX}} objects are suitable for
  \code{extract}. The call will be successful if only
  \code{\link{OPMS}} objects are contained, i.e.
  \code{\link{OPM}} objects are forbidden. But even if
  successful it might result in \code{NA} values within the
  resulting matrix or data frame.  This may cause methods
  that call \code{extract} to fail. \code{NA} values will
  not occur if the set of row names created using
  \code{as.labels} is equal between the distinct elements
  of \code{object}. The also holds if \code{dataframe} is
  \code{TRUE}, even though in that case row names are only
  temporarily created.

  Duplicate combinations of row and columns names currently
  cause the \code{\link{MOPMX}} methods to skip all of them
  except the last one if \code{dataframe} is \code{FALSE}.
  This should mainly effect substrates that occur in plates
  of distinct plate types.

  Similarly, duplicate row names will cause the skipping of
  all but the last one. This can be circumvented by using
  an \code{as.labels} argument that yields unique row
  names. If \code{as.labels} is empty, the
  \code{\link{MOPMX}} method of \code{extract} will create
  potentially unique row names from the names if these are
  present but from the plate types if the \sQuote{names}
  attribute is \code{NULL}. This will not be done, and rows
  will neither be skipped nor reordered, if
  \code{dataframe} is \code{TRUE}.

  Otherwise row names and names of substrate columns will
  be reordered (sorted). The created \sQuote{row.groups}
  attribute, if any, will be adapted accordingly. If
  \code{dataframe} is \code{TRUE}, the placement of the
  columns created by \code{as.groups} will also be as
  usual, but duplicates, if any, will be removed.
}
\examples{
## 'OPMS' method
opm_opt("curve.param") # default parameter

# generate matrix (containing the parameter given above)
(x <- extract(vaas_4, as.labels = list("Species", "Strain")))[, 1:3]
stopifnot(is.matrix(x), dim(x) == c(4, 96), is.numeric(x))
# using a formula also works
(y <- extract(vaas_4, as.labels = ~ Species + Strain))[, 1:3]
stopifnot(identical(x, y))

# generate data frame
(x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  dataframe = TRUE))[, 1:3]
stopifnot(is.data.frame(x), dim(x) == c(4, 99))
# using a formula
(y <- extract(vaas_4, as.labels = ~ Species + Strain,
  dataframe = TRUE))[, 1:3]
stopifnot(identical(x, y))
# using a formula, with joining into new columns
(y <- extract(vaas_4, as.labels = ~ J(Species + Strain),
  dataframe = TRUE))[, 1:3]
stopifnot(identical(x, y[, -3]))

# put all parameters in a single data frame
x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
  as.labels = list("Species", "Strain"), dataframe = TRUE))
x <- do.call(rbind, x)

# get discretised data
(x <- extract(vaas_4, subset = param_names("disc.name"),
  as.labels = list("Strain")))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)), is.logical(x))

## data-frame method

# extract data from OPMS-object as primary data frame
# second call to extract() then applied to this one
(x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  dataframe = TRUE))[, 1:3]

# no normalisation, but grouping for 'Species'
y <- extract(x, as.groups = "Species", norm.per = "none")
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 350, y = 1)

# normalisation by plate means
y <- extract(x, as.groups = "Species", norm.per = "row")
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 130, y = 1)

# normalisation by well means
y <- extract(x, as.groups = "Species", norm.per = "column")
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 20, y = 1)

# normalisation by subtraction of the well means of well A10 only
y <- extract(x, as.groups = "Species", norm.per = "row", norm.by = 10,
  subtract = TRUE)
# plotting using ci_plot()
ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 0, y = 0)

## extract_columns()

# 'OPMS' method

# Create data frame
(x <- extract_columns(vaas_4, what = list("Species", "Strain")))
stopifnot(is.data.frame(x), dim(x) == c(4, 2))
(y <- extract_columns(vaas_4, what = ~ Species + Strain))
stopifnot(identical(x, y)) # same result using a formula
(y <- extract_columns(vaas_4, what = ~ J(Species + Strain)))
stopifnot(is.data.frame(y), dim(y) == c(4, 3)) # additional column created
stopifnot(identical(x, y[, -3]))
(x <- extract_columns(vaas_4, what = TRUE)) # use logical scalar
stopifnot(is.data.frame(x), dim(x) == c(4, 1))
(y <- extract_columns(vaas_4, what = FALSE))
stopifnot(is.data.frame(y), dim(y) == c(4, 1), !all(y[, 1] == x[, 1]))

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
\author{
  Lea A.I. Vaas, Markus Goeker
}
\seealso{
  \code{\link{aggregated}} for the extraction of aggregated
  values from a single \code{OPMA} objects.

  boot::norm base::data.frame base::as.data.frame
  base::matrix base::as.matrix base::cbind

  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{flatten}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{opmx}},
  \code{\link{plates}}, \code{\link{rep}},
  \code{\link{rev}}, \code{\link{sort}},
  \code{\link{split}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{dplot}
\keyword{htest}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{extract-methods}
\alias{extract,MOPMX-method}
\alias{extract,OPMS-method}
\alias{extract,data.frame-method}
\alias{extract_columns-methods}
\alias{extract_columns,WMD-method}
\alias{extract_columns,WMDS-method}
\alias{extract_columns,data.frame-method}
\usage{
  \S4method{extract}{MOPMX}(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, ...) 
  \S4method{extract}{OPMS}(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, sep = " ", dups = "warn",
    exact = TRUE, strict = TRUE, full = TRUE, max = 10000L, ...) 
  \S4method{extract}{data.frame}(object, as.groups = TRUE,
    norm.per = c("row", "column", "none"), norm.by = TRUE, subtract = TRUE,
    direct = inherits(norm.by, "AsIs"), dups = c("warn", "error", "ignore"),
    split.at = param_names("split.at")) 

  \S4method{extract_columns}{WMD}(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) 
  \S4method{extract_columns}{WMDS}(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) 
  \S4method{extract_columns}{data.frame}(object, what,
    as.labels = NULL, as.groups = NULL, sep = opm_opt("comb.value.join"),
    factors = is.list(what), direct = is.list(what) || inherits(what, "AsIs")) 
}
%% END INSERTION BY repair_S4_docu.rb
