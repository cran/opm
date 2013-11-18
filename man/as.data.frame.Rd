\name{as.data.frame}
\alias{as.data.frame}
\alias{flatten}
\title{Create data frame}
\arguments{
  \item{x}{Object of class \code{\link{OPM}}, its child
  classes, or \code{\link{OPMS}}. If an \code{\link{OPMS}}
  object, its elements must either all be \code{\link{OPM}}
  or all be \code{\link{OPMA}} or all be \code{\link{OPMD}}
  objects. There are also methods for some of the objects
  created by \code{\link{substrate_info}}.}

  \item{row.names}{Optional vector for use as row names of
  the resulting data frame. Here, it is not recommended to
  try to set row names explicitly.}

  \item{optional}{Logical scalar passed to the list and
  matrix methods of \code{as.data.frame}.}

  \item{sep}{Character scalar used as word separator in
  column names.}

  \item{stringsAsFactors}{Logical scalar passed to the list
  and matrix methods of \code{as.data.frame}.}

  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object (or list).}

  \item{include}{\code{NULL}, character vector, list or
  formula. If not empty, include this meta-information in
  the data frame, replicated in each row.  Otherwise it
  converted to a list and passed to \code{\link{metadata}}.
  See there for details.}

  \item{fixed}{\code{NULL} or list. If not \code{NULL},
  include these items in the data frame, replicated in each
  row.}

  \item{factors}{Logical scalar. See the
  \code{stringsAsFactors} argument of \code{data.frame} and
  \code{as.data.frame} from the \pkg{base} package.}

  \item{exact}{Logical scalar. Passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar. Passed to
  \code{\link{metadata}}.}

  \item{full}{Logical scalar. Replace well coordinates by
  full names?}

  \item{numbers}{Logical scalar. Use numbers instead of
  well names? This is \emph{not} recommended for must
  usages.}

  \item{...}{Optional other arguments passed to
  \code{\link{wells}}, or from the \code{\link{OPMS}} to
  the \code{\link{OPM}} method, or to the list and matrix
  methods of \code{as.data.frame}.}
}
\value{
  These \code{as.data.frame} methods create a data frame
  with one row for each combination of well and plate.

  In the data frame returned by \code{flatten}, column
  names are unchecked (not converted to variable names).
  The three last columns are coding for time, well and
  value, with the exact spelling of the column names given
  by \code{\link{param_names}}.

  The \code{\link{OPMS}} method yields an additional column
  for the plate, the exact spelling of its name also being
  available via \code{\link{param_names}}. This column
  contains the position of each plate within \code{object}.
}
\description{
  These \code{as.data.frame} methods create a data frame
  from aggregated and discretised values in a manner
  distinct from \code{\link{extract}}. \code{flatten}
  converts into a \sQuote{flat} data frame, including all
  measurements in a single column (suitable, e.g., for
  \pkg{lattice}).
}
\details{
  These \code{as.data.frame} methods for \code{\link{OPMX}}
  objects are mainly intended to produce objects that can
  easily be written to \acronym{CSV} files, for instance
  using \code{write.table} from the \pkg{utils} package.
  There are no \pkg{opm} methods other than
  \code{\link{batch_opm}} (which can write such files) that
  make use of the created kind of objects.

  The following entries are contained in the generated data
  frame: \itemize{ \item The \code{\link{csv_data}} entries
  that identify the plate.  \item The names of the wells.
  \item For \code{\link{OPMA}} objects (and
  \code{\link{OPMS}} objects that contain them), the
  aggregated data (curve parameters), one column for each
  point estimate, upper and lower confidence interval of
  each parameter.  \item For \code{\link{OPMA}} objects
  (and \code{\link{OPMS}} objects that contain them), the
  used aggregation settings, one column per entry, except
  for the \sQuote{options} entry (which is not a scalar).
  \item For \code{\link{OPMD}} objects (and
  \code{\link{OPMS}} objects that contain them), one column
  with the discretised data.  \item For \code{\link{OPMD}}
  objects (and \code{\link{OPMS}} objects that contain
  them), the used discretisation settings, one column per
  entry, except for the \sQuote{options} entry (which is
  not a scalar).  }

  The limits of using \acronym{CSV} as output format
  already show up in this list, and in general we recommend
  to generate \acronym{YAML} or \acronym{JSON} output
  instead.

  For the methods of the other classes, see
  \code{\link{substrate_info}}.
}
\examples{
## OPMD method of as.data.frame()
summary(x <- as.data.frame(vaas_1))
stopifnot(is.data.frame(x), nrow(x) == 96)

## OPMS method of as.data.frame()
summary(x <- as.data.frame(vaas_4[, , 1:10]))
stopifnot(is.data.frame(x), nrow(x) == 10 * 4)

## OPM method of flatten()
# distinct numbers of columns due to distinct selection settings
head(x <- flatten(vaas_1))
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 3L)))
head(x <- flatten(vaas_1, fixed = "TEST", include = "Strain"))
stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 5L)))

## OPMS method of flatten()
# distinct numbers of columns due to distinct selection settings
head(x <- flatten(vaas_4[, , 1:10]))
stopifnot(is.data.frame(x), identical(dim(x), c(15360L, 4L)))
head(x <- flatten(vaas_4[, , 1:10], fixed = "TEST", include = ~ Strain))
stopifnot(is.data.frame(x), identical(dim(x), c(15360L, 6L)))
}
\seealso{
  utils::write.table stats::reshape pkgutils::flatten

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{to_yaml}}, \code{\link{unique}}
}
\keyword{dplot}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{as.data.frame-methods}
\alias{as.data.frame,OPM-method}
\alias{as.data.frame,OPMA-method}
\alias{as.data.frame,OPMD-method}
\alias{as.data.frame,OPMS-method}
\alias{as.data.frame,kegg_compound-method}
\alias{as.data.frame,kegg_compounds-method}
\alias{flatten-methods}
\alias{flatten,OPM-method}
\alias{flatten,OPMS-method}
\usage{
  \S4method{as.data.frame}{OPM}(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) 
  \S4method{as.data.frame}{OPMA}(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) 
  \S4method{as.data.frame}{OPMD}(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) 
  \S4method{as.data.frame}{OPMS}(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) 
  \S4method{as.data.frame}{kegg_compound}(x, row.names = NULL,
    optional = TRUE, ..., stringsAsFactors = FALSE) 
  \S4method{as.data.frame}{kegg_compounds}(x, row.names = NULL,
    optional = TRUE, ..., stringsAsFactors = FALSE) 

  \S4method{flatten}{OPM}(object, include = NULL, fixed = NULL,
    factors = TRUE, exact = TRUE, strict = TRUE, full = TRUE,
    numbers = FALSE, ...) 
  \S4method{flatten}{OPMS}(object, include = NULL, fixed = list(),
    ...) 
}
%% END INSERTION BY repair_S4_docu.rb
