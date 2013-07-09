\name{as.data.frame}
\alias{as.data.frame}
\title{Create data frame}
\arguments{
  \item{x}{Object of class \code{\link{OPM}}, its child
  classes, or \code{\link{OPMS}}. If an \code{\link{OPMS}}
  object, its elements must either all be \code{\link{OPM}}
  or all be \code{\link{OPMA}} or all be \code{\link{OPMD}}
  objects.}

  \item{row.names}{Optional vector for use as row names of
  the resulting data frame. Here, it is not recommended to
  try to set row names explicitly.}

  \item{optional}{Logical scalar passed to the list and
  matrix methods of \code{as.data.frame}.}

  \item{sep}{Character scalar used as word separator in
  column names.}

  \item{...}{Optional arguments passed to the list and
  matrix methods of \code{as.data.frame}.}

  \item{stringsAsFactors}{Logical scalar passed to these
  methods.}
}
\value{
  Data frame with one row for each combination of well and
  plate.
}
\description{
  These methods create a data frame from aggregated and
  discretized values in a manner distinct from
  \code{\link{extract}}.
}
\details{
  This function is mainly intended to produce objects that
  can easily be written to \acronym{CSV} files, for
  instance using \code{write.table} from the \pkg{utils}
  package. There are no \pkg{opm} methods other than
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
  with the discretized data.  \item For \code{\link{OPMD}}
  objects (and \code{\link{OPMS}} objects that contain
  them), the used discretization settings, one column per
  entry, except for the \sQuote{options} entry (which is
  not a scalar).  }

  The limits of using \acronym{CSV} as output format
  already show up in this list, and in general we recommend
  to generate \acronym{YAML} or \acronym{JSON} output
  instead.
}
\examples{
## OPMD method
data(vaas_1)
summary(x <- as.data.frame(vaas_1))
stopifnot(is.data.frame(x), nrow(x) == 96)

## OPMS method
data(vaas_4)
summary(x <- as.data.frame(vaas_4))
stopifnot(is.data.frame(x), nrow(x) == 96 * 4)
}
\seealso{
  utils::write.table

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{manip}

\docType{methods}
\alias{as.data.frame-methods}
\alias{as.data.frame,OPM-method}
\alias{as.data.frame,OPMA-method}
\alias{as.data.frame,OPMD-method}
\alias{as.data.frame,OPMS-method}
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

}
