\name{to_metadata}
\alias{to_metadata}
\title{Input metadata}
\arguments{
  \item{object}{Name of input file (character scalar), or
  any object convertible to a data frame. Might also be
  \code{\link{WMD}} or \code{\link{OPMS}} object.}

  \item{stringsAsFactors}{Logical scalar passed to
  \code{as.data.frame}.}

  \item{optional}{Logical scalar passed to
  \code{as.data.frame} or \code{read.delim}.}

  \item{sep}{Character scalar. Field separator in input
  file. This and the following parameters are passed to
  \code{read.delim}.}

  \item{check.names}{Logical scalar.}

  \item{strip.white}{Logical scalar.}

  \item{...}{Optional other arguments for \code{read.delim}
  or \code{as.data.frame}.}
}
\value{
  Data frame.
}
\description{
  Create data frame holding potential \code{\link{OPM}} or
  \code{\link{OPMS}} object metadata.
}
\details{
  The character method reads metadata from an input file
  and is only a thin wrapper for \code{read.delim} but
  contains some useful adaptations (such as \strong{not}
  converting strings to factors, and not modifying column
  names). The default method reads metadata from an object
  convertible to a data frame and is only a thin wrapper of
  \code{as.data.frame} but contains the same useful
  adaptations as the filename method.

  The \code{\link{WMD}} and \code{\link{OPMS}} methods
  create a data frame from the contained metadata, where
  necessary converting nested metadata entries to
  data-frame columns of mode \sQuote{list}. The number of
  rows of the resuling data frame corresponds to the length
  of \code{object}, the number of columns to the size of
  the set created from all valid names at the top level of
  the metadata entries.
}
\examples{
# Character method
(x <- to_metadata(list(a = 7:8, `b c` = letters[1:2])))
tmpfile <- tempfile()
write.table(x, tmpfile, row.names = FALSE, sep = "\t")
(x1 <- read.delim(tmpfile)) # comparison with base R function
(x2 <- to_metadata(tmpfile))
stopifnot(identical(names(x2), names(x)), !identical(names(x1), names(x)))

# Default method
x <- list(a = 7:8, `b c` = letters[1:2])
(x1 <- as.data.frame(x))
(x2 <- to_metadata(x))
stopifnot(!identical(names(x), names(x1)), identical(names(x), names(x2)))

# WMD method
data(vaas_1)
(x <- to_metadata(vaas_1)) # one row per OPM object
stopifnot(is.data.frame(x), nrow(x) == length(vaas_1), ncol(x) > 0)

# OPMS method
data(vaas_4)
(x <- to_metadata(vaas_4)) # one row per OPM object
stopifnot(is.data.frame(x), nrow(x) == length(vaas_4), ncol(x) > 0)
copy <- vaas_4
metadata(copy) <- x
stopifnot(identical(copy, vaas_4))
# ... this works only in the special case of non-nested metadata that
# have the same set of entries in all OPMS elements
}
\seealso{
  base::default.stringsAsFactors utils::read.delim
  base::as.data.frame

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}}
}
\keyword{IO}
\keyword{manip}

\docType{methods}
\alias{to_metadata-methods}
\alias{to_metadata,character-method}
\alias{to_metadata,ANY-method}
\alias{to_metadata,WMD-method}
\alias{to_metadata,OPMS-method}
\usage{
  \S4method{to_metadata}{character}(object, sep = "\t",
    check.names = !optional, strip.white = TRUE, stringsAsFactors = FALSE,
    optional = TRUE, ...) 

  \S4method{to_metadata}{ANY}(object, stringsAsFactors = FALSE,
    optional = TRUE, ...) 

  \S4method{to_metadata}{WMD}(object, stringsAsFactors = FALSE,
    optional = TRUE, ...) 

  \S4method{to_metadata}{OPMS}(object, stringsAsFactors = FALSE,
    optional = TRUE, ...) 

}
