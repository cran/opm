\name{collect_template}
\alias{collect_template}
\alias{to_metadata}
\title{Input metadata}
\arguments{
  \item{object}{Character vector or \code{\link{OPM}} or
  \code{\link{OPMS}} object. The \code{\link{OPM}} and
  \code{\link{OPMS}} methods collect a data frame from
  their input object.

  If a character vector is provided to
  \code{\link{collect_template}}, it acts like the
  \code{names} argument of \code{\link{read_opm}}. That is,
  if it is a directory name, this is automatically scanned
  for all \acronym{CSV} and \acronym{YAML} files it
  contains (unless restrictions with patterns are made).
  One can also provide file names, or a mixture of file and
  directory names. Regarding the supported input file
  formats, see \code{\link{read_single_opm}}.

  \code{to_metadata} needs the name of an input file
  (unnamed character scalar), or any object convertible to
  a data frame. Might also be \code{\link{WMD}} or
  \code{\link{OPMS}} object. If a named character vector
  with more than a single element, it is used as the first
  row of the resulting data frame. This behaviour is mainly
  intended for using this function after a call to the
  \code{\link{OPM}} method of \code{csv_data}.}

  \item{outfile}{Character scalar. Ignored if \code{NULL},
  empty or empty string. Otherwise, interpreted as the name
  of a \acronym{CSV} output file.  If metadata have already
  been collected in an older file with the same name, old
  metadata will be kept, identifiers for novel files will
  be included, their so far empty entries set to \code{NA}.
  Users who wish to keep the old version can use two
  distinct names for novel and old files; see
  \code{previous}.}

  \item{sep}{Character scalar. \acronym{CSV} field
  separator for \code{outfile} and for the input file of
  \code{to_metadata} (which ignores the argument unless
  \code{object} is interpreted as input file).}

  \item{previous}{Ignored if empty. Otherwise passed to
  \code{\link{to_metadata}}. If it is a file name different
  from \code{outfile}, it is an error if the file does not
  exist.}

  \item{md.args}{List of other arguments passed to the
  \sQuote{to_metadata} methods.}

  \item{add.cols}{Optional character vector with the names
  of columns to be added to the result, or \code{NULL}. If
  not empty, names of columns to be added, initially
  containing \code{NA}.}

  \item{selection}{Elements to be extracted from the
  \acronym{CSV} comments contained in each file. Character
  vector passed to \code{\link{csv_data}}.}

  \item{normalize}{Logical scalar also passed to
  \code{\link{csv_data}}.}

  \item{instrument}{Logical scalar or scalar convertible to
  integer, or empty.  Ignored if empty. If logical and
  \code{TRUE}, \code{\link{opm_opt}("machine.id")} is
  inserted as additional column.  Otherwise,
  \code{instrument} is used directly.}

  \item{include}{File inclusion pattern (or generator for a
  pattern). Passed to \code{\link{batch_collect}}.}

  \item{...}{Other arguments passed to
  \code{\link{batch_collect}}, or to \code{read.delim} or
  \code{as.data.frame}.}

  \item{demo}{Logical scalar. Run in \sQuote{demo} mode?
  Also passed to \code{\link{batch_collect}}. If
  \code{TRUE}, file input and output would be omitted and
  only the respective file names shown.}

  \item{stringsAsFactors}{Logical scalar passed to
  \code{as.data.frame}.}

  \item{optional}{Logical scalar passed to
  \code{as.data.frame} or used after negation as
  \sQuote{check.names} argument of \code{read.delim}.}

  \item{strip.white}{Logical scalar. For the file-name
  method, passed to \code{read.delim} (and set to
  \code{TRUE} if it is \code{NA}). It is often advisable to
  set this to \code{FALSE} if \acronym{CSV} input is done
  for a later call to \code{collect_template}. For a
  character vector not interpreted as file name, set to
  \code{FALSE} if \code{NA}.}
}
\value{
  \code{to_metadata} yields a data frame. Regarding
  \code{collect_template}, in the case of the character
  method, a data frame, returned invisibly if
  \code{outfile} is given; if \code{demo} is \code{TRUE}, a
  character vector of file names instead, returned
  invisibly. The \code{\link{OPM}} method returns a data
  frame with one row and the number of columns equal to the
  sum of the lengths of \code{selection} and
  \code{add.cols}. The \code{\link{OPM}} method returns
  such a data frame with one row per contained plate.
}
\description{
  Either collect a metadata template from
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} comments assisting in later on adding
  metadata using \code{\link{include_metadata}} or create a
  data frame holding potential \code{\link{OPM}} or
  \code{\link{OPMS}} object metadata.
}
\details{
  The character method batch-collects templates for
  meta-information from files and optionally add these data
  as novel rows to previously collected data. It writes the
  collected template to a file for use with an external
  editor, and/or creates a data frame for editing the data
  directly in \R with the \code{edit} function.

  The \code{to_metadata} character method reads metadata
  from an input file and is only a thin wrapper for
  \code{read.delim} but contains some useful adaptations
  (such as \strong{not} converting strings to factors, and
  not modifying column names). The default method reads
  metadata from an object convertible to a data frame and
  is only a thin wrapper of \code{as.data.frame} but
  contains the same useful adaptations as the file-name
  method.

  The \code{\link{WMD}} and \code{\link{OPMS}} methods
  create a data frame from the contained metadata, where
  necessary converting nested metadata entries to
  data-frame columns of mode \sQuote{list}. The number of
  rows of the resulting data frame corresponds to the
  length of \code{object}, the number of columns to the
  size of the set created from all valid names at the top
  level of the metadata entries.
}
\examples{
## collect_template()

# Character method
test.files <- opm_files("omnilog")
if (length(test.files) > 0) { # if the files are found

  # Without writing to a file
  (x <- collect_template(test.files))
  stopifnot(is.data.frame(x), identical(x[, "File"], test.files))
  # now proceed with e.g.
  # x <- edit(x)

  # Write to file
  outfile <- tempfile()
  stopifnot(!file.exists(outfile))
  # This results in a CSV outfile which could be used as a starting point
  # for including the metadata of interest together with the plate
  # identifiers in a single file. include_metadata() can then be used to
  # integrate the metadata in OPM, OPMA or OPMS objects.
  x <- collect_template(test.files, outfile = outfile)
  stopifnot(file.exists(outfile))
  unlink(outfile)
} else {
  warning("test files not found")
}

# OPM method
(x <- collect_template(vaas_1)) # => data frame, one row per plate
stopifnot(dim(x) == c(1, 3))
(x <- collect_template(vaas_1, instrument = TRUE))
stopifnot(dim(x) == c(1, 4))
(x <- collect_template(vaas_1, add.cols = c("A", "B")))
stopifnot(dim(x) == c(1, 5)) # => data frame with more columns
# see include_metadata() for how to use this to add metadata information

# OPMS method
(x <- collect_template(vaas_4)) # => data frame, one row per plate
stopifnot(identical(dim(x), c(4L, 3L)))
(x <- collect_template(vaas_4, add.cols = c("A", "B")))
stopifnot(identical(dim(x), c(4L, 5L))) # => data frame with more columns
# again see include_metadata() for how to use this to add metadata
# information

## to_metadata()

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
(x <- to_metadata(vaas_1)) # one row per OPM object
stopifnot(is.data.frame(x), nrow(x) == length(vaas_1), ncol(x) > 0)

# OPMS method
(x <- to_metadata(vaas_4)) # one row per OPM object
stopifnot(is.data.frame(x), nrow(x) == length(vaas_4), ncol(x) > 0)
copy <- vaas_4
metadata(copy) <- x
stopifnot(identical(copy, vaas_4))
# ... this works only in the special case of non-nested metadata that
# have the same set of entries in all OPMS elements
}
\references{
  \url{http://www.biolog.com/}
}
\seealso{
  base::default.stringsAsFactors base::as.data.frame

  utils::edit utils::read.delim

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_process}},
  \code{\link{clean_filenames}}, \code{\link{explode_dir}},
  \code{\link{file_pattern}}, \code{\link{glob_to_regex}},
      \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}
}
\keyword{IO}
\keyword{attribute}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{collect_template-methods}
\alias{collect_template,MOPMX-method}
\alias{collect_template,OPM-method}
\alias{collect_template,OPMS-method}
\alias{collect_template,character-method}
\alias{to_metadata-methods}
\alias{to_metadata,ANY-method}
\alias{to_metadata,OPMS-method}
\alias{to_metadata,WMD-method}
\alias{to_metadata,character-method}
\usage{
  \S4method{collect_template}{MOPMX}(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = FALSE,
    instrument = NULL, ..., demo = FALSE) 
  \S4method{collect_template}{OPM}(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = FALSE,
    instrument = NULL, ..., demo = FALSE) 
  \S4method{collect_template}{OPMS}(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = FALSE,
    instrument = NULL, ..., demo = FALSE) 
  \S4method{collect_template}{character}(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL, normalize = FALSE,
    instrument = NULL, include = list(), ..., demo = FALSE) 

  \S4method{to_metadata}{ANY}(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) 
  \S4method{to_metadata}{OPMS}(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) 
  \S4method{to_metadata}{WMD}(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = FALSE, ...) 
  \S4method{to_metadata}{character}(object, stringsAsFactors = FALSE,
    optional = TRUE, sep = "\t", strip.white = NA, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
