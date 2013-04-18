\name{collect_template}
\alias{collect_template}
\title{Collect metadata template}
\arguments{
  \item{object}{Character vector or \code{\link{OPM}} or
  \code{\link{OPMS}} object. The \code{\link{OPM}} and
  \code{\link{OPMS}} methods just collect a data frame from
  their input object.

  If a character vector is provided, it acts like the
  \code{names} argument of \code{\link{read_opm}}. That is,
  if it is a directory name, this is automatically scanned
  for all \acronym{CSV} and \acronym{YAML} files it
  contains (unless restrictions with patterns are made).
  One can also provide file names, or a mixture of file and
  directory names.  Regarding the supported input file
  formats, see \code{\link{read_single_opm}}.}

  \item{outfile}{Character scalar. Ignored if \code{NULL}
  or empty string.  Otherwise, interpreted as the name of a
  \acronym{CSV} output file. If metadata have already been
  collected in an older file with the same name, old
  metadata will be kept, identifiers for novel files will
  be included, their so far empty entries set to \code{NA}.
  Users who wish to keep the old version can use two
  distinct names for novel and old files; see
  \code{previous}.}

  \item{sep}{Character scalar. \acronym{CSV} field
  separator for \code{outfile}.}

  \item{previous}{Ignored if \code{NULL}. Otherwise passed
  to \code{\link{to_metadata}}. If it is a filename
  different from \code{outfile}, it is an error if the file
  does not exist.}

  \item{md.args}{List of other arguments passed to the
  \sQuote{to_metadata} methods.}

  \item{add.cols}{Optional character vector with the names
  of columns to be added to the result, or \code{NULL}. If
  not empty, names of columns to be added, initially
  containing \code{NA}.}

  \item{selection}{Elements to be extracted from the
  \acronym{CSV} comments contained in each file. Character
  vector passed to \code{\link{csv_data}}.}

  \item{include}{File inclusion pattern (or generator for a
  pattern). Passed to \code{\link{batch_collect}}.}

  \item{...}{Other arguments passed to
  \code{\link{batch_collect}}, or arguments passed from the
  \code{\link{OPMS}} to the \code{\link{OPM}} method.}

  \item{demo}{Logical scalar. Run in \sQuote{demo} mode?
  Also passed to \code{\link{batch_collect}}.}
}
\value{
  In the case of the character method, a data frame,
  returned invisibly if \code{outfile} is given; if
  \code{demo} is \code{TRUE}, a character vector of
  filenames instead, also returned invisibly. The
  \code{\link{OPM}} method returns a data frame with one
  row and the number of columns equal to the sum of the
  lengths of \code{selection} and \code{add.cols}. The
  \code{\link{OPM}} method returns such a data frame with
  one row per contained plate.
}
\description{
  Collect a metadata template from
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} comments assisting in later on adding
  metadata using \code{\link{include_metadata}}.
}
\details{
  The character method batch-collects templates for
  meta-information from files and optionally add these data
  as novel rows to previously collected data. It writes the
  collected template to a file for use with an external
  editor, and/or creates a data frame for editing the data
  directly in \R with the \code{edit} function.
}
\examples{
# Character method
test.files <- grep("Multiple|Ecoplate", opm_files("testdata"), invert = TRUE,
  value = TRUE, perl = TRUE)
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
data(vaas_1)
(x <- collect_template(vaas_1)) # => data frame, one row per plate
stopifnot(identical(dim(x), c(1L, 3L)))
(x <- collect_template(vaas_1, add.cols = c("A", "B")))
stopifnot(identical(dim(x), c(1L, 5L))) # => data frame with more columns
# see include_metadata() for how to use this to add metadata information

# OPMS method
data(vaas_4)
(x <- collect_template(vaas_4)) # => data frame, one row per plate
stopifnot(identical(dim(x), c(4L, 3L)))
(x <- collect_template(vaas_4, add.cols = c("A", "B")))
stopifnot(identical(dim(x), c(4L, 5L))) # => data frame with more columns
# again see include_metadata() for how to use this to add metadata
# information
}
\references{
  \url{http://www.biolog.com/}
}
\seealso{
  utils::edit utils::read.delim

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}}, \code{\link{explode_dir}},
  \code{\link{file_pattern}}, \code{\link{opm_files}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}
\keyword{attribute}

\docType{methods}
\alias{collect_template-methods}
\alias{collect_template,character-method}
\alias{collect_template,OPM-method}
\alias{collect_template,OPMS-method}
\usage{
  \S4method{collect_template}{character}(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL,
    include = list(), ..., demo = FALSE) 

  \S4method{collect_template}{OPM}(object,
    selection = opm_opt("csv.selection"), add.cols = NULL) 

  \S4method{collect_template}{OPMS}(object, ...) 

}
