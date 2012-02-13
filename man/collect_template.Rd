\name{collect_template}
\alias{collect_template}
\title{Collect metadata template}
\arguments{
  \item{object}{Character vector. Acts like the
  \code{names} argument of \code{\link{read_opm}}. That is,
  if it a directory name, this is automatically scanned for
  all CSV and YAML files it contains (unless restrictions
  with patterns are made). One can also provide file names,
  or a mixture of file and directory names.}

  \item{outfile}{Character scalar. Ignored if \code{NULL}
  or empty string.  Otherwise, interpreted as the name of a
  CSV output file. If metadata have already been collected
  in an older file with the same name, old metadata will be
  kept, identifiers for novel files will be included, their
  so far empty entries set to NA. Users who wish to keep
  the old version will use two distinct names for novel and
  old files; see \code{previous}.}

  \item{sep}{Character scalar. CSV field separator for
  \code{outfile}.}

  \item{previous}{Ignored if \code{NULL}. Otherwise passed
  to \code{\link{to_metadata}} or
  \code{\link{to_metadata,character}}. If it is a filename
  different from \code{outfile}, it is an error if the file
  does not exist.}

  \item{md.args}{List of other arguments passed to the
  \sQuote{to_metadata} methods.}

  \item{add.cols}{Optional character vector with the names
  of columns to be added to the result. See the eponymous
  argument of \code{\link{collect_template,OPM}} for
  details.}

  \item{selection}{Elements to be extracted from the CSV
  comments contained in each file. See
  \code{\link{collect_template,OPM}} for details.}

  \item{include}{File inclusion pattern (or generator for a
  pattern). Passed to \code{\link{batch_collect}}.}

  \item{...}{Other arguments passed to
  \code{\link{batch_collect}}.}

  \item{demo}{Logical scalar. Run in \sQuote{demo} mode?
  Also passed to \code{\link{batch_collect}}.}
}
\value{
  Dataframe, returned invisibly if \code{outfile} is given;
  if \code{demo} is \code{TRUE}, a character vector of
  filenames instead, also returned invisibly.
}
\description{
  Batch-collect information from OmniLog(R) CSV comments
  for later on adding metadata. Optionally add these as
  novel rows to previously collected data. Write collected
  template to a file for use with an external editor,
  and/or create data frame for editing the data directly in
  R with the \code{edit} function.
}
\note{
  Regarding the CSV format, see the remark to
  \code{\link{read_single_opm}}.
}
\examples{
test.files <- grep("Multiple", opm_files("testdata"), invert = TRUE,
  value = TRUE, fixed = TRUE)
if (length(test.files) > 0) {

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
}
}
\references{
  \url{http://www.biolog.com/}
}
\seealso{
  edit read.delim

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}}, \code{\link{explode_dir}},
  \code{\link{file_pattern}}, \code{\link{opm_files}},
  \code{\link{phylo_data}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other metadata-functions:
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}},
  \code{\link{map_metadata,OPMS}},
  \code{\link{map_metadata,WMD+function}},
  \code{\link{metadata}}, \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{IO}

\docType{methods}
\alias{collect_template-methods}
\alias{collect_template,character-method}
\usage{
  \S4method{collect_template}{character}(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = c(SETUP, POS, FILE), add.cols = NULL, 
    include = list(), ..., demo = FALSE) 
}
