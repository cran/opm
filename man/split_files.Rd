\name{split_files}
\alias{split_files}
\title{Split files}
\usage{
  split_files(files, pattern, outdir = "", demo = FALSE,
    single = TRUE, wildcard = FALSE, invert = FALSE,
    format = "\%s-\%05i.\%s", compressed = TRUE, ...)
}
\arguments{
  \item{files}{Character vector or convertible to such.
  Names of the files to be split.}

  \item{pattern}{Regular expression or shell globbing
  pattern for matching the separator lines if \code{invert}
  is \code{FALSE} (the default) or matching the
  non-separator lines if otherwise. Conceptually each of
  the sections into which a file is split comprises a
  separator line followed by non-separator lines. That is,
  separator lines followed by another separator line are
  ignored. Non-separator lines not preceded by a separator
  line are treated as a section of their own, however.}

  \item{outdir}{Character scalar determining the output
  directory. If empty, each file's input directory is
  used.}

  \item{demo}{Logical scalar. Do not create files, just
  return the usual list containing all potentially created
  files. Note that in contrast to the \code{demo} arguments
  of other IO functions, this requires the input files to
  be read.}

  \item{single}{Logical scalar. If there is only one group
  per file, i.e. only one output file would result from the
  splitting, create this file anyway?  Such cases could be
  recognized by empty character vectors as values of the
  returned list (see below).}

  \item{wildcard}{Logical scalar. Is \code{pattern} a
  shell-globbing wildcard that first needs to be converted
  to a regular expression?}

  \item{invert}{Logical scalar. Invert pattern matching,
  i.e. treat all lines that \strong{not} match
  \code{pattern} as separators?}

  \item{format}{Character scalar determining the outfile
  name format. It is passed to \code{sprintf} and expects
  three placeholders: (i) the basename of the file; (ii)
  the index of the section; and (iii) the file extension.
  Getting \code{format} wrong might result in non-unique
  filenames and thus probably in overwritten files;
  accordingly, it should be used with care.}

  \item{compressed}{Logical scalar. Passed to
  \code{\link{file_pattern}}, but here only affects the way
  filenames are split in extensions and basenames.  Should
  only be set to \code{FALSE} if input files are not
  compressed (and have according file extensions).}

  \item{...}{Optional arguments passed to \code{grepl},
  which is used for matching the seperator lines. See also
  \code{invert} listed above.}
}
\value{
  List of character vectors, each vector containing the
  names of the newly generated files. The names of the list
  are the input filenames. The list is returned invisibly.
}
\description{
  Split files, i.e. subdivide each file into sections which
  are written individually to newly generated files.
  Sections are determined with patterns that match the
  start of a section.
}
\examples{
# Splitting an old-style CSV file containing several plates
(x <- grep("Multiple", opm_files("testdata"), value = TRUE, fixed = TRUE))
if (length(x) > 0) {
  outdir <- tempdir()
  # For old-style CSV, use either "^Data File" as pattern or "Data File*"
  # with 'wildcard' set to TRUE:
  (result <- split_files(x, pattern = "^Data File", outdir = outdir))
  stopifnot(is.list(result), length(result) == length(x))
  stopifnot(sapply(result, length) == 3)
  result <- unlist(result)
  stopifnot(file.exists(result))
  unlink(result) # tidy up
}
## One could split new-style CSV as follows (if x is a vector of filenames):
# split_files(x, pattern = '^"Data File",')
## note the correct setting of the quotes
## A pattern that covers both old and new-style CSV is:
# split_files(x, pattern = '^("Data File",|Data File)')
## This is used by the run_opm.R script
}
\seealso{
  split strsplit

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{utilities}

