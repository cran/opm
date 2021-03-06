\name{split_files}
\alias{clean_filenames}
\alias{split_files}
\title{Manipulate files}
\usage{
  split_files(files, pattern, outdir = "", demo = FALSE,
    single = TRUE, wildcard = FALSE, invert = FALSE,
    include = TRUE, format = opm_opt("file.split.tmpl"),
    compressed = TRUE, ...)

  clean_filenames(x, overwrite = FALSE, demo = FALSE,
    empty.tmpl = "__EMPTY__\%05i__")
}
\arguments{
  \item{files}{Character vector or convertible to such.
  Names of the files to be split.}

  \item{pattern}{Regular expression or shell globbing
  pattern for matching the separator lines if \code{invert}
  is \code{FALSE} (the default) or matching the
  non-separator lines if otherwise.

  Conceptually each of the sections into which a file is
  split comprises a separator line followed by
  non-separator lines. That is, separator lines followed by
  another separator line are ignored. Non-separator lines
  not preceded by a separator line are treated as a section
  of their own, however.}

  \item{outdir}{Character scalar determining the output
  directory. If empty, or containing empty strings, each
  file's input directory is used.}

  \item{demo}{Logical scalar. For \code{split_files}, do
  not create files, just return the usual list containing
  all potentially created files. Note that in contrast to
  the \code{demo} arguments of other IO functions, this
  requires the input files to be read.

  For \code{clean_filenames}, do not rename files but just
  return the usual result indicating the renaming actions
  that would be attempted? (Note that this does not
  indicate whether the renaming would also by successful.)}

  \item{single}{Logical scalar. If there is only one group
  per file, i.e. only one output file would result from the
  splitting, create this file anyway?  Such cases could be
  recognised by empty character vectors as values of the
  returned list (see below).}

  \item{wildcard}{Logical scalar. Is \code{pattern} a
  shell-globbing wildcard that first needs to be converted
  to a regular expression?}

  \item{invert}{Logical scalar. Invert pattern matching,
  i.e. treat all lines that \strong{not} match
  \code{pattern} as separators?}

  \item{include}{Logical scalar. Also include the separator
  lines in the output files?}

  \item{format}{Character scalar determining the output
  file name format. It is passed to \code{sprintf} and
  expects three placeholders: \itemize{ \item the base name
  of the file; \item the index of the section; \item the
  file extension.  } Getting \code{format} wrong might
  result in non-unique file names and thus probably in
  overwritten files; accordingly, it should be used with
  care.}

  \item{compressed}{Logical scalar. Passed to
  \code{\link{file_pattern}}, but here only affects the way
  file names are split in extensions and base names. Should
  only be set to \code{FALSE} if input files are not
  compressed (and have according file extensions).}

  \item{...}{Optional arguments passed to \code{grepl},
  which is used for matching the separator lines. See also
  \code{invert} listed above.}

  \item{x}{Character vector or convertible to such. Names
  of the files to be modified.}

  \item{overwrite}{Logical scalar. Overwrite already
  existing files, and do not care for duplicate names
  created by cleaning the file names?}

  \item{empty.tmpl}{Character scalar. The template to use
  for file names that become empty after cleaning. Should
  include an integer placeholder to enable incrementing an
  index for creating unique file names. (Empty file names
  should occur rarely anyway.)}
}
\value{
  \code{split_files} creates a list of character vectors,
  each vector containing the names of the newly generated
  files. The names of the list are the input file names.
  The list is returned invisibly.

  \code{clean_filenames} yields a character vector, its
  names corresponding to the renamed old files, values
  corresponding to the novel names, returned invisibly.
}
\description{
  Split files or clean file names.
}
\details{
  \code{split_files} subdivides each file into sections
  which are written individually to newly generated files.
  Sections are determined with patterns that match the
  start of a section. This function is useful for splitting
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  multiple-plate \acronym{CSV} files before inputting them
  with \code{\link{read_opm}}. It is used by
  \code{\link{batch_opm}} for this purpose. See also the
  \sQuote{Examples}.

  \code{clean_filenames} modifies file names by removing
  anything else then word characters, dashes, and dots.
  Also remove trailing and leading dashes and underscores
  (per part of a file name, with dots separating these
  parts) and reduce adjacent dashes and underscores to a
  single one. Note that directory parts within the file
  names, if any, are not affected. This function might be
  useful for managing
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} files, which can contain a lot of special
  characters.
}
\examples{
## split_files()

# Splitting an old-style CSV file containing several plates
(x <- opm_files("multiple"))
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
} else {
  warning("opm example files not found")
}
## One could split new-style CSV as follows (if x is a vector of file names):
# split_files(x, pattern = '^"Data File",')
## note the correct setting of the quotes
## A pattern that covers both old and new-style CSV is:
# split_files(x, pattern = '^("Data File",|Data File)')
## This is used by batch_opm() in 'split' mode any by the 'run_opm.R' script

## clean_filenames()

# Check the example files: they should be ok
(x <- clean_filenames(opm_files("testdata"), demo = TRUE))
stopifnot(length(x) == 0)

# Example with temporary files
(x <- tempfile(pattern = "cb& ahi+ si--")) # bad file name
write("test", x)
stopifnot(file.exists(x))
(y <- clean_filenames(x)) # file renamed
stopifnot(!file.exists(x), file.exists(y))
unlink(y) # tidy up
}
\seealso{
  base::split base::strsplit base::file.rename

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_process}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{glob_to_regex}},
      \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{to_metadata}}
}
\keyword{utilities}

