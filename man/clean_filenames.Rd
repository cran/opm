\name{clean_filenames}
\alias{clean_filenames}
\title{Clean file names}
\usage{
  clean_filenames(x, overwrite = FALSE, demo = FALSE,
    empty.tmpl = "__EMPTY__\%05i__")
}
\arguments{
  \item{x}{Character vector or convertible to such. Names
  of the files to be modified.}

  \item{overwrite}{Logical scalar. Overwrite pre-existing
  files, and do not care for duplicate names created by
  cleaning the filenames?}

  \item{demo}{Logical scalar. Do not rename files but just
  return the usual result indicating the renaming actions
  that would be attempted? (Note that this does not
  indicate whether the renaming would also by successful.)}

  \item{empty.tmpl}{Character scalar. The template to use
  for filenames that become empty after cleaning. Should
  include an integer placeholder to enable incrementing an
  index for creating unique filenames. (Empty filenames
  should occur rarely anyway.)}
}
\value{
  Character vector, its names corresponding to the renamed
  old files, values corresponding to the novel names,
  returned invisibly.
}
\description{
  Clean file names by removing anything else then word
  characters, dashes, and dots. Also remove trailing and
  leading dashes and underscores (per part of a file name,
  with dots separating these parts) and reduce adjacent
  dashes and underscores to a single one. Note that
  directory parts within the file names, if any, are not
  affected.
}
\examples{
# Check the example files: they should be ok
(x <- clean_filenames(opm_files("testdata"), demo = TRUE))
stopifnot(length(x) == 0)

# Example with temporary files
(x <- tempfile(pattern = "cb& ahi+ si--"))
write("test", x)
stopifnot(file.exists(x))
(y <- clean_filenames(x))
stopifnot(!file.exists(x), file.exists(y))
unlink(y) # tidy up
}
\seealso{
  base::file.rename

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{utilities}

