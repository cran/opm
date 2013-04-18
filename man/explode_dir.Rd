\name{explode_dir}
\alias{explode_dir}
\title{Conversion of directory names to file names}
\usage{
  explode_dir(names, include = NULL, exclude = NULL,
    ignore.case = TRUE, wildcard = TRUE, recursive = TRUE,
    missing.error = TRUE, remove.dups = TRUE)
}
\arguments{
  \item{names}{Character vector containing filenames or
  directories, or convertible to such.}

  \item{include}{If a character scalar, used as regular
  expression or wildcard (see the \code{wildcard} argument)
  for selecting from the input files. If \code{NULL},
  ignored. If a list, used as arguments of
  \code{\link{file_pattern}} and its result used as regular
  expression. Note that selection is done \strong{after}
  expanding the directory names to filenames.}

  \item{exclude}{Like \code{include}, but for excluding
  matching input files.  Note that exclusion is done
  \strong{after} applying \code{include}.}

  \item{ignore.case}{Logical scalar. Ignore differences
  between uppercase and lowercase when using \code{include}
  and \code{exclude}? Has no effect for \code{NULL} values
  for \code{include} or \code{exclude}, respectively.}

  \item{wildcard}{Logical scalar. Are \code{include} and
  \code{exclude} wildcards (as used by UNIX shells) that
  first need to be concerted to regular expressions? Has no
  effect if lists are used for \code{include} or
  \code{exclude}, respectively. See
  \code{\link{glob_to_regex}} for details on such wildcards
  (a.k.a. globbing patterns).}

  \item{recursive}{Logical scalar. Traverse directories
  recursively and also consider all subdirectories? See
  \code{list.files} from the \pkg{base} package for
  details.}

  \item{missing.error}{Logical scalar. If a file/directory
  does not exist, raise an error or only a warning?}

  \item{remove.dups}{Logical scalar. Remove duplicates from
  \code{names}? Note that if requested this is done
  \strong{before} expanding the names of directories, if
  any.}
}
\value{
  Character vector (which would be empty if all existing
  files, if any, had been unselected).
}
\description{
  Turn a mixed file/directory list into a list of files.
  This is not normally directly called by an \pkg{opm} user
  but by the other IO functions of the package. One can use
  their \code{demo} argument directly for testing the
  results of the applied filename patterns.
}
\note{
  Other functions that call this function should have a
  \code{demo} argument which, if set to \code{TRUE}, caused
  the respective function to do no real work but print the
  names of the files that it would process in normal
  running mode.
}
\examples{
# Example with temporary directory
td <- tempdir()
tf <- tempfile()
(x <- explode_dir(td))
write(letters, tf)
(y <- explode_dir(td))
stopifnot(length(y) > length(x))
unlink(tf)
(y <- explode_dir(td))
stopifnot(length(y) == length(x))

# Example with R installation directory
(x <- explode_dir(R.home(), include = "*/doc/html/*"))
(y <- explode_dir(R.home(), include = "*/doc/html/*", exclude = "*.html"))
stopifnot(length(x) == 0L || length(x) > length(y))

# More interesting use cases are provided by the functions that call
# explode_dir(). Consider to first try them in 'demo' mode. Globbing
# examples are given under glob_to_regex().
}
\seealso{
  base::list.files base::Sys.glob

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{file_pattern}}, \code{\link{opm_files}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}
\keyword{character}

