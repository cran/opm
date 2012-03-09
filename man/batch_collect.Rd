\name{batch_collect}
\alias{batch_collect}
\title{Collect information from files}
\usage{
  batch_collect(names, fun, fun.args = list(), ...,
    simplify = FALSE, demo = FALSE)
}
\arguments{
  \item{names}{Character vector with file or directory
  names, or convertible to such. See
  \code{\link{explode_dir}} for details.}

  \item{fun}{Collecting function. Should use the filename
  as first argument.}

  \item{fun.args}{Optional list of arguments to
  \code{fun}.}

  \item{...}{Optional further arguments passed to
  \code{\link{explode_dir}}.}

  \item{simplify}{Logical scalar. Should the resulting list
  be simplified to a vector or matrix if possible?}

  \item{demo}{Logical scalar. Do not read files, but print
  a vector with the names of the files that would be
  (attempted to) read, and return them invisibly?}
}
\value{
  List, potentially simplified to a vector, depending on
  the output of \code{fun} and the value of
  \code{simplify}. See also \code{demo}.
}
\description{
  Batch-collect information from a series of input files.
  This is not normally directly called by an \pkg{opm} user
  because \code{\link{collect_template}} is available.
}
\examples{
# Read the first line from each of the OPM test dataset files
f <- opm_files("testdata")
if (length(f) > 0L) {
  x <- batch_collect(f, fun = readLines, fun.args = list(n = 1L))
  # yields a list with the input files as names and the result from each
  # file as values (exactly one line)
  stopifnot(is.list(x), identical(names(x), f))
  stopifnot(sapply(x, is.character), sapply(x, length) == 1L)
}
}
\seealso{
  Other IO-functions: \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}

