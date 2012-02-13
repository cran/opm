\name{batch_process}
\alias{batch_process}
\title{Convert infiles to outfiles}
\usage{
  batch_process(names, out.ext, io.fun, fun.args = list(),
    proc = 1L, outdir = NULL,
    overwrite = c("yes", "older", "no"), in.ext = "any",
    compressed = TRUE, ..., verbose = TRUE, demo = FALSE)
}
\arguments{
  \item{out.ext}{Character scalar. The extension of the
  outfile names (without the dot).}

  \item{proc}{Integer scalar. The number pf processes to
  spawn.}

  \item{outdir}{Character vector. Directories in which to
  place the outfiles.  If \code{NULL}, each infile's
  directory is used.}

  \item{in.ext}{Character scalar. Passed through
  \code{\link{file_pattern}}, then used for the replacement
  of old file extensions with new ones.}

  \item{compressed}{Logical scalar. Passed as 2nd argument
  to \code{\link{file_pattern}}.}

  \item{demo}{Logical scalar. Do not convert files, but
  print the attempted infile-outfile conversions and
  invisibly return a matrix with infiles in the first and
  outfiles in the second column?}

  \item{io.fun}{Conversion function. Should accept
  \code{infile} and \code{outfile} as the first two
  arguments.}

  \item{fun.args}{Optional list of further arguments of
  \code{io.fun}.}

  \item{overwrite}{Character scalar. If \sQuote{yes},
  conversion is always tried if \code{infile} exists and is
  not empty. If \sQuote{no}, conversion is not tried if
  \code{outfile} exists and is not empty. If
  \sQuote{older}, conversion is tried if \code{outfile}
  does not exist or is empty or is older than \code{infile}
  (with respect to the modification time).}

  \item{verbose}{Logical scalar. Print conversion and
  success/failure information?}

  \item{names}{Character vector with file or directory
  names, or convertible to such. See
  \code{\link{explode_dir}} for details.}

  \item{...}{Optional further arguments passed to
  \code{\link{explode_dir}}.}
}
\value{
  In normal mode, an invisibly returned character matrix in
  which each row corresponds to a named character vector
  with the keys \sQuote{infile}, \sQuote{outfile},
  \sQuote{before} and \sQuote{after}. The latter two
  describe the result of the action(s) before and after
  attempting to convert \code{infile} to \code{outfile}.
  \sQuote{after} is the empty string if no conversion was
  tried (see \code{overwrite}), \sQuote{ok} if conversion
  was successful and a message describing the problems
  otherwise.  For the results of the \code{demo} mode see
  above.
}
\description{
  Batch-convert data from infiles to data in outfiles. This
  is not normally directly called by an \pkg{opm} user
  because \code{\link{batch_opm_to_yaml}} is available.
}
\examples{
# Read the first line from each of the OPM test dataset files and store it
# in temporary files
pf <- function(infile, outfile) write(readLines(infile, n = 1), outfile)
infiles <- opm_files("testdata")
if (length(infiles) > 0) {
  x <- batch_process(infiles, out.ext = "tmp", io.fun = pf,
    outdir = tempdir())
  stopifnot(is.matrix(x), identical(x[, 1], infiles))
  stopifnot(file.exists(x[, 2]))
  unlink(x[, 2])
}
}
\seealso{
  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{IO}

