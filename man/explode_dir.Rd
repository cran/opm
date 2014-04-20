\name{explode_dir}
\alias{batch_collect}
\alias{batch_process}
\alias{explode_dir}
\alias{file_pattern}
\alias{glob_to_regex}
\alias{glob_to_regex.character}
\alias{glob_to_regex.factor}
\title{Batch processing of files}
\usage{
  explode_dir(names, include = NULL, exclude = NULL,
    ignore.case = TRUE, wildcard = TRUE, recursive = TRUE,
    missing.error = TRUE, remove.dups = TRUE)

  batch_collect(names, fun, fun.args = list(), proc = 1L,
    ..., use.names = TRUE, simplify = FALSE, demo = FALSE)

  batch_process(names, out.ext, io.fun, fun.args = list(),
    proc = 1L, outdir = NULL,
    overwrite = c("yes", "older", "no"), in.ext = "any",
    compressed = TRUE,
    literally = inherits(in.ext, "AsIs"), ...,
    verbose = TRUE, demo = FALSE)

  file_pattern(type = c("both", "csv", "yaml", "json", "yorj", "any", "empty"),
    compressed = TRUE, literally = inherits(type, "AsIs"))

  glob_to_regex(object)

  \method{glob_to_regex}{character} (object)

  \method{glob_to_regex}{factor} (object)
}
\arguments{
  \item{names}{Character vector containing file names or
  directories, or convertible to such.}

  \item{object}{Character vector or factor.}

  \item{include}{If a character scalar, used as regular
  expression or wildcard (see the \code{wildcard} argument)
  for selecting from the input files. If \code{NULL},
  ignored. If a list, used as arguments of
  \code{file_pattern} and its result used as regular
  expression. Note that selection is done \strong{after}
  expanding the directory names to file names.}

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
  \code{exclude}, respectively. See below for details on
  such wildcards (a.k.a. globbing patterns).}

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

  \item{fun}{Collecting function. Should use the file name
  as first argument.}

  \item{fun.args}{Optional list of arguments to \code{fun}
  or \code{io.fun}.}

  \item{...}{Optional further arguments passed to
  \code{explode_dir}.}

  \item{proc}{Integer scalar. The number of processes to
  spawn. Cannot be set to more than 1 core if running under
  Windows. See the \code{cores} argument of
  \code{\link{do_aggr}} for details.}

  \item{simplify}{Logical scalar. Should the resulting list
  be simplified to a vector or matrix if possible?}

  \item{use.names}{Logical scalar. Should \code{names} be
  used for naming the elements of the result?}

  \item{out.ext}{Character scalar. The extension of the
  output file names (without the dot).}

  \item{outdir}{Character vector. Directories in which to
  place the output files. If \code{NULL} or only containing
  empty strings, the directory of each input file is used.}

  \item{in.ext}{Character scalar. Passed through
  \code{file_pattern}, then used for the replacement of old
  file extensions with new ones.}

  \item{type}{Character scalar indicating the file types to
  be matched by extension. For historical reasons,
  \kbd{both} means either \acronym{CSV} or \acronym{YAML}
  \emph{or} \acronym{JSON}, whereas \kbd{yorj} means either
  \acronym{YAML} or \acronym{JSON}. Alternatively, directly
  the extension or extensions, or a list of file names (not
  \code{NA}).}

  \item{compressed}{Logical scalar. Shall compressed files
  also be matched?  This affects the returned pattern as
  well as the pattern used for extracting file extensions
  from complete file names (if \code{literally} is
  \code{TRUE}).}

  \item{literally}{Logical scalar. Interpret \code{type}
  literally? This also allows for vectors with more than a
  single element, as well as the extraction of file
  extensions from file names.}

  \item{demo}{Logical scalar. In the case of
  \code{batch_process}, if \code{TRUE} do not convert
  files, but print the attempted input file-output file
  conversions and invisibly return a matrix with input
  files in the first and output files in the second column?
  For the other functions, the effect is equivalent.}

  \item{io.fun}{Conversion function. Should accept
  \code{infile} and \code{outfile} as the first two
  arguments.}

  \item{overwrite}{Character scalar. If \sQuote{yes},
  conversion is always tried if \code{infile} exists and is
  not empty. If \sQuote{no}, conversion is not tried if
  \code{outfile} exists and is not empty. If
  \sQuote{older}, conversion is tried if \code{outfile}
  does not exist or is empty or is older than \code{infile}
  (with respect to the modification time).}

  \item{verbose}{Logical scalar. Print conversion and
  success/failure information?}
}
\value{
  \code{explode_dir} returns a character vector (which
  would be empty if all existing files, if any, had been
  unselected).

  \code{batch_collect} returns a list, potentially
  simplified to a vector, depending on the output of
  \code{fun} and the value of \code{simplify}. See also
  \code{demo}.

  In normal mode, \code{batch_process} creates an invisibly
  returned character matrix in which each row corresponds
  to a named character vector with the keys \code{infile},
  \code{outfile}, \code{before} and \code{after}. The
  latter two describe the result of the action(s) before
  and after attempting to convert \code{infile} to
  \code{outfile}. The \code{after} entry is the empty
  string if no conversion was tried (see \code{overwrite}),
  \code{ok} if conversion was successful and a message
  describing the problems otherwise. For the results of the
  \code{demo} mode see above.

  \code{file_pattern} yields a character scalar, holding a
  regular expression. \code{glob_to_regex} yields a vector
  of regular expressions.
}
\description{
  Batch-collect information from a series of input files or
  batch-convert data from input files to data in output
  files. Alternatively, turn a mixed file/directory list
  into a list of files or create a regular expression
  matching certain file extensions, or convert a wildcard
  pattern to a regular expression. These functions are not
  normally directly called by an \pkg{opm} user but by the
  other IO functions of the package such as
  \code{\link{collect_template}} or
  \code{\link{batch_opm}}. One can use their \code{demo}
  argument directly for testing the results of the applied
  file name patterns.
}
\details{
  Other functions that call \code{explode_dir} have a
  \code{demo} argument which, if set to \code{TRUE}, caused
  the respective function to do no real work but print the
  names of the files that it would process in normal
  running mode.

  \code{glob_to_regex} changes a shell globbing wildcard
  into a regular expression. This is just a slightly
  extended version of \code{glob2rx} from the \pkg{utils}
  package, but more conversion steps might need to be added
  here in the future. Particularly
  \code{\link{explode_dir}} and the IO functions calling
  that function internally use \code{glob_to_regex}. Some
  hints when using globbing patterns are given in the
  following.

  The here used globbing search patterns contain only two
  special characters, \sQuote{?} and \sQuote{*}, and are
  thus more easy to master than regular expressions.
  \sQuote{?} matches a single arbitrary character, whereas
  \sQuote{*} matches zero to an arbitrary number of
  arbitrary characters. Some examples: \describe{
  \item{a?c}{Matches \verb{abc}, \verb{axc}, \verb{a c}
  etc. but not \verb{abbc}, \verb{abbbc}, \verb{ac} etc.}
  \item{a*c}{Matches \verb{abc}, \verb{abbc}, \verb{ac}
  etc. but not \verb{abd} etc.} \item{ab*}{Matches
  \verb{abc}, \verb{abcdefg}, \verb{abXYZ} etc. but not
  \verb{acdefg} etc.} \item{?bc}{Matches \verb{abc},
  \verb{Xbc}, \verb{ bc} etc. but not \verb{aabc},
  \verb{abbc}, \verb{bc} etc.} } Despite their simplicity,
  globbing patterns are often sufficient for selecting file
  names.
}
\examples{
# explode_dir()
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

# batch_collect()
# Read the first line from each of the OPM test data set files
f <- opm_files("testdata")
if (length(f) > 0) { # if the files are found
  x <- batch_collect(f, fun = readLines, fun.args = list(n = 1L))
  # yields a list with the input files as names and the result from each
  # file as values (exactly one line)
  stopifnot(is.list(x), identical(names(x), f))
  stopifnot(sapply(x, is.character), sapply(x, length) == 1L)
} else {
  warning("test files not found")
}
# For serious tasks, consider to first try the function in 'demo' mode.

# batch_process()
# Read the first line from each of the OPM test data set files and store it
# in temporary files
pf <- function(infile, outfile) write(readLines(infile, n = 1), outfile)
infiles <- opm_files("testdata")
if (length(infiles) > 0) { # if the files are found
  x <- batch_process(infiles, out.ext = "tmp", io.fun = pf,
    outdir = tempdir())
  stopifnot(is.matrix(x), identical(x[, 1], infiles))
  stopifnot(file.exists(x[, 2]))
  unlink(x[, 2])
} else {
  warning("test files not found")
}
# For serious tasks, consider to first try the function in 'demo' mode.

# file_pattern()
(x <- file_pattern())
(y <- file_pattern(type = "csv", compressed = FALSE))
stopifnot(nchar(x) > nchar(y))
# constructing pattern from existing files
(files <- list.files(pattern = "[.]"))
(x <- file_pattern(I(files))) # I() causes 'literally' to be TRUE
stopifnot(grepl(x, files, ignore.case = TRUE))

# glob_to_regex()
x <- "*what glob2rx() can't handle because a '+' is included*"
(y <- glob_to_regex(x))
(z <- glob2rx(x))
stopifnot(!identical(y, z))
# Factor method
(z <- glob_to_regex(as.factor(x)))
stopifnot(identical(as.factor(y), z))
}
\seealso{
  base::list.files base::Sys.glob utils::glob2rx
  base::regex

  Other io-functions: \code{\link{batch_opm}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}}
}
\keyword{IO}
\keyword{character}

