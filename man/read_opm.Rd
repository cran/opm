\name{read_opm}
\alias{read_opm}
\title{Read multiple PM files at once}
\usage{
  read_opm(names,
    convert = c("try", "no", "yes", "sep", "grp"),
    gen.iii = opm_opt("gen.iii"), include = list(), ...,
    demo = FALSE)
}
\arguments{
  \item{names}{Character vector with names of files in one
  of the formats accepted by \code{\link{read_single_opm}},
  or names of directories containing such files, or both;
  or convertible to such a vector. See the \code{include}
  argument of \code{\link{read_opm}} and
  \code{\link{explode_dir}} for how to select subsets from
  the input files or directories.}

  \item{convert}{Character scalar with one of the following
  values: \describe{ \item{no}{Always return a list (of
  \code{\link{OPM}} objects).} \item{yes}{Convert to
  \code{NULL}, \code{\link{OPM}} object, or
  \code{\link{OPMS}} object, depending on the number of
  files read (0, 1, or more).} \item{try}{Behave like
  \sQuote{yes} but do not result in an error message if
  conversion to OPMS is impossible; return a list in that
  case.} \item{sep}{Return a nested list, each sublist
  containing \code{\link{OPM}} objects of the same plate
  type.} \item{grp}{Also split into such sublists but
  convert them to \code{\link{OPMS}} objects if more than
  one plate is encountered. An error is raised if this is
  impossible (in contrast to \sQuote{try}).} }}

  \item{gen.iii}{Logical or character scalar. If
  \code{TRUE}, invoke \code{\link{gen_iii}} on each plate.
  This is automatically done with \acronym{CSV} input if
  the plate type is given as \sQuote{OTH} (which is usually
  the case for plates run in ID mode). If a character
  scalar, it is used as the \code{to} argument of
  \code{\link{gen_iii}} to set other plate types unless it
  is empty.}

  \item{include}{Pattern for selecting from the input
  files. The default value results in the output of
  \code{\link{file_pattern}}, which should be sufficient in
  most cases. See \code{\link{explode_dir}} for details on
  other possibilities.}

  \item{...}{Optional further arguments passed to
  \code{\link{explode_dir}}.}

  \item{demo}{Logical scalar. Do not read files, but print
  a vector with the names of the files that would be
  (attempted to) read, and return them invisibly?}
}
\value{
  \code{\link{OPM}} object (maybe \code{\link{OPMA}} in
  case of \acronym{YAML} input), or list of such objects,
  or \code{\link{OPMS}} object. If \code{demo} is
  \code{TRUE}, a character vector instead.
}
\description{
  Read OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  or \pkg{opm} data file(s) in one of three possible
  formats: either new- or old-style
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} or \pkg{opm} \acronym{YAML} format. Files
  compressed using \command{gzip}, \command{bzip2},
  \command{lzma} or \command{xz} are also understood (but
  may be excluded using \code{include} and/or
  \code{exclude}).
}
\details{
  Regarding the \acronym{CSV} format, see the remark to
  \code{\link{read_single_opm}}. The \acronym{YAML} format
  is described in detail under \code{\link{batch_opm}}.

  or splitting lists of \code{\link{OPM}} objects according
  to the plate type, see \code{\link{plate_type}}, and
  consider the plate-type selection options of
  \code{\link{opms}}.
}
\examples{
test.files <- grep("Multiple|Ecoplate", opm_files("testdata"), invert = TRUE,
  value = TRUE, perl = TRUE)
if (length(test.files) > 0) { # if the folder is found
  x <- read_opm(test.files, demo = TRUE) # check first what you would get
  stopifnot(identical(test.files, x))
  x <- read_opm(test.files[1:2]) # these two have the same plate type
  class(x)
  dim(x)
  summary(x)
  stopifnot(is(x, "OPMS"), identical(dim(x), c(2L, 384L, 96L)))
} else {
  warning("test files not found")
}
# This can be repeated for the other input test files. Instead of a several
# file names one can also provide a single one, one to several directory
# names, or mixture of file and directory names.

\dontrun{

# Reading all files from the current working directory is also easy:
x <- read_opm(getwd())
# or
x <- read_opm(".")
}
}
\references{
  \url{http://www.yaml.org/}

  \url{http://www.json.org/}

  \url{http://www.biolog.com/}
}
\seealso{
  utils::read.csv yaml::yaml.load_file

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}

