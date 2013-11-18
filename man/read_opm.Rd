\name{read_opm}
\alias{read_opm}
\alias{read_single_opm}
\title{Read multiple \acronym{PM} files at once or read single \acronym{PM} file}
\usage{
  read_opm(names,
    convert = c("try", "no", "yes", "sep", "grp"),
    gen.iii = opm_opt("gen.iii"), include = list(), ...,
    demo = FALSE)

  read_single_opm(filename)
}
\arguments{
  \item{names}{Character vector with names of files in one
  of the formats accepted by \code{\link{read_single_opm}},
  or names of directories containing such files, or both;
  or convertible to such a vector. See the \code{include}
  argument of \code{\link{read_opm}} and
  \code{\link{explode_dir}} for how to select subsets from
  the input files or directories.}

  \item{filename}{Character scalar, or convertible to such,
  with the obvious meaning.}

  \item{convert}{Character scalar with one of the following
  values: \describe{ \item{no}{Always return a list of
  \code{\link{OPM}} objects. (This list is rather a
  \code{\link{MOPMX}} object than a plain list.)}
  \item{yes}{Convert to \code{NULL}, \code{\link{OPM}}
  object, or \code{\link{OPMS}} object, depending on the
  number of files read (0, 1, or more).} \item{try}{Behave
  like \sQuote{yes} but do not result in an error message
  if conversion to \code{\link{OPMS}} is impossible; return
  a list (\code{\link{MOPMX}} object) in that case.}
  \item{sep}{Return a nested list, each partial list
  (\code{\link{MOPMX}} object) containing \code{\link{OPM}}
  objects of the same plate type.} \item{grp}{Also split
  into such contained lists but convert them to
  \code{\link{OPMS}} objects if more than one plate is
  encountered. An error is raised if this is impossible (in
  contrast to \sQuote{try}). Return a list
  (\code{\link{MOPMX}} object).} }}

  \item{gen.iii}{Logical or character scalar. If
  \code{TRUE}, invoke \code{\link{gen_iii}} on each plate.
  This is automatically done with \acronym{CSV} input if
  the plate type is given as \acronym{OTH} (which is
  usually the case for plates run in ID mode). If a
  character scalar, it is used as the \code{to} argument of
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
  \code{read_opm} returns an \code{\link{OPM}} object
  (maybe \code{\link{OPMA}} in case of \acronym{YAML}
  input), or list (\code{\link{MOPMX}} object) of such
  objects, or \code{\link{OPMS}} object.  If \code{demo} is
  \code{TRUE}, a character vector instead.

  \code{read_single_opm} also returns an\code{\link{OPM}}
  object. In the case of \acronym{YAML} input, this might
  also be an \code{\link{OPMA}} object or a list of such
  objects, but \strong{not} an \code{\link{OPMS}} object.
}
\description{
  Read OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  or \pkg{opm} data file(s) in one of three possible
  formats: either new- or old-style
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} or \pkg{opm} \acronym{YAML} (including
  \acronym{JSON}) format.
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
  \acronym{CSV} are also understood, as well as files
  compressed using \command{gzip}, \command{bzip2},
  \command{lzma} or \command{xz}. (Files can be
  specifically excluded using \code{include} and/or
  \code{exclude}).
}
\details{
  The expected \acronym{CSV} format is what is output by an
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  instrument, one plate per file, or a
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
  instrument, with one to many plates per file. Other
  formats, or
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} files
  re-saved with distinct \acronym{CSV} settings, are not
  understood. For this reason, if any editing of the files
  was necessary at all, it is advisable to do this in an
  editor for plain text, not in a spreadsheet program.

  Plates run in ID mode are automatically detected as such
  (their plate type is changed from \acronym{OTH} to the
  internally used spelling of \sQuote{Generation III}). A
  generation-III or other plate type can also be forced
  later on by using \code{\link{gen_iii}}.

  It is \strong{impossible} to read \acronym{CSV} files
  that contain more than one plate. For splitting old-style
  and new-style \acronym{CSV} files into one file per
  plate, see the example under \code{\link{split_files}}.
  In contrast, input \acronym{YAML} files can contain data
  from more than one plate. The format (which includes
  \acronym{JSON}) is described in detail under
  \code{\link{batch_opm}}.

  For splitting lists of \code{\link{OPM}} objects
  according to the plate type, see
  \code{\link{plate_type}}, and consider the plate-type
  selection options of \code{\link{opms}}.

  The order in which it is tried to read distinct formats
  of \acronym{CSV} files can be modified using the
  \sQuote{input.try.order} key of \code{\link{opm_opt}}.
  The value is an integer vector whose elements have the
  following meaning: \enumerate{ \item New-style
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV}.  \item Old-style
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV}.  \item
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
  \acronym{CSV}.  } For instance,
  \code{opm_opt(input.try.order = 2:1} would change the
  order in which
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  formats are tried and deselect
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
  files entirely. Negative indexes can be used, but
  non-negative values not within the range listed above
  will result in an error. If it known in advance which
  formats are (not) to be expected, subset creation or just
  changing the order can be used to accelerate data input.
}
\examples{
test.files <- opm_files("omnilog")
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

# read_single_opm()
test.files <- opm_files("omnilog")
if (length(test.files) > 0) { # if the folder is found
  x <- read_single_opm(test.files[1]) # => 'OPM' object
  class(x)
  dim(x)
  summary(x)
  stopifnot(is(x, "OPM"), identical(dim(x), c(384L, 96L)))
} else {
  warning("test-file folder not found")
}
# this can be repeated for the other input test files
}
\references{
  \url{http://www.yaml.org/}

  \url{http://www.json.org/}

  \url{http://www.biolog.com/}
}
\seealso{
  utils::read.csv yaml::yaml.load_file

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{glob_to_regex}},
      \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}

