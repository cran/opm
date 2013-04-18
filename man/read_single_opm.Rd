\name{read_single_opm}
\alias{read_single_opm}
\title{Read single PM file}
\usage{
  read_single_opm(filename)
}
\arguments{
  \item{filename}{Character scalar, or convertible to such,
  with the obvious meaning.}
}
\value{
  \code{\link{OPM}} object. In the case of \acronym{YAML}
  input, this might also be an \code{\link{OPMA}} object or
  a list of such objects, but \strong{not} an
  \code{\link{OPMS}} object.
}
\description{
  Read single
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} or
  \pkg{opm} data file in either new- or old-style
  \acronym{CSV} or \acronym{YAML} (including \acronym{JSON}
  format.
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
  \acronym{CSV} are also understood, as well as files
  compressed using \command{gzip}, \command{bzip2},
  \command{lzma} or \command{xz}.
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
  (their plate type is changed from \sQuote{OTH} to the
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
}
\examples{
test.files <- opm_files("testdata")
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
  utils::read.csv

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{read_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}

