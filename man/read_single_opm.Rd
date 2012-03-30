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
  \code{\link{OPM}} object. In the case of YAML input, this
  might also be an \code{\link{OPMA}} object or a list of
  such objects, but \strong{not} an \code{\link{OPMS}}
  object.
}
\description{
  Read single OmniLog(R) or \pkg{opm} data file in either
  new- or old-style CSV or YAML format. Files compressed
  using gzip, bzip2 or lzma/xz are also understood.
}
\note{
  \itemize{ \item The expected CSV format is what is output
  by the OmniLog(R) device, one plate per file. Other
  formats, or OmniLog(R) files re-saved with distinct CSV
  settings, are not understood. For this reason, if any
  editing of the files was necessary at all, it is
  advisable to do this in an editor for plain text, not in
  a spreadsheet program.  \item It is impossible to read
  CSV files that contain more than one plate.  For
  splitting old-style and new-style CSV files into one file
  per plate, see the example under
  \code{\link{split_files}}.  \item In contrast, input YAML
  files can contain data from more than one plate.  \item
  Plates run in ID mode are automatically detected as such
  (their plate type is changed from \sQuote{OTH} to the
  internally used spelling of \sQuote{Generation III}). A
  generation-III plate type can also be forced later on by
  using \code{\link{gen_iii}}. }
}
\examples{
test.files <- opm_files("testdata")
if (length(test.files) > 0) { # if the folder is found
  x <- read_single_opm(test.files[1])
  class(x)
  dim(x)
  summary(x)
  stopifnot(is(x, "OPM"), identical(dim(x), c(384L, 96L)))
}
# this can be repeated for the other input test files
}
\references{
  \url{http://www.yaml.org/}

  \url{http://www.biolog.com/}
}
\seealso{
  utils::read.csv

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}}
}
\keyword{IO}

