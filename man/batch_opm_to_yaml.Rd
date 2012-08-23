\name{batch_opm_to_yaml}
\alias{batch_opm_to_yaml}
\title{Batch-convert to YAML}
\usage{
  batch_opm_to_yaml(names, md.args = NULL,
    aggr.args = NULL, force.aggr = FALSE,
    gen.iii = opm_opt("gen.iii"), ..., verbose = TRUE,
    demo = FALSE)
}
\arguments{
  \item{md.args}{If not \code{NULL} but a list, passed as
  arguments to \code{\link{include_metadata}} with the data
  read from each individual file as additional argument
  \sQuote{object}. If \code{NULL}, metadata are not
  included (but may already be present in the case of YAML
  input).}

  \item{aggr.args}{If not \code{NULL} but a list, passed as
  arguments to \code{\link{do_aggr}} with the data read
  from each individual file as additional argument
  \code{x}.  If \code{NULL}, aggregation takes not place
  (but aggregated data may already be present in case of
  YAML input).}

  \item{force.aggr}{Logical scalar. If \code{FALSE}, do not
  aggregate already aggregated data (which can be present
  in YAML input).}

  \item{...}{Optional arguments passed to
  \code{\link{batch_process}} in addition to \code{verbose}
  and \code{demo}. Note that \code{out.ext}, \code{fun} and
  \code{fun.args} are set automatically.}

  \item{names}{Character vector with names of files in one
  of three formats accepted by \code{\link{read_opm}}, or
  names of directories containing such files, or both; or
  convertible to such a vector. See the \code{include}
  argument of \code{\link{read_opm}} and
  \code{\link{explode_dir}} for how to select subsets from
  the input files or directories.}

  \item{gen.iii}{Logical scalar. If \code{TRUE}, invoke
  \code{\link{gen_iii}} on each plate. This is
  automatically done with CSV input if the plate type is
  given as \sQuote{OTH} (which is usually the case for
  plates run in ID mode).}

  \item{demo}{Logical scalar. Do not read files, but print
  a vector with the names of the files that would be
  (attempted to) read, and return them invisibly?}

  \item{verbose}{Logical scalar. Print conversion and
  success/failure information?}
}
\value{
  The function invisibly returns a matrix which describes
  each attempted file conversion. See
  \code{\link{batch_process}} for details.
}
\description{
  Batch-convert from OmniLog(R) CSV (or previous \pkg{opm}
  YAML) to \pkg{opm} YAML. It is possible to add metadata
  to each set of raw data and to aggregate the curves;
  these additional data will then be included in the YAML
  output file.
}
\details{
  A YAML document can comprise \emph{scalars} (single
  values of some type), \emph{sequences} (ordered
  collections of some values, without names) and
  \emph{mappings} (collections assigning a name to each
  value), in a variety of combinations (e.g., mappings of
  sequences). The output of \code{batch_opm_to_yaml} is one
  YAML document \emph{per plate} which represents a mapping
  with the following components (key-value pairs):
  \describe{ \item{metadata}{Arbitrarily nested mapping of
  arbitrary metadata entries. Empty if no metadata have
  been added.} \item{csv_data}{Non-nested mapping
  containing the OmniLog(R) run information read from the
  input CSV file (character scalars) together with the
  measurements. The most important entry is most likely the
  plate type.} \item{measurements}{A mapping whose values
  are sequences of floating-point numbers of the same
  length and in the appropriate order. The keys are
  \sQuote{hours}, which refers to the time points, and the
  well coordinates, ranging between \sQuote{A01} and
  \sQuote{H12}.} \item{aggregated}{A mapping, only present
  if curve parameters have been estimated. Its keys
  correspond to those of \sQuote{measurements} with the
  exception of \sQuote{hours}. The values are themselves
  mappings, whose keys indicate the respective curve
  parameter and whether this is the point estimate or the
  upper or lower confidence interval. The values of these
  secondary mappings are floating-point numbers.}
  \item{aggr_settings}{A mapping, only present if curve
  parameters have been estimated. Its keys are
  \sQuote{program} and \sQuote{options}.  The value of the
  former is a character scalar. The value of the latter is
  an arbitrarily nested mapping with arbitrary content.} }
  Details of the contents should be obvious from the
  documentation of the classes of the objects from which
  the YAML output is generated. In the case of YAML input
  with several plates per file, \code{batch_opm_to_yaml}
  generates YAML output files containing a sequence of
  mappings as described above, one per plate, to keep a 1:1
  relationship between input and output files.
}
\note{
  \itemize{ \item Regarding the CSV format, see the remark
  to \code{\link{read_single_opm}}.  \item This function is
  for batch-converting many files; for writing a single
  object to a YAML file (or string), see
  \code{\link{to_yaml}}.  \item When inputting YAML files
  generated with the help of the \pkg{yaml} package (on
  which the \pkg{opm} implementation is based) using other
  programming languages, a potential problem is that they,
  and YAML in general, lack a native representation of
  \code{NA} values. Such entries are likely to be
  misunderstood as \sQuote{NA} character scalars. }
}
\examples{
test.files <- grep("Multiple", opm_files("testdata"), invert = TRUE,
  value = TRUE, fixed = TRUE)
if (length(test.files) > 0) {
  num.files <- length(list.files(outdir <- tempdir()))
  x <- batch_opm_to_yaml(test.files[1], outdir = outdir)
  stopifnot(length(list.files(outdir)) == num.files + 1, is.matrix(x))
  stopifnot(file.exists(x[, "outfile"]))
  stopifnot(test.files[1] == x[, "infile"])
  unlink(x[, "outfile"])
}
}
\references{
  \url{http://www.yaml.org/}

  \url{http://www.biolog.com/}
}
\seealso{
  utils::read.csv yaml::yaml.load_file

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}}
}
\keyword{IO}

