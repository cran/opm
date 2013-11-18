\name{batch_opm}
\alias{batch_opm}
\title{Batch-convert \acronym{PM} data}
\usage{
  batch_opm(names, md.args = NULL, aggr.args = NULL,
    force.aggr = FALSE, disc.args = NULL,
    force.disc = FALSE, gen.iii = opm_opt("gen.iii"),
    device = "mypdf", dev.args = NULL, plot.args = NULL,
    csv.args = NULL,
    table.args = list(sep = "\t", row.names = FALSE), ...,
    proc = 1L, outdir = "", overwrite = "no",
    output = c("yaml", "json", "csv", "xyplot", "levelplot", "split", "clean"),
    combine.into = NULL, verbose = TRUE, demo = FALSE)
}
\arguments{
  \item{md.args}{If not \code{NULL} but a list, passed as
  arguments to \code{\link{include_metadata}} with the data
  read from each individual file as additional argument
  \sQuote{object}. If \code{NULL}, metadata are not
  included (but may already be present in the case of
  \acronym{YAML} input).}

  \item{aggr.args}{If not \code{NULL} but a list, passed as
  arguments to \code{\link{do_aggr}} with the data read
  from each individual file as additional argument
  \code{object}.  If \code{NULL}, aggregation takes not
  place (but aggregated data may already be present in case
  of \acronym{YAML} input).}

  \item{force.aggr}{Logical scalar. If \code{FALSE}, do not
  aggregate already aggregated data (which can be present
  in \acronym{YAML} input).}

  \item{disc.args}{If not \code{NULL} but a list, passed as
  arguments to \code{\link{do_disc}} with the data read
  from each individual file as additional argument
  \code{object}.  If \code{NULL}, discretisation takes not
  place (but discretised data may already be present in
  case of \acronym{YAML} input).}

  \item{force.disc}{Logical scalar. If \code{FALSE}, do not
  discretise already discretised data (which can be present
  in \acronym{YAML} input).}

  \item{device}{Character scalar describing the graphics
  device used for outputting plots. See \code{Devices} from
  the \pkg{grDevices} package and \code{mypdf} from the
  \pkg{pkgutils} package for possible values. The extension
  of the output files is created from the device name after
  a few adaptations (such as converting \kbd{postscript} to
  \kbd{ps}).}

  \item{dev.args}{List. Passed as additional arguments to
  \code{device}.}

  \item{plot.args}{List. Passed as additional arguments to
  the plotting function used.}

  \item{csv.args}{If not \code{NULL} but a list, used for
  specifying ways to use \code{\link{csv_data}} entries
  directly as \code{\link{metadata}}. The list can contain
  character vectors used for selecting and optionally
  renaming \acronym{CSV} entries or functions that can be
  applied to an entire data frame containing all
  \acronym{CSV} entries. Note that this argument has
  nothing to do with \kbd{csv} output.}

  \item{table.args}{Passed to \code{write.table} from the
  \pkg{utils} package if \code{output} is set to \kbd{csv}.
  Do not confuse this with \code{csv.args}.}

  \item{...}{Optional arguments passed to
  \code{\link{batch_process}} in addition to \code{verbose}
  and \code{demo}. Note that \code{out.ext}, \code{fun} and
  \code{fun.args} are set automatically. Alternatively,
  these are parameters passed to
  \code{\link{batch_collect}}.}

  \item{output}{Character scalar determining the main
  output mode. \describe{ \item{clean}{Apply
  \code{\link{clean_filenames}}.} \item{csv}{Create
  \acronym{CSV} files, by default one per input file.}
  \item{json}{Create \acronym{JSON} files, by default one
  per input file.} \item{levelplot}{Create graphics files,
  by default one per input file, containing the output of
  \code{\link{level_plot}}.} \item{split}{Split
  multiple-plate new style or old style \acronym{CSV} files
  with \code{\link{split_files}}.} \item{yaml}{Create
  \acronym{YAML} files, by default one per input file.}
  \item{xyplot}{Create graphics files, by default one per
  input file, containing the output of
  \code{\link{xy_plot}}.} }}

  \item{combine.into}{Empty or character scalar modifying
  the output mode unless it is \sQuote{clean} or
  \sQuote{split}. If non-empty, causes the creation of a
  single output file named per plate type encountered in
  the input, instead of one per input file (the default).
  Thus \code{combine.into} should be given as a template
  string for \code{sprintf} from the \pkg{base} package
  with one placeholder for the plate-type, and without a
  file extension.}

  \item{names}{Character vector with names of files in one
  of the formats accepted by \code{\link{read_single_opm}},
  or names of directories containing such files, or both;
  or convertible to such a vector. See the \code{include}
  argument of \code{\link{read_opm}} and
  \code{\link{explode_dir}} for how to select subsets from
  the input files or directories.}

  \item{gen.iii}{Logical or character scalar. If
  \code{TRUE}, invoke \code{\link{gen_iii}} on each plate.
  This is automatically done with \acronym{CSV} input if
  the plate type is given as \acronym{OTH} (which is
  usually the case for plates run in ID mode). If a
  character scalar, it is used as the \code{to} argument of
  \code{\link{gen_iii}} to set other plate types unless it
  is empty.}

  \item{demo}{Logical scalar. Do not read files, but print
  a vector with the names of the files that would be
  (attempted to) read, and return them invisibly?}

  \item{proc}{Integer scalar. The number of processes to
  spawn. Cannot be set to more than 1 core if running under
  Windows. See the \code{cores} argument of
  \code{\link{do_aggr}} for details.}

  \item{outdir}{Character vector. Directories in which to
  place the output files. If \code{NULL} or only containing
  empty strings, the directory of each input file is used.}

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
  The function invisibly returns a matrix which describes
  each attempted file conversion. See
  \code{\link{batch_process}} for details.
}
\description{
  Batch-convert from
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} (or previous \pkg{opm} \acronym{YAML} or
  \acronym{JSON}) to \pkg{opm} \acronym{YAML} (or
  \acronym{JSON}). It is possible to add metadata to each
  set of raw data and to aggregate the curves; these
  additional data will then be included in the output
  files.
}
\details{
  This function is for batch-converting many files; for
  writing a single object to a \acronym{YAML} file (or
  string), see \code{\link{to_yaml}}.

  A \acronym{YAML} document can comprise \emph{scalars}
  (single values of some type), \emph{sequences} (ordered
  collections of some values, without names) and
  \emph{mappings} (collections assigning a name to each
  value), in a variety of combinations (e.g., mappings of
  sequences). The output of \code{batch_opm} is one
  \acronym{YAML} document \emph{per plate} which represents
  a mapping with the following components (key-value
  pairs): \describe{ \item{metadata}{Arbitrarily nested
  mapping of arbitrary metadata entries. Empty if no
  metadata have been added.} \item{csv_data}{Non-nested
  mapping containing the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} run
  information read from the input \acronym{CSV} file
  (character scalars) together with the measurements. The
  most important entry is most likely the plate type.}
  \item{measurements}{A mapping whose values are sequences
  of floating-point numbers of the same length and in the
  appropriate order.  The keys are \sQuote{hours}, which
  refers to the time points, and the well coordinates,
  ranging between \sQuote{A01} and \sQuote{H12}.}
  \item{aggregated}{A mapping, only present if curve
  parameters have been estimated. Its keys correspond to
  those of \sQuote{measurements} with the exception of
  \sQuote{hours}. The values are themselves mappings, whose
  keys indicate the respective curve parameter and whether
  this is the point estimate or the upper or lower
  confidence interval. The values of these secondary
  mappings are floating-point numbers.}
  \item{aggr_settings}{A mapping, only present if curve
  parameters have been estimated. Its keys are
  \sQuote{software}, \sQuote{version} and \sQuote{options}.
  The value of the former two is a character scalar.  The
  value of \sQuote{options} is an arbitrarily nested
  mapping with arbitrary content.} \item{discretized}{A
  mapping, only present if curve parameters have been
  estimated and also discretised. Its keys correspond to
  those of \sQuote{measurements} with the exception of
  \sQuote{hours}. The values are logical scalars.}
  \item{disc_settings}{A mapping, only present if curve
  parameters have been estimated and also discretised. Its
  keys are \sQuote{software}, \sQuote{version} and
  \sQuote{options}. The value of the former two is a
  character scalar. The value of \sQuote{options} is an
  arbitrarily nested mapping with arbitrary content.} }
  Details of the contents should be obvious from the
  documentation of the classes of the objects from which
  the \acronym{YAML} output is generated.  In the case of
  \acronym{YAML} input with several plates per file,
  \code{batch_opm} generates \acronym{YAML} output files
  containing a sequence of mappings as described above, one
  per plate, to keep a 1:1 relationship between input and
  output files.

  Attempting to generate \acronym{YAML} from input data
  with a wrong character encoding might cause \R to crash
  or hang. This problem was observed with \acronym{CSV}
  files that were generated on a distinct operating system
  and contained special characters such as German umlauts.
  It is then necessary to explicitly (and correctly)
  specify the encoding used in these files; see the
  \sQuote{file.encoding} option of \code{\link{opm_opt}}
  for how to do this.

  \acronym{JSON}, which is almost a subset of
  \acronym{YAML}, can also be generated, but has more
  restrictions. It is only recommended if a \acronym{YAML}
  parser is unavailable. It is also more delicate regarding
  the encoding of character strings.

  When inputting \acronym{YAML} files generated with the
  help of the \pkg{yaml} package (on which the \pkg{opm}
  implementation is based), or \acronym{JSON} files
  generated with the help of the \pkg{rjson} package, using
  other programming languages, a potential problem is that
  they, and \acronym{YAML} in general, lack a native
  representation of \code{NA} values. Such entries are
  likely to be misunderstood as \sQuote{NA} character
  scalars (if the \pkg{json} package or the \pkg{yaml}
  package prior to version 2.1.7 are used) or as
  \code{.na}, \code{.na.real}, \code{.na.logical} or
  \code{.na.character} character scalars (if more recent
  versions of the \pkg{yaml} package are used). Input
  functions in other programming languages should conduct
  according conversions. \pkg{opm} translates these values
  when converting a list to a \code{\link{OPM}} object.

  See \code{\link{as.data.frame}} regarding the generated
  \acronym{CSV}.
}
\examples{
test.files <- opm_files("omnilog")
if (length(test.files) > 0) { # if the files are found
  num.files <- length(list.files(outdir <- tempdir()))
  x <- batch_opm(test.files[1], outdir = outdir)
  stopifnot(length(list.files(outdir)) == num.files + 1, is.matrix(x))
  stopifnot(file.exists(x[, "outfile"]))
  stopifnot(test.files[1] == x[, "infile"])
  unlink(x[, "outfile"])
} else {
  warning("opm example data files not found")
}
}
\references{
  \url{http://www.yaml.org/}

  \url{http://www.json.org/}

  \url{http://www.biolog.com/}
}
\seealso{
  utils::read.csv yaml::yaml.load_file grDevices::Devices

  pkgutils::mypdf

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{glob_to_regex}},
      \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{IO}

