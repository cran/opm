\name{batch_opm_to_yaml}
\alias{batch_opm_to_yaml}
\title{Batch-convert to YAML (deprecated)}
\usage{
  batch_opm_to_yaml(names, md.args = NULL,
    aggr.args = NULL, force.aggr = FALSE, disc.args = NULL,
    force.disc = FALSE, gen.iii = opm_opt("gen.iii"), ...,
    verbose = TRUE, demo = FALSE)
}
\arguments{
  \item{names}{Character vector with names of files in one
  of the formats accepted by \code{\link{read_single_opm}},
  or names of directories containing such files, or both;
  or convertible to such a vector. See the \code{include}
  argument of \code{\link{read_opm}} and
  \code{\link{explode_dir}} for how to select subsets from
  the input files or directories.}

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
  \code{object}.  If \code{NULL}, discretization takes not
  place (but discretized data may already be present in
  case of \acronym{YAML} input).}

  \item{force.disc}{Logical scalar. If \code{FALSE}, do not
  discretize already discretized data (which can be present
  in \acronym{YAML} input).}

  \item{gen.iii}{Logical or character scalar. If
  \code{TRUE}, invoke \code{\link{gen_iii}} on each plate.
  This is automatically done with \acronym{CSV} input if
  the plate type is given as \sQuote{OTH} (which is usually
  the case for plates run in ID mode). If a character
  scalar, it is used as the \code{to} argument of
  \code{\link{gen_iii}} to set other plate types unless it
  is empty.}

  \item{...}{Optional arguments passed to
  \code{\link{batch_process}} in addition to \code{verbose}
  and \code{demo}. Note that \code{out.ext}, \code{fun} and
  \code{fun.args} are set automatically.}

  \item{verbose}{Logical scalar. Print conversion and
  success/failure information?}

  \item{demo}{Logical scalar. Do not read files, but print
  a vector with the names of the files that would be
  (attempted to) read, and return them invisibly?}
}
\value{
  The function invisibly returns a matrix which describes
  each attempted file conversion. See
  \code{\link{batch_process}} for details.
}
\description{
  \strong{Deprecated} function to batch-convert from
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} (or previous \pkg{opm} \acronym{YAML}) to
  \pkg{opm} \acronym{YAML}. Use \code{\link{batch_opm}}
  instead.
}
\details{
  This function is \strong{deprecated}; use
  \code{\link{batch_opm}} instead.
}
\note{
  This \strong{deprecated} function is for batch-converting
  many files; for writing a single object to a
  \acronym{YAML} file (or string), see
  \code{\link{to_yaml}}.
}
\examples{
# see batch_opm()
}
\references{
  \url{http://www.yaml.org/}

  \url{http://www.biolog.com/}
}
\seealso{
  utils::read.csv yaml::yaml.load_file

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}}
}
\keyword{IO}

