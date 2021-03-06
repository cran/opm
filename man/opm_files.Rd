\name{opm_files}
\alias{opm_files}
\alias{param_names}
\alias{select_colors}
\title{Files, parameter names and colour sets used by the package}
\usage{
  opm_files(what = c("scripts", "testdata", "auxiliary", "demo", "doc", "css", "sql",
    "omnilog", "single", "multiple", "growth"))

  param_names(what = c("param.names", "disc.name", "reserved.md.names", "split.at"))
    
  select_colors(set = c("w3c", "w3c.i", "nora", "nora.i", "brewer", "brewer.i", "roseo",
    "roseo.i"))
}
\arguments{
  \item{what}{Character scalar indicating the subdirectory
  to search in or the kind of names to obtain. Currently
  the following subdirectories are included: \describe{
  \item{auxiliary}{Miscellaneous files which are neither
  executable scripts nor test data.} \item{css}{Predefined
  \acronym{CSS} files for \acronym{HTML} files generated
  by, e.g., \code{\link{phylo_data}}.} \item{doc}{The
  vignette (documentation) in several formats, including
  the extracted \R code.} \item{demo}{Example \R code using
  the \pkg{opm} package that neither fitted into these help
  pages nor into the vignette. Can directly be loaded via
  \code{demo}; see \code{demo(package = "opm")}.}
  \item{growth}{Growth-measurement example files.}
  \item{multiple}{Not directly readable (i.e.,
  multiple-plate) test files.} \item{omnilog}{Directly
  readable (i.e., single-plate) test files from
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  runs.} \item{scripts}{\R script files for non-interactive
  uses of the \pkg{opm} package, particularly for the batch
  processing of many files.  When called without input
  arguments or with the \sQuote{-h} switch, the scripts
  output usage information.} \item{single}{Directly
  readable (i.e., single-plate) test files.}
  \item{sql}{\acronym{SQL} files for working with
  relational databases.} \item{testdata}{Files as output by
  the devices such as the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  instrument.  Included here as examples for data input
  (and metadata management).} } and the following kinds of
  parameter names: \describe{ \item{param.names}{Names of
  the estimated curve parameters used internally and in the
  output.} \item{disc.name}{Alternative name used to select
  discretised values instead.}
  \item{reserved.md.names}{Names that should not be used in
  metadata entries because they are used as predefined
  column names by functions such as \code{\link{flatten}}.}
  \item{split.at}{The name of the column in data frames
  generated by \code{\link{extract}} that separates data
  from metadata.} }}

  \item{set}{Character scalar. Name of the colour vector to
  use. Colour vectors have been optimised for maximum
  contrast between adjacent colours, either manually or
  using \code{max_rgb_contrast} from the \pkg{pkgutils}
  package.  Names ending in \sQuote{.i} indicate vectors in
  inverse order (compared to the vector with the same name
  except \sQuote{.i}).}
}
\value{
  Character vector of file names or reserved parameter
  names or names of colours.
}
\description{
  Get list of files from the \pkg{opm} package of interest
  for the user, or get list of predefined parameter names
  of interest for the user, or select from some predefined
  colour sets for plotting.
}
\details{
  In addition to the results of \sQuote{reserved.md.names},
  it should be avoided to use metadata keys that start with
  a dot, as such keys might also be created intermediary by
  methods that have to compile metadata together with other
  information.

  Note that \code{pkg_files} might fail with very unusual
  installations of the \pkg{opm} package.

  See \code{\link{xy_plot}} for a usage example of
  \code{select_colors}. This function is not normally
  directly called by an \pkg{opm} user but could be used
  for testing before doing some serious plotting.
}
\examples{
## example input files
isRfile <- function(x) grepl("\\\.R$", x, ignore.case = TRUE)
(x <- opm_files("auxiliary"))
stopifnot(!isRfile(x))
(x <- opm_files("demo"))
stopifnot(isRfile(x))
(x <- opm_files("scripts"))
stopifnot(isRfile(x))
(x <- opm_files("testdata"))
stopifnot(!isRfile(x))
for (name in c("growth", "single", "multiple", "omnilog")) {
  print(y <- opm_files(name))
  stopifnot(y \%in\% x) # i.e., a subset of the set of all input example files
}

# On UNIX systems you should be able to do this if Rscript and the optparse
# package are properly installed:
# invisible(sapply(paste("Rscript", opm_files()), system))
# ...and get the usage messages of all scripts.

## reserved parameter names
(x <- param_names())
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
(x <- param_names("reserved"))
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
stopifnot(param_names("split.at") \%in\% x)

## colours
(x <- select_colors("nora"))
(y <- select_colors("nora.i")) # same in reverse order
stopifnot(is.character(x), length(x) > 0L, identical(x, rev(y)))
}
\references{
  \url{http://www.colorbrewer.org}
}
\seealso{
  pkgutils::pkg_files utils::demo grDevices::colors
  grDevices::rainbow grDevices::grey

  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}}, \code{\link{gen_iii}},
  \code{\link{listing}}, \code{\link{plate_type}},
  \code{\link{register_plate}},
  \code{\link{substrate_info}}, \code{\link{wells}}
}
\keyword{color}
\keyword{utilities}

