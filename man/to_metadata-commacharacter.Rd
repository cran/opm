\name{to_metadata,character}
\alias{to_metadata,character}
\title{Input metadata (filename version)}
\arguments{
  \item{object}{Name of input file (character scalar).}

  \item{sep}{Character scalar. Field separator in input
  file. This and the following parameters are passed to
  \code{read.delim}.}

  \item{check.names}{Logical scalar.}

  \item{strip.white}{Logical scalar.}

  \item{stringsAsFactors}{Logical scalar.}

  \item{...}{Optional other arguments for
  \code{read.delim}.}
}
\value{
  Dataframe.
}
\description{
  Read metadata from an input file. This is only a thin
  wrapper of \code{read.delim} but contains some useful
  adaptations (such as \strong{not} converting strings to
  factors, and not modifying column names).
}
\examples{
(x <- to_metadata(list(a = 7:8, `b c` = letters[1:2])))
tmpfile <- tempfile()
write.table(x, tmpfile, row.names = FALSE, sep = "\\t")
(x1 <- read.delim(tmpfile))
(x2 <- to_metadata(tmpfile))
stopifnot(identical(names(x2), names(x)), !identical(names(x1), names(x)))
}
\seealso{
  default.stringsAsFactors read.delim

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}

  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}},
  \code{\link{map_metadata,OPMS}},
  \code{\link{map_metadata,WMD+function}},
  \code{\link{metadata}}, \code{\link{metadata-set}},
  \code{\link{metadata-set,OPMS+ANY+ANY}},
  \code{\link{metadata-set,OPMS+missing+list}},
  \code{\link{metadata-set,WMD+character+ANY}},
  \code{\link{metadata-set,WMD+list+list}},
  \code{\link{metadata-set,WMD+numeric+list}},
  \code{\link{metadata_chars}},
  \code{\link{metadata_chars,OPMS}},
  \code{\link{to_metadata}}
}
\keyword{IO}

\docType{methods}
\alias{to_metadata,character-method}
\usage{
  \S4method{to_metadata}{character}(object,
    sep = "\t", check.names = FALSE, strip.white = TRUE,
    stringsAsFactors = FALSE, ...) 
}
