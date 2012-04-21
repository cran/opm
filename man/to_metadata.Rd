\name{to_metadata}
\alias{to_metadata}
\title{Input metadata}
\arguments{
  \item{object}{Name of input file (character scalar), or
  any object convertible to a data frame.}

  \item{stringsAsFactors}{Logical scalar passed to
  \code{as.data.frame}.}

  \item{optional}{Logical scalar passed to
  \code{as.data.frame} or \code{read.delim}.}

  \item{sep}{Character scalar. Field separator in input
  file. This and the following parameters are passed to
  \code{read.delim}.}

  \item{check.names}{Logical scalar.}

  \item{strip.white}{Logical scalar.}

  \item{...}{Optional other arguments for \code{read.delim}
  or \code{as.data.frame}.}
}
\value{
  Dataframe.
}
\description{
  The character method reads metadata from an input file
  and is only a thin wrapper for \code{read.delim} but
  contains some useful adaptations (such as \strong{not}
  converting strings to factors, and not modifying column
  names). The default method reads metadata from an object
  convertible to a data frame and is only a thin wrapper of
  \code{as.data.frame} but contains the same useful
  adaptations as the filename method.
}
\examples{
# Character method
(x <- to_metadata(list(a = 7:8, `b c` = letters[1:2])))
tmpfile <- tempfile()
write.table(x, tmpfile, row.names = FALSE, sep = "\\t")
(x1 <- read.delim(tmpfile))
(x2 <- to_metadata(tmpfile))
stopifnot(identical(names(x2), names(x)), !identical(names(x1), names(x)))

# Default method
x <- list(a = 7:8, `b c` = letters[1:2])
(x1 <- as.data.frame(x))
(x2 <- to_metadata(x))
stopifnot(!identical(names(x), names(x1)), identical(names(x), names(x2)))
}
\seealso{
  base::default.stringsAsFactors utils::read.delim
  base::as.data.frame

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}

  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}}, \code{\link{metadata}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata_chars}}
}
\keyword{IO}
\keyword{manip}

\docType{methods}
\alias{to_metadata-methods}
\alias{to_metadata,character-method}
\alias{to_metadata,ANY-method}
\usage{
  \S4method{to_metadata}{character}(object,
    sep = "\t", check.names = FALSE, strip.white = TRUE,
    stringsAsFactors = FALSE, ...) 

  \S4method{to_metadata}{ANY}(object, stringsAsFactors = FALSE,
    optional = TRUE, ...) 

}
