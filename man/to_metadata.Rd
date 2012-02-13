\name{to_metadata}
\alias{to_metadata}
\title{Input metadata}
\arguments{
  \item{object}{Object convertible to a dataframe.}

  \item{stringsAsFactors}{Logical scalar passed to
  \code{as.data.frame}.}

  \item{optional}{Logical scalar passed to
  \code{as.data.frame}.}

  \item{...}{Optional arguments passed to
  \code{as.data.frame}.}
}
\value{
  Dataframe.
}
\description{
  Read metadata from an object convertible to a dataframe.
  This is only a thin wrapper of \code{as.data.frame} but
  contains some useful adaptations, such as \strong{not}
  converting strings to factors, and not modifying column
  names by default.
}
\examples{
x <- list(a = 7:8, `b c` = letters[1:2])
(x1 <- as.data.frame(x))
(x2 <- to_metadata(x))
stopifnot(!identical(names(x), names(x1)), identical(names(x), names(x2)))
}
\seealso{
  default.stringsAsFactors as.data.frame

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{phylo_data}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}},
  \code{\link{to_metadata,character}}

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
  \code{\link{to_metadata,character}}
}
\keyword{manip}

\docType{methods}
\alias{to_metadata-methods}
\alias{to_metadata,ANY-method}
\usage{
  \S4method{to_metadata}{ANY}(object, stringsAsFactors = FALSE, 
    optional = TRUE, ...) 
}
