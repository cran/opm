\name{include_metadata}
\alias{include_metadata}
\title{Add metadata (from file or data frame)}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{md}{Dataframe containing keys as column names, or
  name of file from which to read the data frame. Handled
  by \code{\link{to_metadata}}.}

  \item{keys}{Character vector.}

  \item{replace}{Logical scalar indicating whether the
  previous metadata, if any, shall be replaced by the novel
  ones, or whether these shall be appended.}

  \item{skip.failure}{Logical scalar. Do not stop with an
  error message if (unambiguous) selection is impossible
  but raise a warning only?}

  \item{remove.keys}{Logical scalar. When including
  \code{md} in the metadata, discard the \code{keys}
  columns?}

  \item{...}{Optional argument passed to
  \code{\link{to_metadata}}.}
}
\value{
  Novel \code{\link{OPM}} object.
}
\description{
  Include metadata by mapping CSV data and column names in
  a data frame.
}
\examples{
data(vaas_1)
(x <- collect_template(vaas_1, add.cols = "Location")) # generate data frame
x[1, "Location"] <- "Braunschweig" # insert additional information
copy <- include_metadata(vaas_1, x) # include the data in new OPM object
stopifnot(is.null(metadata(vaas_1, "Location")))
stopifnot(identical(metadata(copy, "Location"), "Braunschweig"))
}
\seealso{
  Other metadata-functions: \code{\link{map_metadata}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata.set}},
  \code{\link{metadata.set}}, \code{\link{metadata_chars}}
}
\keyword{manip}

\docType{methods}
\alias{include_metadata-methods}
\alias{include_metadata,OPM-method}
\alias{include_metadata,OPMS-method}
\usage{
  \S4method{include_metadata}{OPM}(object, md, 
    keys = opm_opt("csv.keys"), replace = FALSE, skip.failure = FALSE, 
    remove.keys = TRUE, ...) 

  \S4method{include_metadata}{OPMS}(object, ...) 

}
