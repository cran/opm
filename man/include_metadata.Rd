\name{include_metadata}
\alias{include_metadata}
\title{Add metadata (from file or dataframe)}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{md}{Dataframe containing keys as column names, or
  name of file from which to read the dataframe. Handled by
  \code{\link{to_metadata}} or
  \code{\link{to_metadata,character}}.}

  \item{keys}{Character vector.}

  \item{replace}{Logical scalar indicating whether the
  previous metadata, if any, shall be replaced by the novel
  ones, or whether these shall be appended.}

  \item{stop.on.error}{Logical scalar. Stop with an error
  message if (unambiguous) selection is impossible?
  Otherwise, create a warning only.}

  \item{remove.csv.data}{Logical scalar. Remove all names
  present in \code{csv_data}, not only those in
  \code{keys}?}

  \item{...}{Optional argument passed to
  \code{\link{to_metadata}} or
  \code{\link{to_metadata,character}}.}
}
\value{
  Novel \code{\link{OPM}} object.
}
\description{
  Include metadata by mapping CSV data and column names in
  a dataframe.
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
  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
  \code{\link{collect_template,OPMS}},
  \code{\link{extract_columns}},
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
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{manip}

\docType{methods}
\alias{include_metadata-methods}
\alias{include_metadata,OPM-method}
\usage{
  \S4method{include_metadata}{OPM}(object, md,
    keys = c(SETUP, POS), replace = FALSE, stop.on.error = TRUE,
    remove.csv.data = TRUE, ...) 
}
