\name{collect_template,OPMS}
\alias{collect_template,OPMS}
\title{Collect template (OPMS version)}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}

  \item{...}{Arguments passed to
  \code{\link{collect_template,OPM}}.}
}
\value{
  Dataframe with one row per contained plate. The number of
  columns is equal to the sum of the lengths of
  \code{selection} and \code{add.cols}.
}
\description{
  Collect a dataframe template assisting in later on adding
  metadata using \code{\link{include_metadata}}.
}
\examples{
data(vaas_4)
(x <- collect_template(vaas_4))
stopifnot(identical(dim(x), c(4L, 3L)))
(x <- collect_template(vaas_4, add.cols = c("A", "B")))
stopifnot(identical(dim(x), c(4L, 5L)))
# see include_metadata() for how to use this to add metadata information
}
\seealso{
  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{collect_template,OPM}},
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
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{attribute}

\docType{methods}
\alias{collect_template,OPMS-method}
\usage{
  \S4method{collect_template}{OPMS}(object, ...) 
}
