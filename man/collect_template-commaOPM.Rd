\name{collect_template,OPM}
\alias{collect_template,OPM}
\title{Collect template (OPM version)}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{selection}{Character vector passed to
  \code{\link{csv_data}}.}

  \item{add.cols}{\code{NULL} or character vector. If not
  empty, names of columns to add to the dataframe,
  initially containing \code{NA}.}
}
\value{
  Dataframe with one row and the number columns equal to
  the sum of the lengths of \code{selection} and
  \code{add.cols}.
}
\description{
  Collect a dataframe template assisting in later on adding
  metadata using \code{\link{include_metadata}}.
}
\examples{
data(vaas_1)
(x <- collect_template(vaas_1))
stopifnot(identical(dim(x), c(1L, 3L)))
(x <- collect_template(vaas_1, add.cols = c("A", "B")))
stopifnot(identical(dim(x), c(1L, 5L)))
# see include_metadata() for how to use this to add metadata information
}
\seealso{
  Other metadata-functions: \code{\link{collect_template}},
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
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}
}
\keyword{attribute}

\docType{methods}
\alias{collect_template,OPM-method}
\usage{
  \S4method{collect_template}{OPM}(object, 
    selection = c(SETUP, POS, FILE), add.cols = NULL) 
}
