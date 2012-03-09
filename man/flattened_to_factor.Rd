\name{flattened_to_factor}
\alias{flattened_to_factor}
\title{Factor from flattened data}
\arguments{
  \item{data}{Object as returned by \code{\link{flatten}}.}

  \item{sep}{Character scalar. Separator used for joining
  the columns together.}
}
\value{
  Factor with one entry per plate.
}
\description{
  Extract all plate-specifying information from a dataframe
  as created by \code{\link{flatten}}. If metadata have
  been included, these will be joined together; otherwise
  the plate identifiers (basically numbers) themselves are
  used.
}
\keyword{internal}

\docType{methods}
\alias{flattened_to_factor-methods}
\alias{flattened_to_factor,data.frame-method}
\usage{
  \S4method{flattened_to_factor}{data.frame}(object, sep = " ") 

}
