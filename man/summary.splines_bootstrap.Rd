\name{summary.splines_bootstrap}
\alias{summary.splines_bootstrap}
\title{Summary method for bootstraped splines}
\usage{
  summary.splines_bootstrap(object, ...)
}
\arguments{
  \item{object}{An object of class
  \code{splines_bootstrap}.}

  \item{...}{Further arguments. Currently not used.}
}
\value{
  vector of bootstrap confidence intervals
}
\description{
  Function for internal use; Creates confidence intervals
  based on bootstrap replicates.
}
\author{
  Benjamin Hofner
}
\keyword{internal}

