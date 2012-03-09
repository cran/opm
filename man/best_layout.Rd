\name{best_layout}
\alias{best_layout}
\title{Best two-dimensional layout}
\arguments{
  \item{object}{Numeric scalar.}

  \item{by}{Numeric scalar (width/height relation).}
}
\value{
  Numeric vector of length 2.
}
\description{
  Determine number of rows/columns in plot for given number
  of fields.
}
\keyword{internal}

\docType{methods}
\alias{best_layout-methods}
\alias{best_layout,numeric-method}
\usage{
  \S4method{best_layout}{numeric}(object, by = 0.75) 

}
