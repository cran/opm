\name{guess_cex}
\alias{guess_cex}
\title{Estimate cex}
\arguments{
  \item{object}{Numeric vector.}
}
\value{
  Numeric vector.
}
\description{
  Guess a suitable \code{cex} parameter for
  \code{\link{level_plot}}. 0.5 is fine for the original
  number of wells (96).
}
\keyword{internal}

\docType{methods}
\alias{guess_cex-methods}
\alias{guess_cex,numeric-method}
\usage{
  \S4method{guess_cex}{numeric}(object) 

}
