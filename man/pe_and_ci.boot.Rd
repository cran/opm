\name{pe_and_ci.boot}
\alias{pe_and_ci.boot}
\title{CI and point-estimate calculation}
\usage{
  \method{pe_and_ci}{boot} (x, ci = 0.95,
    as.pe = c("median", "mean", "pe"),
    type = c("basic", "perc", "norm"), ...)
}
\arguments{
  \item{x}{Object of class \sQuote{boot}.}

  \item{ci}{Numeric scalar. See
  \code{\link{fast_estimate}}.}

  \item{as.pe}{Character scalar. See
  \code{\link{fast_estimate}}.}

  \item{type}{Character scalar. See \code{\link{boot.ci}}
  from the \pkg{boot} package.}

  \item{...}{Optional arguments passed to
  \code{\link{boot.ci}} from the \pkg{boot} package.}
}
\value{
  See \code{\link{fast_estimate}}.
}
\description{
  Get point estimates and CIs (if possible) from the result
  of \code{boot}.
}
\keyword{internal}

