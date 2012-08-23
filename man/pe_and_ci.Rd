\name{pe_and_ci}
\alias{pe_and_ci}
\alias{pe_and_ci.boot}
\title{CI and point-estimate calculation}
\usage{
  pe_and_ci(x, ...)

  \method{pe_and_ci}{boot} (x, ci = 0.95,
    as.pe = c("median", "mean", "pe"),
    type = c("basic", "perc", "norm"), fill.nas = FALSE,
    ...)
}
\arguments{
  \item{x}{Object of class \sQuote{boot}.}

  \item{ci}{Numeric scalar. See
  \code{\link{fast_estimate}}.}

  \item{as.pe}{Character scalar. See
  \code{\link{fast_estimate}}.}

  \item{type}{Character scalar. See \code{\link{boot.ci}}
  from the \pkg{boot} package.}

  \item{fill.nas}{Logical scalar. Assume that if the CI
  borders are both \code{NA} bootstrapping yielded constant
  values if the point estimate is not \code{NA}, and
  replace the CI borders by the point estimate in such
  cases.}

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

