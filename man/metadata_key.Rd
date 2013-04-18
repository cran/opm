\name{metadata_key}
\alias{metadata_key}
\alias{metadata_key.character}
\alias{metadata_key.default}
\alias{metadata_key.factor}
\alias{metadata_key.formula}
\alias{metadata_key.list}
\title{Create metadata key}
\usage{
  metadata_key(x, to.formula, ...)

  \method{metadata_key}{default} (x, to.formula = FALSE,
    remove = NULL, ...)

  \method{metadata_key}{factor} (x, to.formula = FALSE,
    remove = NULL, ...)

  \method{metadata_key}{character} (x, to.formula = FALSE,
    remove = NULL, ...)

  \method{metadata_key}{list} (x, to.formula = FALSE,
    remove = NULL, ops = "+", ...)

  \method{metadata_key}{formula} (x, to.formula = FALSE,
    ..., full.eval = !to.formula, envir = parent.frame())
}
\arguments{
  \item{x}{List, formula, or atomic object.}

  \item{to.formula}{Logical scalar indicating whether
  conversion to a formula should be conducted.}

  \item{remove}{Names of elements to be deleted after
  conversion to list.}

  \item{ops}{Character vector containing the operators to
  use when converting a list to a formula. Recycled if
  necessary.}

  \item{full.eval}{Logical scalar indicating whether to
  evaluate the result.  Usually makes no sense for formulas
  here.}

  \item{envir}{Passed to \code{eval}.}

  \item{...}{Optional arguments passed to and from other
  methods.}
}
\value{
  List or character vector.
}
\description{
  A helper function for \code{\link{metadata}} and the
  methods that are dependent on it.
}
\keyword{internal}

