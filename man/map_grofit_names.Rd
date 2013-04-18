\name{map_grofit_names}
\alias{map_grofit_names}
\title{Grofit mapping}
\usage{
  map_grofit_names(subset = NULL, ci = TRUE, plain = FALSE,
    opm.fast = FALSE)
}
\arguments{
  \item{subset}{\code{NULL} or character vector. Use only
  these values?}

  \item{ci}{Logical scalar. Also return CI names?}

  \item{plain}{Logical scalar. Return the plain basenames
  only, ignoring \code{subset} and \code{ci}?}

  \item{opm.fast}{Logical scalar. Produce the mapping for
  the \sQuote{opm-fast} method instead?}
}
\value{
  Named list with old names as keys, new ones as values.
}
\description{
  Create a mapping for names of curve parameters.
}
\keyword{internal}

