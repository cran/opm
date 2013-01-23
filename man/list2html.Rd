\name{list2html}
\alias{list2html}
\title{Convert recursively to HTML}
\usage{
  list2html(x, level = 1L, fmt = opm_opt("html.class"),
    fac = 2L)
}
\arguments{
  \item{x}{List or other vector.}

  \item{level}{Integer scalar defining the starting level
  for indentation and naming of unnamed sections.}

  \item{fmt}{Character scalar used for transforming
  \code{level} into section \sQuote{class} and
  \sQuote{title} attributes.}

  \item{fac}{Integer scalar for inferring the number of
  spaces used for indentation from the current \code{level}
  (recursively incremented).}
}
\value{
  Character scalar.
}
\description{
  This is the helper function used by \code{\link{format}}
  for converting user-defined additions to \acronym{HTML}.
}
\details{
  If applied to lists, this functions works recursively,
  generating \sQuote{div} elements from each list. Names
  are used as \sQuote{class} and \sQuote{title} attributes.
  Where names are missing, \code{level} is used in
  conjunction with \code{fmt}. Non-list vectors are
  converted using \sQuote{span} tags if names are present,
  simply joined otherwise.
}
\keyword{internal}

