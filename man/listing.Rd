\name{listing}
\alias{listing}
\title{Nicer message listings}
\usage{
  listing(x, header = NULL, footer = NULL, begin = NULL,
    collapse = "\n", style = "list", ...)
}
\arguments{
  \item{x}{Object convertible via \code{unlist} to a
  vector.}

  \item{header}{\code{NULL} or character vector. Prepended
  to the result.}

  \item{footer}{\code{NULL} or character vector. Appended
  to the result.}

  \item{begin}{\code{NULL} or numeric or character vector.
  Prepended to each line except \code{header} and
  \code{footer}. If a numeric vector, the number of spaces.
  Otherwise converted to \sQuote{character} mode and used
  directly.}

  \item{collapse}{Character scalar. How to join the
  resulting vector elements.}

  \item{style}{Character scalar. Passed to
  \code{formatDL}.}

  \item{...}{Optional other arguments passed to
  \code{formatDL}.}
}
\value{
  Character vector.
}
\description{
  Create a nice-looking message listing. This is not
  normally directly called by an \pkg{opm} user but by,
  e.g., the scripts accompanying the package; see
  \code{\link{opm_files}} for details.
}
\examples{
x <- letters[1:5]
names(x) <- LETTERS[1:5]
message(y <- listing(x, header = "Five letters:", footer = "...end here",
  begin = 3))
stopifnot(is.character(y), length(y) == 1)
}
\seealso{
  base::message base::warning base::stop base::formatDL
}
\keyword{utilities}

