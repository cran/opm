\name{listing}
\alias{listing}
\title{Nicer message listings}
\usage{
  listing(x, header = NULL, footer = NULL, begin = NULL,
    collapse = "\n", style = "list", force.numbers = FALSE,
    digits = opm_opt("digits"), ...)
}
\arguments{
  \item{x}{Object convertible via \code{unlist} to a
  vector. Afterwards its \sQuote{names} attribute is used
  as the first column of the resulting listing; if it is
  \code{NULL} or if \code{force.numbers} is \code{TRUE},
  numbers are inserted.}

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

  \item{style}{Character scalar. If \sQuote{table} or
  \sQuote{list}, passed to \code{formatDL}. Otherwise, a
  pattern for \code{sprintf} is assumed taking two
  arguments, the names of \code{x} and the values \code{x}
  (after conversion with \code{unlist}).}

  \item{force.numbers}{Logical scalar. Always use numbers
  instead of the \sQuote{names} attribute?}

  \item{digits}{Numeric scalar. Ignored unless \code{x} is
  numeric.}

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

x <- c("CTMT", "Chryseobacterium soli DSM 19298T",
  "Chryseobacterium soldanellicola DSM 17072T")
message(y <- listing(x, style = "\%s, \%s", collapse = "; "))
stopifnot(is.character(y), length(y) == 1)
}
\seealso{
  base::message base::warning base::stop base::formatDL

  Other auxiliary-functions: \code{\link{contains}},
  \code{\link{glob_to_regex}}, \code{\link{kubrick}},
  \code{\link{map_names}}, \code{\link{map_values}},
  \code{\link{opm_opt}}, \code{\link{separate}}
}
\keyword{utilities}

