\name{safe_labels}
\alias{safe_labels}
\title{Safe labels}
\usage{
  safe_labels(x, format, enclose = TRUE, pad = FALSE,
    comment = FALSE)
}
\arguments{
  \item{x}{Character vector or convertible to such.}

  \item{format}{Character scalar. See
  \code{\link{phylo_data}}.}

  \item{enclose}{Logical scalar. See
  \code{\link{phylo_data}} and the description of
  \code{comment}.}

  \item{pad}{Logical scalar. Bring labels to the same
  number of characters by appending spaces? Has no effect
  for \sQuote{phylip} and \sQuote{html} output format.}

  \item{comment}{Logical scalar. If \code{TRUE}, comments
  as used in the respective format will be produced.
  \sQuote{phylip} and \sQuote{epf} do not accept comments
  and will yield an error. If \code{enclose} is
  \code{TRUE}, the comment-enclosing characters are
  appended prepended to the vector, otherwise to each
  string seperately.}
}
\value{
  Character vector.
}
\description{
  Convert strings to safe phylogenetic taxon labels:
  replace disallowed characters or include all labels in
  single quotes, and double pre-existing single quotes, if
  any.
}
\details{
  This is not normally called directly by an \pkg{opm} user
  but by \code{\link{phylo_data}}; see there for further
  details.
}
\examples{
# Some animals you might know
x <- c("Elephas maximus", "Loxodonta africana", "Giraffa camelopardalis")

(y <- safe_labels(x, "phylip"))
stopifnot(nchar(y) == 10L) # truncation

(y <- safe_labels(x, "epf"))
stopifnot(nchar(y) == nchar(x)) # changes in length unnecessary
(y <- safe_labels(x, "epf", pad = TRUE))
stopifnot(nchar(y) == 22) # padded to uniform length

(y <- safe_labels(x, "nexus", enclose = TRUE))
stopifnot(grepl("^'.*'$", y)) # all strings enclosed in sinqle quotes
}
\seealso{
  base::gsu

  Other phylogeny-functions: \code{\link{html_args}},
  \code{\link{phylo_data}}
}
\keyword{character}

