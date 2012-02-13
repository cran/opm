\name{safe_labels}
\alias{safe_labels}
\title{Safe labels}
\usage{
  safe_labels(chars, format, enclose = TRUE)
}
\arguments{
  \item{chars}{Character vector or convertible to such.}

  \item{format}{Character scalar. See
  \code{\link{phylo_data}}.}

  \item{enclose}{Logical scalar. See
  \code{\link{phylo_data}}.}
}
\value{
  Character vector.
}
\description{
  Convert strings to safe phylogenetic taxon labels:
  replace disallowed characters or include all labels in
  single quotes, and double pre-existing single quotes, if
  any. This is not normally called directly by an \pkg{opm}
  user but by \code{\link{phylo_data}}; see there for
  further details.
}
\examples{
# Some animals you might know
x <- c("Elephas maximus", "Loxodonta africana", "Giraffa camelopardalis")

(y <- safe_labels(x, "phylip"))
stopifnot(nchar(y) == 10L)

(y <- safe_labels(x, "epf"))
stopifnot(nchar(y) == nchar(x))

(y <- safe_labels(x, "nexus", enclose = TRUE))
stopifnot(grepl("^'.*'$", y))
}
\seealso{
  gsub

  Other phylogeny-functions: \code{\link{discrete}},
  \code{\link{discrete,matrix}}, \code{\link{phylo_data}}
}
\keyword{character}

