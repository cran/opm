\name{substrate_info}
\alias{substrate_info}
\title{Provide information on substrates}
\arguments{
  \item{object}{Query character vector or query list.}

  \item{what}{Character scalar indicating which kind of
  information to output.  See the references for the
  background of each possible value.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  The character method returns a character vector with
  \code{object} used as names and either a matched entry or
  \code{NA} as value. The factor method does the same,
  whereas the list method traverses a list and calls
  \code{substrate_info} on its elements.
}
\description{
  Return information on substrates such as their CAS number
  or KEGG ID. The query names must be written exactly as
  used in the stored plate annotations. To determine their
  spelling, use \code{\link{find_substrate}}.
}
\note{
  Currently the information is incomplete, particularly for
  the PM-M plates.
}
\examples{
# Character method; compare correct and misspelled substrate name
(x <- substrate_info(c("a-D-Glucose", "a-D-Gloucose")))

# Factor method
(y <- substrate_info(as.factor(c("a-D-Glucose", "a-D-Gloucose"))))
stopifnot(identical(x, y))

# List method
(x <- substrate_info(find_substrate(c("a-D-Glucose", "a-D-Gloucose"))))
stopifnot(length(x[[1]]) > length(x[[2]]))
}
\references{
  Bochner, B. R., pers. comm.

  \url{http://en.wikipedia.org/wiki/CAS_registry_number}

  \url{http://www.genome.jp/kegg/}

  \url{http://metacyc.org/}
}
\seealso{
  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}},
  \code{\link{well_to_substrate}}
}
\keyword{utilities}

\docType{methods}
\alias{substrate_info-methods}
\alias{substrate_info,character-method}
\alias{substrate_info,list-method}
\alias{substrate_info,factor-method}
\usage{
  \S4method{substrate_info}{character}(object,
    what = c("cas", "kegg", "metacyc")) 

  \S4method{substrate_info}{list}(object, ...) 

  \S4method{substrate_info}{factor}(object, ...) 

}
