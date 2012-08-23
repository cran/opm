\name{find_positions}
\alias{find_positions}
\title{Identify positions of substrates}
\arguments{
  \item{object}{Query character vector or query list.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  The character method returns a list of character matrices
  (empty if nothing was found), with one row per position
  found, the plate name in the first column and the well
  name in the second. The names of this list correspond to
  \code{names}. The list method returns lists of such
  lists.
}
\description{
  Identify the positions of substrates, i.e. the plate(s)
  and well(s) in which they occur. The query names must be
  written exactly as used in the stored plate annotations.
  To determine their spelling, use
  \code{\link{find_substrate}}.
}
\examples{
# Character method; compare correct and misspelled substrate name
(x <- find_positions(c("a-D-Glucose", "a-D-Gloucose")))
stopifnot(length(x[[1]]) > length(x[[2]]))

# Factor method
(y <- find_positions(as.factor(c("a-D-Glucose", "a-D-Gloucose"))))
stopifnot(identical(y, x))

# List method
(x <- find_positions(find_substrate(c("a-D-Glucose", "a-D-Gloucose"))))
stopifnot(length(x[[1]]) > length(x[[2]]))
}
\seealso{
  Other naming-functions: \code{\link{find_substrate}},
  \code{\link{substrate_info}},
  \code{\link{well_to_substrate}}
}
\keyword{utilities}

\docType{methods}
\alias{find_positions-methods}
\alias{find_positions,character-method}
\alias{find_positions,list-method}
\alias{find_positions,factor-method}
\usage{
  \S4method{find_positions}{character}(object) 

  \S4method{find_positions}{list}(object) 

  \S4method{find_positions}{factor}(object, ...) 

}
