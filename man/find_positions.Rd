\name{find_positions}
\alias{find_positions}
\title{Identify positions of substrates}
\arguments{
  \item{x}{Query character vector.}
}
\value{
  List of character matrices (empty if nothing was found),
  with one row per position found, the plate name in the
  first column and the well name in the second. The names
  of this list correspond to \code{names}.
}
\description{
  Identify the positions of substrates, i.e. the plate(s)
  and well(s) in which they occur. The query names must be
  written exactly as used in the stored plate annotations.
  To determine their spelling, use
  \code{\link{find_substrate}}.
}
\examples{
# Compare correct and misspelled substrate name
(x <- find_positions(c("a-D-Glucose", "a-D-Gloucose")))
stopifnot(length(x[[1]]) > length(x[[2]]))
}
\seealso{
  Other naming-functions:
  \code{\link{find_positions,list}},
  \code{\link{find_substrate}},
  \code{\link{normalize_plate_name}},
  \code{\link{well_to_substrate}}
}
\keyword{utilities}

\docType{methods}
\alias{find_positions-methods}
\alias{find_positions,character-method}
\usage{
  \S4method{find_positions}{character}(x) 
}
