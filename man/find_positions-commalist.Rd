\name{find_positions,list}
\alias{find_positions,list}
\title{Identify positions of substrates (list version)}
\arguments{
  \item{x}{Query list. Values are used as argument of
  \code{\link{find_positions}}. See there for further
  details and examples.}
}
\value{
  List of lists as returned by
  \code{\link{find_positions}}.
}
\description{
  Identify the positions of substrates, using a list as
  input (as, e.g., output by \code{\link{find_substrate}}.
}
\examples{
(x <- find_positions(find_substrate(c("a-D-Glucose", "a-D-Gloucose"))))
stopifnot(length(x[[1]]) > length(x[[2]]))
}
\seealso{
  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}},
  \code{\link{normalize_plate_name}},
  \code{\link{well_to_substrate}}
}
\keyword{utilities}

\docType{methods}
\alias{find_positions,list-method}
\usage{
  \S4method{find_positions}{list}(x) 
}
