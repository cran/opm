\name{well_to_substrate}
\alias{well_to_substrate}
\title{Map well names to substrates}
\usage{
  well_to_substrate(plate, well = 1L:96L)
}
\arguments{
  \item{plate}{Character vector. The type(s) of the
  plate(s). See \code{\link{plate_type}}.
  \code{\link{normalize_plate_name}} is applied before
  searching for the substrate names, and partial matching
  is allowed.}

  \item{well}{Character vector of original well names
  (coordinates on the plate), or integer vector, or
  convertible to such.}
}
\value{
  Character vector or matrix (depending on the length of
  \code{plate}), containing \code{NA} values for plates and
  wells that could not be identified.
}
\description{
  Translate well names (which are basically their
  coordinates on the plate) to substrate names, given the
  name of the plate.
}
\examples{
x <- c("A01", "B10")
(y <- well_to_substrate("PM1", x))
stopifnot(nchar(y) > nchar(x))
}
\seealso{
  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}},
  \code{\link{normalize_plate_name}}
}
\keyword{utilities}

