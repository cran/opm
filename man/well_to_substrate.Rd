\name{well_to_substrate}
\alias{well_to_substrate}
\title{Map well names to substrates}
\usage{
  well_to_substrate(plate, well = TRUE)
}
\arguments{
  \item{plate}{Character vector or factor. The type(s) of
  the plate(s). See \code{\link{plate_type}}.
  \code{\link{plate_type}} is applied before searching for
  the substrate names, and partial matching is allowed.}

  \item{well}{Character vector of original well names
  (coordinates on the plate), or integer vector, or
  convertible to such, or formula. The formula allows for
  sequences of well coordinates; see the examples for
  details.}
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
# formula yields same result
stopifnot(identical(y, well_to_substrate("PM1", ~ c(A01, B10))))
# using a sequence of well coordinates
stopifnot(nrow(well_to_substrate("PM1", ~ C02:C06)) == 5) # well sequence
stopifnot(nrow(well_to_substrate("PM1")) == 96) # all wells by default
}
\seealso{
  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}}, \code{\link{listing}},
  \code{\link{substrate_info}}
}
\keyword{utilities}

