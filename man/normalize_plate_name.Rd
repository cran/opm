\name{normalize_plate_name}
\alias{normalize_plate_name}
\title{Normalize plate name}
\usage{
  normalize_plate_name(plate, subtype = FALSE)
}
\arguments{
  \item{plate}{Character vector of original plate name(s).}

  \item{subtype}{Logical scalar. Keep the plate subtype
  indicator, if any?}
}
\value{
  Character vector of the same length than \code{plate}.
}
\description{
  Normalize the names of OmniLog(R) PM plates to the
  internally used naming scheme. Unrecognized names are
  returned unchanged. This needs not normally be called by
  the \pkg{opm} user but might be of interest.
}
\examples{
# Entirely unrecognized strings are returned as-is
x <- normalize_plate_name(letters)
stopifnot(identical(x, letters))

# Something more realistic
(x <- normalize_plate_name(y <- c("PM1", "PM-11C", "PMM04-a"), TRUE))
stopifnot(x != y)
}
\seealso{
  base::gsub

  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}},
  \code{\link{well_to_substrate}}
}
\keyword{character}
\keyword{utilities}

