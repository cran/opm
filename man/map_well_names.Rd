\name{map_well_names}
\alias{map_well_names}
\title{Map well names to substrates}
\usage{
  map_well_names(wells, plate, in.parens = FALSE,
    brackets = FALSE, paren.sep = " ", downcase = FALSE,
    ...)
}
\arguments{
  \item{wells}{Character vector of original well names
  (coordinates on the plate).}

  \item{plate}{Character scalar. The type of the plate. See
  \code{\link{plate_type}}.}

  \item{in.parens}{Logical scalar. See
  \code{\link{wells}}.}

  \item{brackets}{Logical scalar. See \code{\link{wells}}.}

  \item{paren.sep}{Character scalar. See
  \code{\link{wells}}.}

  \item{downcase}{Logical scalar. See \code{\link{wells}}.}

  \item{...}{Arguments that can be passed to both
  \code{\link{add_in_parens}} and
  \code{\link{trim_string}}.}
}
\value{
  Character vector.
}
\description{
  Translate well names (which are basically their
  coordinates on the plate) to substrate names, given the
  name of the plate.
}
\note{
  The user-level function is \code{\link{wells}}.
}
\keyword{internal}

