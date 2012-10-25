\name{read_opm_yaml}
\alias{read_opm_yaml}
\title{Read opm YAML file}
\usage{
  read_opm_yaml(filename)
}
\arguments{
  \item{filename}{Character scalar with the obvious
  meaning.}
}
\value{
  \code{\link{OPM}}, \code{\link{OPMA}} or
  \code{\link{OPMD}} object, depending on the presence of
  aggregated and discretized data.
}
\description{
  Read \pkg{opm} data from a single YAML file. Arbitrary
  nesting levels are allowed, i.e. the YAML input results
  in either a list representing the slots of an
  \code{\link{OPM}} or \code{\link{OPMA}} object, or a list
  whose direct or indirect sublists represent such an
  object. However, it is an error if non-list elements are
  encountered when traversing such lists, and if no
  \code{\link{OPM}} or \code{\link{OPMA}} objects can be
  found at all.
}
\references{
  \url{http://www.yaml.org/}
}
\keyword{internal}

