\name{read_microstation_opm}
\alias{read_microstation_opm}
\title{Read opm from old MicroStation output}
\usage{
  read_microstation_opm(filename)
}
\arguments{
  \item{filename}{Character scalar with the obvious
  meaning.}
}
\value{
  List of \code{\link{OPM}} objects.
}
\description{
  Read \pkg{opm} data from a single
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
  file. This must be a comma-separated file with a certain
  number of defined columns. There are only end-point
  measurements present: each resulting \code{\link{OPM}}
  object contains only a single time point.
}
\note{
  It is unclear whether \sQuote{Plate Number} can really
  serve as an identifier, as duplicates might be present.
}
\keyword{internal}

