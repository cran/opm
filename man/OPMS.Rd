\docType{class}
\name{OPMS}
\alias{OPMS}
\alias{OPMS-class}
\title{OPMS class}
\description{
  Class for holding multi-plate OmniLog(R) phenotype
  microarray data with or without aggregated values. The
  data may have been obtained from distinct organisms
  and/or replicates, but \strong{must} correspond to the
  same plate type and \strong{must} contain the same wells.
  Regarding the name: \sQuote{OPMS} is just the plural of
  \sQuote{OPM}.
}
\details{
  Objects of this class are usually created by calling
  \code{\link{opms}} or other combination functions on
  \code{\link{OPM}} or \code{\link{OPM}}-derived objects,
  or by inputting files with \code{\link{read_opm}} if
  these files altogether contain more than a single plate.
}
\note{
  As a rule, OPMS has the same methods as the
  \code{\link{OPM}} class, but adapted to a collection of
  more than one \code{\link{OPM}} object.  Only the
  additional ones and those with special arguments and/or
  behaviors are documented in detail. Also, OPMS can hold
  \code{\link{OPMA}} as well as \code{\link{OPM}} objects,
  even though this is not indicated for all its methods in
  this manual.
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{MOA}}, \code{\link{OPM}},
  \code{\link{OPMA}}, \code{\link{OPMD}},
  \code{\link{OPMX}}, \code{\link{WMD}},
  \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

