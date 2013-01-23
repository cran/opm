\docType{class}
\name{OPMD}
\alias{OPMD}
\alias{OPMD-class}
\title{OPMD class}
\description{
  Class for holding single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data together with aggregated and
  discretized values. For further details see its parent
  class, \code{\link{OPMA}}. \acronym{OPMD} is an acronym
  for \sQuote{\acronym{OPM}, discretized}.
}
\details{
  Objects of this class are usually created by calling
  \code{\link{do_disc}} on an \code{\link{OPMA}} object, or
  by inputting files with \code{\link{read_single_opm}} or
  \code{\link{read_opm}} if these files already contain
  discretized data.
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{MOA}}, \code{\link{OPM}},
  \code{\link{OPMA}}, \code{\link{OPMS}},
  \code{\link{OPMX}}, \code{\link{WMD}},
  \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

