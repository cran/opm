\docType{class}
\name{OPMA}
\alias{OPMA}
\alias{OPMA-class}
\title{OPMA class}
\description{
  Class for holding single-plate OmniLog(R) phenotype
  microarray data together with aggregated values. For
  further details see its parent class, \code{\link{OPM}}.
  \sQuote{OPMA} is an acronym for \sQuote{OPM, aggregated}.
}
\details{
  Objects of this class are usually created by calling
  \code{\link{do_aggr}} on an \code{\link{OPM}} object, or
  by inputting files with \code{\link{read_single_opm}} or
  \code{\link{read_opm}} if these files already contain
  aggregated data.
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{MOA}}, \code{\link{OPM}},
  \code{\link{OPMD}}, \code{\link{OPMS}},
  \code{\link{OPMX}}, \code{\link{WMD}},
  \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

