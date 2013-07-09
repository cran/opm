\docType{class}
\name{OPMD}
\alias{OPMD}
\alias{OPMD-class}
\title{OPMD class}
\description{
  Class for holding single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data together with aggregated
  \strong{and} discretized values.
}
\details{
  Objects of this class are usually created by calling
  \code{\link{do_disc}} on an \code{\link{OPMA}} object, or
  by inputting files with \code{\link{read_single_opm}} or
  \code{\link{read_opm}} if these files already contain
  discretized data.

  The discretized data are considered as
  \sQuote{consistent} with the curve parameter from which
  they have been estimated if no \code{FALSE} value
  corresponds to curve parameter larger than the curve
  parameter of any \code{TRUE} value; \code{NA} values are
  not considered when checking consistency. The
  \sQuote{strict.OPMD} entry of \code{\link{opm_opt}}
  determines whether an error or only a warning is issued
  in the case of inconsistency.

  For further details see the parent class,
  \code{\link{OPMA}}.
}
\note{
  \acronym{OPMD} is an acronym for \sQuote{\acronym{OPM},
  discretized}.
}
\examples{
# conversion of a list to an OPMD object is tolerant against re-orderings
# and additions (but not against omissions)
x <- as(vaas_1, "list")
x$discretized <- c(Answer = 42L, rev(x$discretized), Text = LETTERS)
summary(x)
x <- as(x, "OPMD")
summary(x)
stopifnot(identical(discretized(x), discretized(vaas_1)))
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{FOE}}, \code{\link{MOA}},
  \code{\link{OPM}}, \code{\link{OPMA}},
  \code{\link{OPMS}}, \code{\link{OPMX}},
  \code{\link{WMD}}, \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

