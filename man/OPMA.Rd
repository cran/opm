\docType{class}
\name{OPMA}
\alias{OPMA}
\alias{OPMA-class}
\title{OPMA class}
\description{
  Class for holding single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data together with aggregated
  values.
}
\details{
  Objects of this class are usually created by calling
  \code{\link{do_aggr}} on an \code{\link{OPM}} object, or
  by inputting files with \code{\link{read_single_opm}} or
  \code{\link{read_opm}} if these files already contain
  aggregated data.

  For further details see the parent class,
  \code{\link{OPM}}.
}
\note{
  \acronym{OPMA} is an acronym for \sQuote{acronym{OPM},
  aggregated}.
}
\examples{
# conversion of a list to an OPMA object is tolerant against re-orderings
# and additions (but not against omissions)
x <- as(vaas_1, "list")
x$aggregated <- c(Answer = 42L, rev(x$aggregated), Text = LETTERS)
summary(x)
x <- as(x, "OPMA")
summary(x)
stopifnot(identical(aggregated(x), aggregated(vaas_1)))
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{FOE}}, \code{\link{MOA}},
  \code{\link{OPM}}, \code{\link{OPMD}},
  \code{\link{OPMS}}, \code{\link{OPMX}},
  \code{\link{WMD}}, \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

