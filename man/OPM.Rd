\docType{class}
\name{OPM}
\alias{OPM}
\alias{OPM-class}
\title{OPM class}
\description{
  Class for holding single-plate OmniLog(R) phenotype
  microarray data without aggregated values, but with
  information read from the original input CSV files as
  well as an additional arbitrary amount of arbitrarily
  organized metadata. \sQuote{OPM} is an acronym for
  \sQuote{OmniLog(R) Phenotype Microarray}.
}
\details{
  Objects of this class are usually created by inputting
  files with \code{\link{read_single_opm}} or
  \code{\link{read_opm}}.
}
\note{
  Regarding the coercion of this class to other classes
  (see the \code{coerce} methods listed above and \code{as}
  from the \pkg{methods} package), consider the following:
  \itemize{ \item The coercion of this class (and its child
  classes) to a list (and vice versa) relies on a mapping
  between slot names and keys in the list, i.e. the list
  must be appropriately named. For instance, this is the
  mechanism when reading from and writing to YAML, see
  \code{\link{to_yaml}}.  \item Coercions to other data
  frames and matrices first coerce the
  \code{\link{measurements}} and then add the other slots
  as attributes.  \item Methods such as
  \code{\link{flatten}} might be more appropriate for
  converting \code{\link{OPM}} objects.  }
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{MOA}}, \code{\link{OPMA}},
  \code{\link{OPMD}}, \code{\link{OPMS}},
  \code{\link{OPMX}}, \code{\link{WMD}},
  \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

