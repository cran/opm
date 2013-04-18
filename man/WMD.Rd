\docType{class}
\name{WMD}
\alias{WMD}
\alias{WMD-class}
\title{WMD class}
\description{
  This is a virtual class facilitating the management of
  metadata. No objects can be created from it because
  metadata without data make not much sense. It is used by
  its child classes such as \code{\link{OPM}}, but it is
  not directly applied by an \pkg{opm} user.
}
\details{
  Conceptually, this class treats metadata as arbitrarily
  nested lists with arbitrary content. Containers of
  objects that inherit from this class are not forced to
  contain the same metadata entries. Problems might arise
  if such data are queried and attempted to be converted
  to, e.g., data frames because some values might be
  missing. But metadata can be queried beforehand for the
  keys as well as the values they contain, and other
  methods support setting, modifying and deleting metadata.

  For \code{\link{OPM}} and the other \pkg{opm} classes
  that use it, \sQuote{metadata} refers to information
  that, in contrast to, e.g., \code{\link{csv_data}} must
  be added by the user \strong{after} reading
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{CSV} files.  Metadata might already be present
  in \acronym{YAML} files created by the \pkg{opm} package,
  however.
}
\note{
  \acronym{WMD} is an acronym for \sQuote{with metadata}.
}
\seealso{
  methods::Methods

  Other classes: \code{\link{FOE}}, \code{\link{MOA}},
  \code{\link{OPM}}, \code{\link{OPMA}},
  \code{\link{OPMD}}, \code{\link{OPMS}},
  \code{\link{OPMX}}, \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

