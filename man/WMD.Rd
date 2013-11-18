\docType{class}
\name{WMD}
\alias{FOE}
\alias{FOE-class}
\alias{MOA}
\alias{MOA-class}
\alias{OPMX}
\alias{OPMX-class}
\alias{WMD}
\alias{WMD-class}
\alias{YAML_VIA_LIST}
\alias{YAML_VIA_LIST-class}
\title{Virtual classes of the \pkg{opm} package}
\description{
  Classes that are virtual and thus are not directly dealt
  with by an \pkg{opm} user: \acronym{WMD}, \acronym{MOA},
  \acronym{FOE}, \acronym{OPMX} and \code{YAML_VIA_LIST}.
}
\details{
  \acronym{WMD} is an acronym for \sQuote{with metadata}.
  This is a virtual class facilitating the management of
  metadata. No objects can be created from it because
  metadata without data make not much sense. It is used by
  its child classes such as \code{\link{OPM}}.

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
  \acronym{CSV} files. Metadata might already be present in
  \acronym{YAML} files created by the \pkg{opm} package,
  however.

  \acronym{MOA} is an acronym for \sQuote{matrix or array}.
  \acronym{MOA} is a virtual class facilitating the
  implementation of functionality for both matrices and
  arrays. Methods defined for objects from the class can be
  applied to either kind of object. See
  \code{\link{map_values}} and \code{\link{map_names}} for
  usage examples.

  \acronym{FOE} is an acronym for \sQuote{formula or
  expression}. This is a virtual class facilitating the
  implementation of functionality for both formulae and
  expressions. Methods defined for objects from the class
  can be applied to either kind of object. See
  \code{\link{metadata.set}} and \code{\link{map_metadata}}
  for usage examples.

  \acronym{OPMX} stands for \sQuote{\acronym{OPM} or
  \acronym{OPMS}}. It is a virtual class containing helper
  methods mainly for plotting \code{\link{OPM}} and
  \code{\link{OPMS}} objects. See \code{\link{show}} and
  \code{\link{sort}} for usage examples.

  See \code{\link{to_yaml}} for a usage example of
  \code{YAML_VIA_LIST}. This is a virtual class
  facilitating the conversion to \acronym{YAML} format (or
  its subset, \acronym{JSON}). It can currently be used by
  any class that can be coerced to a list.
}
\seealso{
  methods::Methods base::matrix base::array
  base::expression stats::formula

  Other classes: \code{\link{MOPMX}}, \code{\link{OPM}},
  \code{\link{OPMA}}, \code{\link{OPMD}},
  \code{\link{OPMS}}
}
\keyword{classes}
\keyword{methods}

