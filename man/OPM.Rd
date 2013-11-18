\docType{class}
\name{OPM}
\alias{MOPMX}
\alias{MOPMX-class}
\alias{OPM}
\alias{OPM-class}
\alias{OPMA}
\alias{OPMA-class}
\alias{OPMD}
\alias{OPMD-class}
\alias{OPMS}
\alias{OPMS-class}
\title{Real classes of the \pkg{opm} package}
\description{
  Classes whose members can directly be generated and
  manipulated by an \pkg{opm} user: \acronym{OPM},
  \acronym{OPMA}, \acronym{OPMD} and \acronym{OPMS}.
}
\details{
  \acronym{OPM} is an acronym for
  \sQuote{OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  Phenotype Microarray}. This is the class for holding
  single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data without aggregated values, but
  with information read from the original input
  \acronym{CSV} files as well as an additional arbitrary
  amount of arbitrarily organised metadata. Objects of this
  class are usually created by inputting files with
  \code{\link{read_single_opm}} or \code{\link{read_opm}},
  not with a call to \code{new} or \code{as}.

  \acronym{OPM} inherits from \code{\link{WMD}} and, hence,
  has all its methods.

  Regarding the coercion of this class to other classes
  (see the \code{as} method from the \pkg{methods}
  package), consider the following: \itemize{ \item
  Conversion of its child classes to this class is
  straightforward, see the examples below.  \item The
  coercion of this class (and its child classes) to a list
  (and vice versa) is only for expert users and relies on a
  mapping between slot names and keys in the list, i.e. the
  list must be appropriately named. For instance, this is
  the mechanism when reading from and writing to
  \acronym{YAML}, see \code{\link{to_yaml}}.  \item
  Coercions to other data frames and matrices first coerce
  the \code{\link{measurements}} and then add the other
  slots as attributes.  \item Methods such as
  \code{\link{flatten}} and \code{\link{extract}} might be
  way more appropriate for converting \acronym{OPM}
  objects. }

  \acronym{OPMA} is an acronym for \sQuote{\acronym{OPM},
  aggregated}. This is the class for holding single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data together with aggregated
  values. Objects of this class are usually created by
  calling \code{\link{do_aggr}} on an \acronym{OPM} object,
  or by inputting files with \code{\link{read_single_opm}}
  or \code{\link{read_opm}} if these files already contain
  aggregated data.

  \acronym{OPMA} inherits from \acronym{OPM} and, hence,
  has all its methods.

  \acronym{OPMD} is an acronym for \sQuote{\acronym{OPM},
  discretised}. This is the class for holding single-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data together with aggregated
  \strong{and} discretised values. Objects of this class
  are usually created by calling \code{\link{do_disc}} on
  an \acronym{OPMA} object, or by inputting files with
  \code{\link{read_single_opm}} or \code{\link{read_opm}}
  if these files already contain discretised data.

  \acronym{OPMD} inherits from \acronym{OPMA} and, hence,
  has all its methods.

  The discretised data are considered as
  \sQuote{consistent} with the curve parameter from which
  they have been estimated if no \code{FALSE} value
  corresponds to curve parameter larger than the curve
  parameter of any \code{TRUE} value; \code{NA} values are
  not considered when checking consistency. The
  \code{strict.OPMD} entry of \code{\link{opm_opt}}
  determines whether an error or only a warning is issued
  in the case of inconsistency.

  \acronym{OPMS} is the class for holding multiple-plate
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data with or without aggregated or
  discretised values. Regarding the name: \acronym{OPMS} is
  just the plural of \acronym{OPM}. Objects of this class
  are usually created by calling \code{\link{opms}} or
  other combination functions on \acronym{OPM} or derived
  objects, or by inputting files with
  \code{\link{read_opm}} if these files altogether contain
  more than a single plate. \acronym{OPMS} objects are not
  normally created with a call to \code{new} or \code{as}.
  The data may have been obtained from distinct organisms
  and/or replicates, but \strong{must} correspond to the
  same plate type and \strong{must} contain the same wells.

  As a rule, \acronym{OPMS} has the same methods as the
  \acronym{OPM} class, but adapted to a collection of more
  than one \acronym{OPM} object. Also, \acronym{OPMS} can
  hold \acronym{OPMD} and \acronym{OPMA} as well as
  \acronym{OPM} objects, even though this is not indicated
  for all its methods in this manual.

  \acronym{MOPMX} is an object for holding \acronym{OPMX}
  objects with potentially multiple plate types. Regarding
  the name: the \acronym{OPMS} stands for
  \sQuote{multiple}. \acronym{MOPMX} objects are generated
  by \code{\link{read_opm}(convert = "grp")} and
  \code{\link{opms}(group = TRUE)}. They currently contain
  relatively few methods of their own and behave like
  lists. \acronym{MOPMX} objects can be created with
  \code{new} or \code{as} and then further manipulated; see
  the examples below.
}
\examples{
## OPMX conversions with as()
showMethods("coerce", classes = c("OPM", "OPMA", "OPMD"))
data(vaas_1)
data(vaas_4)
(x <- as(vaas_1, "OPMA")) # drops the discretised data
stopifnot(has_disc(vaas_1), !has_disc(x))
(x <- as(vaas_1, "OPM")) # drops the aggregated data
stopifnot(has_aggr(vaas_1), !has_aggr(x))

## MOPMX creation and conversion
(x <- new("MOPMX")) # don't do this with the other classes
(x <- as(vaas_1, "MOPMX"))
(x <- as(vaas_4, "MOPMX"))
# conversion backwards is only possible as long as the MOPMX object contains
# only a single OPMX object
}
\seealso{
  methods::Methods methods::new

  Other classes: \code{\link{FOE}}, \code{\link{MOA}},
  \code{\link{OPMX}}, \code{\link{WMD}},
  \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{methods}

