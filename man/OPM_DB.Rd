\docType{class}
\name{OPM_DB}
\alias{OPMA_DB}
\alias{OPMA_DB-class}
\alias{OPMD_DB}
\alias{OPMD_DB-class}
\alias{OPM_DB}
\alias{OPM_DB-class}
\title{Classes for \pkg{opm} database I/O}
\description{
  These child classes of \code{DBTABLES} from the
  \pkg{pkgutils} package hold intermediary objects that can
  be used for database input and output of
  \code{\link{OPMX}} objects. These classes are not
  normally directly dealt with by an \pkg{opm} user but are
  documented here for completeness. See
  \code{\link{opm_dbput}} for methods that internally use
  these classes for database I/O.
}
\details{
  See their documentation for details on \code{\link{OPMX}}
  objects themselves.  We here define the following
  additional classes: \describe{ \item{OPM_DB}{Holds all
  data that occur in an \code{\link{OPM}} object, or in
  several such objects as contained in an
  \code{\link{OPMS}} object.} \item{OPMA_DB}{Holds all data
  that occur in an \code{\link{OPMA}} object, or in several
  such objects as contained in an \code{\link{OPMS}}
  object.} \item{OPMD_DB}{Holds all data that occur in an
  \code{\link{OPMD}} object, or in several such objects as
  contained in an \code{\link{OPMS}} object.} } The
  inheritance relationships thus mirror those of the
  \code{\link{OPMX}} objects (with the exception of
  \code{\link{OPMS}}). Conversion with \code{as} is
  implemented from all \code{\link{OPMX}} classes to all
  classes defined here. Lists can also be converted
  provided they only contain \code{\link{OPMX}} objects (or
  lists of such objects).

  Conversion in the other direction, yielding one of the
  \code{\link{OPMX}} classes, is also implemented.
  Attempting to convert several plates to an
  \code{\link{OPMX}} class other than \code{\link{OPMS}}
  will yield an error, however, as well as trying to
  convert a single plate to \code{\link{OPMS}}, or several
  plates with distinct plate types. In contrast, conversion
  to a list will work in all instances, and such a list
  could further be processed with the \code{\link{opms}}
  function, irrespective of the number of plates contained.

  In contrast to the \code{\link{OPMX}} classes, the three
  ones defined here can be created using \code{new},
  yielding empty objects. These can neither be converted to
  \code{\link{OPMX}} objects nor combined with them using
  \code{c}. Instead, they are useful in conjunction with
  \code{by} from the \pkg{pkgutils} package with
  \code{do_inline} set to \code{TRUE}. They contain all
  \code{fkeys} information and can be filled using a
  suitable \code{FUN} argument.
}
\examples{
library(pkgutils)

## overview on the classes
showClass("OPM_DB")
showClass("OPMA_DB")
showClass("OPMD_DB")

## show all conversions with as()
showMethods("coerce", classes = c("OPM_DB", "OPMA_DB", "OPMD_DB"))

## conversions back and forth, OPMD as starting point
(x <- as(vaas_1, "OPMD_DB"))
(y <- as(x, "OPMD"))
stopifnot(
  dim(y) == dim(vaas_1),
  # numeric data remain except for rounding errors:
  all.equal(measurements(y), measurements(vaas_1)),
  all.equal(aggregated(y), aggregated(vaas_1)),
  all.equal(discretized(y), discretized(vaas_1)),
  # file names get normalized, hence CSV dat may get unequal:
  !isTRUE(all.equal(csv_data(y), csv_data(vaas_1)))
)
(y <- try(as(x, "OPMS"), silent = TRUE))
stopifnot(inherits(y, "try-error")) # does not work because only 1 plate

## conversions back and forth, OPMS as starting point
(small <- vaas_4[, , 11:15])
(x <- as(small, "OPMD_DB"))
(y <- as(x, "OPMS"))
stopifnot(sapply(1:length(y), # same values
  function(i) dim(y[i]) == dim(small[i])))
(y <- try(as(x, "OPMD"), silent = TRUE)) # does not work because > 1 plate
stopifnot(inherits(y, "try-error"))
(y <- as(x, "list")) # one can always go through a list
stopifnot(sapply(y, is, "OPMD")) # opms() could now be called

## one can create new objects without data
(y <- new("OPMD_DB"))
stopifnot(fkeys_valid(y), fkeys(y) == fkeys(x), !length(y))
# such objects cannot be converted to OPMX but can be filled using by()
}
\seealso{
  methods::Methods methods::new opm::opms

  Other classes: \code{\link{FOE}}, \code{\link{MOPMX}},
  \code{\link{OPM}}, \code{\link{OPMA}},
  \code{\link{OPMD}}, \code{\link{OPMS}},
  \code{\link{OPMX}}, \code{\link{OPM_MCP_OUT}},
  \code{\link{WMD}}, \code{\link{WMDS}},
  \code{\link{WMDX}},   \code{\link{YAML_VIA_LIST}}
}
\keyword{classes}
\keyword{database}
\keyword{methods}

