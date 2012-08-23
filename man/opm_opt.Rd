\name{opm_opt}
\alias{opm_opt}
\title{OPM options}
\arguments{
  \item{x}{Character scalar or list. If not given, all
  current settings are returned (as a named list). If a
  list, it is expected to contain key-value pairs that can
  be set. In that case, it is an error if a key is unknown
  or if the value's class(es) is/are not compatible with
  the previously stored value's class(es). If \code{x} is a
  character scalar, it is used for querying for a value.}

  \item{...}{Optional arguments. If \code{x} is missing,
  these arguments are concatenated into a list and used as
  if \code{x} was given as a list (see above). That is, the
  argument names are used as the keys for setting values.}
}
\value{
  List or atomic vector.
}
\description{
  Get and set global \pkg{opm} options.
}
\details{
  The following keys can be used with the following kinds
  of values: \describe{ \item{color.borders}{Character
  vector with default color borders between which
  \code{\link{level_plot}} interpolates to obtain a colour
  palette.} \item{csv.keys}{Character vector with names of
  entries of \code{\link{csv_data}} be used by
  \code{\link{include_metadata}}.  Should be kept a subset
  of \code{opm_opt("csv.selection")}.}
  \item{csv.selection}{Character vector with names of
  entries of \code{\link{csv_data}} (must be a valid
  \sQuote{keys} argument) to be extracted by
  \code{\link{collect_template}}.} \item{digits}{Integer
  scalar. Number of digits used by some functions
  generating output text.} \item{gen.iii}{Logical scalar
  indicating whether \code{\link{read_opm}} and other IO
  functions based on it automatically convert to Generation
  III as plate type.} \item{phylo.fmt}{Character scalar
  indicating the default output format used by
  \code{\link{phylo_data}}.} \item{split}{Character scalar
  indicating the default spliiting characters used by
  \code{\link{separate}}.} \item{time.zone}{Character
  scalar indicating the time zone to be used when parsing
  \code{\link{setup_time}} entries. This is relevant for
  \code{\link{merge}}, which by default attempts to sort by
  parsed setup times} \item{time.fmt}{Character vector
  indicating the time formats used for parsing the
  \code{\link{setup_time}} entries (in the given order).
  Also relevant for \code{\link{merge}} by default.}
  \item{xy.colors}{Default color set used by the
  \code{\link{OPMS}} method of \code{\link{xy_plot}}.} } It
  is an error to set novel values whose classes are not
  identical to, or derived from, the classes of the old
  value. It is also an error to use a name that is not
  already contained (\pkg{opm} would never query for it
  anyway).
}
\examples{
# fetching a value
(gen.3 <- opm_opt("gen.iii"))
stopifnot(identical(gen.3, FALSE))

# setting a value; previous value is returned as list
(old.opts <- opm_opt(gen.iii = TRUE))
stopifnot(is.list(old.opts), length(old.opts) == 1L)
stopifnot(identical(old.opts$gen.iii, FALSE))

# fetching the value again: should now be changed
(gen.3 <- opm_opt("gen.iii"))
stopifnot(isTRUE(gen.3))

# resetting the value
(old.opts <- opm_opt(old.opts))
stopifnot(is.list(old.opts), length(old.opts) == 1L)
stopifnot(isTRUE(old.opts$gen.iii))
(gen.3 <- opm_opt("gen.iii"))
stopifnot(identical(gen.3, FALSE))
}
\seealso{
  base::options base::getOption

  Other auxiliary-functions: \code{\link{contains}},
  \code{\link{glob_to_regex}}, \code{\link{kubrick}},
  \code{\link{listing}}, \code{\link{map_names}},
  \code{\link{map_values}}, \code{\link{separate}}
}
\keyword{utilities}

\docType{methods}
\alias{opm_opt-methods}
\alias{opm_opt,list-method}
\alias{opm_opt,missing-method}
\alias{opm_opt,character-method}
\usage{
  \S4method{opm_opt}{list}(x) 

  \S4method{opm_opt}{missing}(x, ...) 

  \S4method{opm_opt}{character}(x) 

}