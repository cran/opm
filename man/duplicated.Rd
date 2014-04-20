\name{duplicated}
\alias{anyDuplicated}
\alias{contains}
\alias{duplicated}
\title{Determine duplicated plates}
\arguments{
  \item{x}{\code{\link{OPMX}} or \code{\link{MOPMX}}
  object.}

  \item{incomparables}{Vector passed to \code{duplicated}
  from the \pkg{base} package. By default this is
  \code{FALSE}.}

  \item{what}{Indicating which parts of \code{x} should be
  compared. If a character scalar, the following entries
  are special: \describe{ \item{all}{Compares entire
  \code{OPM} objects.} \item{csv}{Compares the
  \acronym{CSV} data entries \code{setup_time} and
  \code{position} (see \code{\link{csv_data}}). Not for
  \code{\link{MOPMX}} objects.} \item{metadata}{Compares
  the entire metadata content.} \item{plate.type}{Compares
  the plate types (only for \code{\link{MOPMX}} objects).}
  } If \code{what} does not match any of these, or is not a
  character scalar at all, it is passed as \code{key}
  argument to \code{\link{metadata}}, and the resulting
  metadata subsets are compared. The following two
  arguments are only relevant in this case.}

  \item{exact}{Logical scalar passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar passed to
  \code{\link{metadata}}.}

  \item{...}{Optional arguments passed to \code{duplicated}
  from the \pkg{base} package. For \code{contains},
  optional arguments passed to \code{identical} from the
  \pkg{base} package, allowing for fine-control of
  identity.}

  \item{object}{\code{\link{OPMX}} object.}

  \item{other}{For the \code{\link{OPMX}} method of
  \code{contains}, an \code{\link{OPMX}} object used as
  query.}
}
\value{
  Logical vector in the case of \code{duplicated}, integer
  scalar in the case of \code{anyDuplicated}. \code{0} if
  no values are duplicated, the index of the first or last
  (depending on \code{fromLast}) duplicated object
  otherwise. \code{contains} returns a logical vector.
}
\description{
  Check whether some, or duplicated, \code{\link{OPM}}
  objects are contained within an \code{\link{OPMS}}
  object, or whether \code{\link{OPMX}} objects are
  contained within an \code{\link{MOPMX}} object. For
  reasons of consistency, the \code{\link{OPM}} methods
  always returns \code{FALSE} or \code{0}. Alternatively,
  query \code{\link{OPMX}} objects with other such objects.
}
\details{
  The \code{\link{OPMS}} and \code{\link{OPM}} methods of
  \code{contains} test, for instance, whether an
  \code{\link{OPM}} object is contained in an
  \code{\link{OPMS}} object. The length of the resulting
  logical vector is the length of \code{other}.
}
\examples{
# 'OPM' methods
(x <- duplicated(vaas_1)) # 1 element => nothing duplicated
stopifnot(identical(x, FALSE))

(x <- anyDuplicated(vaas_1))
stopifnot(identical(x, 0L)) # no complete plate duplicated
(x <- anyDuplicated(vaas_1, what = list("Strain", "Species")))
stopifnot(identical(x, 0L)) # no organisms duplicated

# 'OPMS' methods
stopifnot(!duplicated(vaas_4)) # => no complete plates duplicated
stopifnot(!duplicated(vaas_4, what = list("Species", "Strain")))
# => no organisms duplicated
stopifnot(duplicated(vaas_4, what = "Species") == rep(c(FALSE, TRUE), 2))
# => species duplicated
x <- vaas_4[c(1, 1)] # => complete plate duplicated
stopifnot(c(FALSE, TRUE) == duplicated(x))

stopifnot(identical(anyDuplicated(vaas_4), 0L))
stopifnot(identical(anyDuplicated(vaas_4, what = list("Strain")), 0L))
# => no strains duplicated
stopifnot(identical(anyDuplicated(vaas_4, what = list("Species")), 2L))
# => species duplicated
x <- vaas_4[c(1, 1)] # complete plate duplicated
stopifnot(identical(anyDuplicated(x), 2L))

## contains: 'OPMS'/'OPM' methods
(x <- contains(vaas_4, vaas_4[3])) # single one contained
stopifnot(length(x) == 1, x)
(x <- contains(vaas_4, vaas_4))
stopifnot(length(x) == 4, x) # all contained
(x <- contains(vaas_4[3], vaas_4)) # one of four contained
stopifnot(length(x) == 4, sum(x) == 1)
stopifnot(contains(vaas_4[3], vaas_4[3])) # identical OPM objects
stopifnot(!contains(vaas_4[3], vaas_4[2])) # non-identical OPM objects
}
\seealso{
  base::duplicated base::anyDuplicated base::identical

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{minmax}}, \code{\link{seq}},
  \code{\link{subset}}, \code{\link{thin_out}},
  \code{\link{well}}
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{anyDuplicated-methods}
\alias{anyDuplicated,MOPMX,ANY-method}
\alias{anyDuplicated,MOPMX-method}
\alias{anyDuplicated,MOPMX,missing-method}
\alias{anyDuplicated,OPM,ANY-method}
\alias{anyDuplicated,OPM-method}
\alias{anyDuplicated,OPM,missing-method}
\alias{anyDuplicated,OPMS,ANY-method}
\alias{anyDuplicated,OPMS-method}
\alias{anyDuplicated,OPMS,missing-method}
\alias{contains-methods}
\alias{contains,MOPMX,MOPMX-method}
\alias{contains,MOPMX,OPMX-method}
\alias{contains,OPM,OPM-method}
\alias{contains,OPM,OPMS-method}
\alias{contains,OPMS,OPM-method}
\alias{contains,OPMS,OPMS-method}
\alias{contains,OPMX,MOPMX-method}
\alias{duplicated-methods}
\alias{duplicated,MOPMX,ANY-method}
\alias{duplicated,MOPMX-method}
\alias{duplicated,MOPMX,missing-method}
\alias{duplicated,OPM,ANY-method}
\alias{duplicated,OPM-method}
\alias{duplicated,OPM,missing-method}
\alias{duplicated,OPMS,ANY-method}
\alias{duplicated,OPMS-method}
\alias{duplicated,OPMS,missing-method}
\usage{
  \S4method{anyDuplicated}{MOPMX,ANY}(x, incomparables, ...) 
  \S4method{anyDuplicated}{MOPMX,missing}(x, incomparables,
    ...) 
  \S4method{anyDuplicated}{OPM,ANY}(x, incomparables, ...) 
  \S4method{anyDuplicated}{OPM,missing}(x, incomparables, ...) 
  \S4method{anyDuplicated}{OPMS,ANY}(x, incomparables, ...) 
  \S4method{anyDuplicated}{OPMS,missing}(x, incomparables, ...) 

  \S4method{contains}{MOPMX,MOPMX}(object, other, ...) 
  \S4method{contains}{MOPMX,OPMX}(object, other, ...) 
  \S4method{contains}{OPM,OPM}(object, other, ...) 
  \S4method{contains}{OPM,OPMS}(object, other, ...) 
  \S4method{contains}{OPMS,OPM}(object, other, ...) 
  \S4method{contains}{OPMS,OPMS}(object, other, ...) 
  \S4method{contains}{OPMX,MOPMX}(object, other, ...) 

  \S4method{duplicated}{MOPMX,ANY}(x, incomparables,
    what = c("all", "plate.type", "metadata"), exact = TRUE, strict = FALSE,
    ...) 
  \S4method{duplicated}{MOPMX,missing}(x, incomparables, ...) 
  \S4method{duplicated}{OPM,ANY}(x, incomparables, ...) 
  \S4method{duplicated}{OPM,missing}(x, incomparables, ...) 
  \S4method{duplicated}{OPMS,ANY}(x, incomparables,
    what = c("all", "csv", "metadata"), exact = TRUE, strict = FALSE, ...) 
  \S4method{duplicated}{OPMS,missing}(x, incomparables, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
