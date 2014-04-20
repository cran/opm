\name{merge}
\alias{merge}
\alias{split}
\title{Merge or split plates}
\arguments{
  \item{x}{\code{\link{OPMX}} or \code{\link{MOPMX}}
  object.}

  \item{y}{For the \code{\link{OPMS}} method a numeric
  vector indicating the time(s) (in hours) between two
  subsequent plates. Must be positive throughout, and its
  length should fit to the number of plates (e.g., either
  \code{1} or \code{length(x) - 1} would work). If missing,
  \code{0.25} is used.

  If \code{x} is an \code{\link{OPM}} object, a missing or
  numeric \code{y} argument causes \code{merge} to just
  return \code{x} because there is nothing to merge. But
  \code{y} can be an \code{\link{OPM}} object in that case,
  which, if compatible, will be merged with \code{x}.

  For the \code{\link{MOPMX}} method, the optional \code{y}
  can be any object that can be convert to the class of
  \code{x} using \code{as}.}

  \item{sort.first}{Logical scalar. Sort the plates
  according to their setup times before merging?}

  \item{parse}{Logical scalar. Ignored unless
  \code{sort.first} is \code{TRUE}.  For sorting, parse the
  setup times using \code{strptime} from the \pkg{base}
  package? It is an error if this does not work, but see
  \sQuote{Details}.}

  \item{f}{For the \code{\link{OPMX}} methods, a factor or
  missing. If missing, the behaviour is special. Splitting
  is applied to the plates themselves and attempted
  according to the positions of substrates within series as
  revealed by \code{\link{substrate_info}} in
  \sQuote{concentration} mode.

  If a factor, \code{f} is used as in the default
  \code{split} method from the \pkg{base} package, yielding
  a list (\code{\link{MOPMX}} object) of single or multiple
  plates.

  If neither missing nor a factor, \code{f} is used as
  \code{key} argument of \code{\link{metadata}}. The
  resulting entries are pasted together per plate and
  converted to a factor used for splitting \code{x}.

  For the \code{\link{MOPMX}} methods, f is a factor, a
  list of factors, or an object suitable as
  \code{\link{metadata}} key. If a factor, it is directly
  used for splitting \code{x}. If a list of factors, the
  factor lengths must correspond to the lengths of the
  elements of \code{x}, in turn. Each element of \code{x}
  is then split separately, and the resulting
  \code{\link{MOPMX}} objects are reassigned, yielding a
  list with one \code{\link{MOPMX}} object per factor
  level. Factor levels that do not occur in some of the
  elements of \code{x} are dropped, with a warning,
  independent of \code{drop} argument.

  If \code{f} is neither a factor nor a list of factors,
  such a list of factors is generated from the metadata,
  with \code{NULL} results replaced by \code{NA}.}

  \item{drop}{Passed to \code{\link{[}}. The default is
  \code{FALSE}.}
}
\value{
  The \code{\link{OPMX}} method of \code{merge} yields an
  \code{\link{OPM}} object. The \code{\link{metadata}} and
  \code{\link{csv_data}} will be taken from the first
  contained plate, but aggregated values, if any, will be
  dropped.

  The \code{\link{MOPMX}} method for \code{merge} yields a
  \code{\link{MOPMX}} object with a potentially different
  number of elements.

  The \code{split} methods yield either an
  \code{\link{OPMS}} or an \code{\link{MOPMX}} object; the
  \code{\link{MOPMX}} method for \code{split} yields a list
  of \code{\link{MOPMX}} objects.
}
\description{
  Combine all plates in a single \code{\link{OPM}} object
  by treating them as originating from subsequent runs of
  the same experimental plate. Adjust the times
  accordingly. Alternatively, split plates according to the
  contained regular series of substrates, if any. The
  \code{\link{MOPMX}} method merges according to plate
  types, optionally including a novel element.
}
\details{
  This \code{\link{OPMS}} method of \code{merge} is
  intended for dealing with slowly growing or reacting
  organisms that need to be analysed with subsequent runs
  of the same plate in \acronym{PM} mode. Results obtained
  with \emph{Geodermatophilus} strains and Generation-III
  plates indicate that this works well in practice. See the
  references, and see the documentation of the
  \code{montero_et_al} data set in the \pkg{opmdata}
  package.

  See the arguments \code{time.fmt} and \code{time.zone} of
  \code{\link{opm_opt}} for modifying the parsing of
  setup-time entries. If it does not work, additional
  time-string templates must be stored.

  The \code{CMAT} method of \code{merge} is only for
  internal use.

  The \code{split} methods with missing \code{f} are for
  splitting plates that contain series of substrate-usage
  assays as indicated in the full substrate names (mostly
  interpretable as concentrations).  \code{\link{OPMS}}
  objects are generated that contain each replicate within
  the series in a separate plate and the replicate
  \acronym{ID} indicated in the metadata entry given by
  \code{\link{opm_opt}("series.key")}. This allows for
  comparisons between within-plate replicates.

  Splitting according to substrate series will not work if
  these are not regular, i.e. the same substrates occur in
  each replicate. In such cases \code{x} will be returned,
  with a warning. Substrates without a replicate
  (\sQuote{concentration}) indicator would silently be
  skipped, however. The composition and order of the wells
  per pseudo-plate must be made uniform.  This is done by
  enforcing well names and well ordering of the first
  replicate in all forthcoming replicates.

  After a successful splitting, the numeric suffixes in the
  full well names make no sense any more, as each plate
  contains a constant set of such suffixes. The
  \code{no.num} argument of \code{\link{wells}} and the
  dependent methods can be used to remove the suffixes
  before displaying the full well names.

  The \code{\link{MOPMX}} method for \code{merge} will
  raise an error if elements occur within \code{x} (and
  optionally \code{y}) that have the same plate type but
  cannot be combined any way because they contain distinct
  sets of wells. See the comments on combining plates into
  a \code{\link{OPMS}} object.
}
\examples{
## merge: OPM methods
stopifnot(identical(merge(vaas_1, 0.5), vaas_1)) # nothing to merge
summary(x <- merge(vaas_1, vaas_1)) # biologically unreasonable!
stopifnot(is(x, "OPM"), dim(x) == c(2 * hours(vaas_1, "size"), 96))

## merge: OPMS methods
summary(x <- merge(vaas_4)) # biologically unreasonable for these data!
stopifnot(is(x, "OPM"), dim(x) == c(sum(hours(vaas_4, "size")), 96))

# See opmdata::montero_et_al for an object to which this can be sensibly
# applied. An according example is given in the montero_et_al documentation.

## split: OPM methods
(x <- split(vaas_1))
metadata(x, opm_opt("series.key"))
stopifnot(is(x, "OPMS"), dim(x) == c(2, hours(vaas_1, "size"), 1))
# only D-Serine is present as series, all other wells are skipped
# thus split is more useful when applied to other plate types such as "ECO"

(x <- split(vaas_1, "Species"))
stopifnot(is(x, "MOPMX"), length(x) == 1)

## split: OPMS methods
(x <- split(vaas_4))
metadata(x, opm_opt("series.key"))
stopifnot(is(x, "OPMS"), dim(x) == c(8, hours(vaas_4, "size")[1], 1))

(x <- split(vaas_4, "Species"))
stopifnot(is(x, "MOPMX"), length(x) == 2)

# Split into list of OPMS objects with the same overall measurement hours
x <- split(vaas_4, as.factor(hours(vaas_4)))
stopifnot(is(x, "MOPMX"), length(x) == 1, class(x[[1]]) == "OPMS")
# ... because the running times were actually already identical, the list
# contains only a single element.
}
\references{
  Montero-Calasanz, M. d. C., Goeker, M., Poetter, G.,
  Rohde, M., Sproeer, C., Schumann, P., Gorbushina, A. A.,
  Klenk, H.-P. 2012 \emph{Geodermatophilus arenarius} sp.
  nov., a xerophilic actinomycete isolated from Saharan
  desert sand in Chad. \emph{Extremophiles} \strong{16},
  903--909.

  Montero-Calasanz, M. d. C., Goeker, M., Rohde, M.,
  Schumann, P., Poetter, G., Sproeer, C., Gorbushina, A.
  A., Klenk, H.-P. 2013 \emph{Geodermatophilus siccatus}
  sp. nov., isolated from arid sand of the Saharan desert
  in Chad. \emph{Antonie van Leeuwenhoek} \strong{103},
  449--456.
}
\seealso{
  opmdata::montero_et_al

  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{extract}}, \code{\link{extract_columns}},
  \code{\link{flatten}}, \code{\link{oapply}},
  \code{\link{opmx}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{to_yaml}}, \code{\link{unique}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{merge-methods}
\alias{merge,CMAT,ANY-method}
\alias{merge,CMAT-method}
\alias{merge,CMAT,factor-method}
\alias{merge,CMAT,logical-method}
\alias{merge,MOPMX,ANY-method}
\alias{merge,MOPMX-method}
\alias{merge,MOPMX,missing-method}
\alias{merge,OPM,OPM-method}
\alias{merge,OPM,missing-method}
\alias{merge,OPM,numeric-method}
\alias{merge,OPMS,missing-method}
\alias{merge,OPMS,numeric-method}
\alias{split-methods}
\alias{split,MOPMX,ANY,ANY-method}
\alias{split,MOPMX-method}
\alias{split,MOPMX,ANY,missing-method}
\alias{split,MOPMX,factor,ANY-method}
\alias{split,MOPMX,factor-method}
\alias{split,MOPMX,factor,missing-method}
\alias{split,MOPMX,list,ANY-method}
\alias{split,MOPMX,list-method}
\alias{split,MOPMX,list,missing-method}
\alias{split,OPM,ANY,missing-method}
\alias{split,OPM,factor,ANY-method}
\alias{split,OPM,factor-method}
\alias{split,OPM,factor,missing-method}
\alias{split,OPM,missing,ANY-method}
\alias{split,OPM,missing-method}
\alias{split,OPM,missing,missing-method}
\alias{split,OPMS,ANY,missing-method}
\alias{split,OPMS,factor,ANY-method}
\alias{split,OPMS,factor-method}
\alias{split,OPMS,factor,missing-method}
\alias{split,OPMS,missing,ANY-method}
\alias{split,OPMS,missing-method}
\alias{split,OPMS,missing,missing-method}
\alias{split,OPMX,ANY,ANY-method}
\alias{split,OPMX-method}
\usage{
  \S4method{merge}{CMAT,ANY}(x, y) 
  \S4method{merge}{CMAT,factor}(x, y) 
  \S4method{merge}{CMAT,logical}(x, y) 
  \S4method{merge}{MOPMX,ANY}(x, y) 
  \S4method{merge}{MOPMX,missing}(x, y) 
  \S4method{merge}{OPM,OPM}(x, y, sort.first = TRUE,
    parse = TRUE) 
  \S4method{merge}{OPM,missing}(x, y, sort.first = TRUE,
    parse = TRUE) 
  \S4method{merge}{OPM,numeric}(x, y, sort.first = TRUE,
    parse = TRUE) 
  \S4method{merge}{OPMS,missing}(x, y, sort.first = TRUE,
    parse = TRUE) 
  \S4method{merge}{OPMS,numeric}(x, y, sort.first = TRUE,
    parse = TRUE) 

  \S4method{split}{MOPMX,ANY,ANY}(x, f, drop) 
  \S4method{split}{MOPMX,ANY,missing}(x, f, drop) 
  \S4method{split}{MOPMX,factor,ANY}(x, f, drop) 
  \S4method{split}{MOPMX,factor,missing}(x, f, drop) 
  \S4method{split}{MOPMX,list,ANY}(x, f, drop) 
  \S4method{split}{MOPMX,list,missing}(x, f, drop) 
  \S4method{split}{OPM,ANY,missing}(x, f, drop) 
  \S4method{split}{OPM,factor,ANY}(x, f, drop) 
  \S4method{split}{OPM,factor,missing}(x, f, drop) 
  \S4method{split}{OPM,missing,ANY}(x, f, drop) 
  \S4method{split}{OPM,missing,missing}(x, f, drop) 
  \S4method{split}{OPMS,ANY,missing}(x, f, drop) 
  \S4method{split}{OPMS,factor,ANY}(x, f, drop) 
  \S4method{split}{OPMS,factor,missing}(x, f, drop) 
  \S4method{split}{OPMS,missing,ANY}(x, f, drop) 
  \S4method{split}{OPMS,missing,missing}(x, f, drop) 
  \S4method{split}{OPMX,ANY,ANY}(x, f, drop) 
}
%% END INSERTION BY repair_S4_docu.rb
