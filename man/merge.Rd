\name{merge}
\alias{merge}
\title{Merge plates}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{y}{Numeric vector indicating the time(s) (in hours)
  between two subsequent plates. Must be positive
  throughout, and its length should fit to the number of
  plates (e.g., either \code{1} or \code{length(x) - 1}
  would work). If missing, \code{0.25} is used.}

  \item{sort.first}{Logical scalar. Sort the plates
  according to their setup times before merging?}

  \item{parse}{Logical scalar. Ignored unless
  \code{sort.first} is \code{TRUE}.  For sorting, parse the
  setup times using \code{strptime} from the \pkg{base}
  package? It is an error if this does not work, but see
  \sQuote{Details}.}
}
\value{
  \code{\link{OPM}} object. The \code{\link{metadata}} and
  \code{\link{csv_data}} will be taken from the first
  contained plate, but aggregated values, if any, will be
  dropped.
}
\description{
  Combine all plates in a single \code{\link{OPM}} object
  by treating them as originating from subsequent runs of
  the same experimental plate. Adjust the times
  accordingly.
}
\details{
  This \code{\link{OPMS}} method is intended for dealing
  with slowly growing or reacting organisms that need to be
  analyzed with subsequent runs of the same plate in PM
  mode. Results obtained with \emph{Geodermatophilus}
  strains and Generation-III plates indicate that this
  works well in practice. See the references.

  See the arguments \sQuote{time.fmt} and
  \sQuote{time.zone} of \code{\link{opm_opt}} for modifying
  the parsing of setup-time entries. If it does not work,
  additional time-string templates must be stored.

  The \code{CMAT} method is only for internal use.
}
\examples{
data(vaas_4) # merge() is biologically unreasonable for these data!
summary(x <- merge(vaas_4))
stopifnot(is(x, "OPM"), dim(x) == c(sum(hours(vaas_4, "size")), 96))
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
  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{oapply}},
  \code{\link{plates}}, \code{\link{rep}},
  \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{manip}

\docType{methods}
\alias{merge-methods}
\alias{merge,OPMS,numeric-method}
\alias{merge,OPMS,missing-method}
\alias{merge,CMAT,ANY-method}
\usage{
  \S4method{merge}{OPMS,numeric}(x, y, sort.first = TRUE,
    parse = TRUE) 

  \S4method{merge}{OPMS,missing}(x, sort.first = TRUE,
    parse = TRUE) 

  \S4method{merge}{CMAT,ANY}(x, y) 

}
