\name{listing}
\alias{listing}
\title{Create textual listing}
\arguments{
  \item{x}{\code{\link{OPMD}} or \code{\link{OPMS}}
  object.}

  \item{as.groups}{Vector or \code{NULL}. If non-empty,
  passed as eponymous argument to \code{\link{extract}}.
  Thus \code{TRUE} and \code{FALSE} can be used, creating
  either a single group or one per plate. The extracted
  metadata define groups for which the discretized data are
  aggregated.

  If \code{x} is an \code{\link{OPMD}} object and
  \code{as.groups} is not empty, it is used to create the
  row name of the single row of the resulting
  \sQuote{OPMS_Listing} object. Otherwise an
  \sQuote{OPMD_Listing} object is produced. It is currently
  possible but \strong{deprecated} to not set
  \code{as.groups} explicitly.}

  \item{cutoff}{Numeric scalar used if \sQuote{as.groups}
  is a list. If the relative frequency of the most frequent
  entry within the discretized values to be joined is below
  that cutoff, \code{NA} is used. Ignored if \code{x} is an
  \code{\link{OPMD}} object but added to the result if
  \code{as.groups} is non-empty.}

  \item{downcase}{Logical scalar passed to
  \code{\link{wells}}.}

  \item{full}{Logical scalar passed to
  \code{\link{wells}}.}

  \item{in.parens}{Logical scalar passed to
  \code{\link{wells}}.}

  \item{html}{Logical scalar. Convert to \acronym{HTML}?
  This involves Greek letters and paragraph (\sQuote{div})
  tags.}

  \item{sep}{Character scalar used for joining the
  \sQuote{as.groups} entries (if any).}

  \item{...}{Optional arguments passed between the methods
  or to \code{\link{wells}}.}

  \item{exact}{Logical scalar passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar also passed to
  \code{\link{metadata}}.}
}
\value{
  Character vector or matrix with additional class atribute
  \sQuote{OPMD_Listing} or \sQuote{OPMS_Listing}. See the
  examples for details.
}
\description{
  This creates a textual listing of the discretized values.
  This is useful to describe
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray results in a scientific manuscript.
}
\note{
  See \code{\link{do_disc}} for generating discretized
  data.
}
\examples{
# 'OPMD' method
data(vaas_1)

# this yields one sentence for each kind of reaction:
(x <- listing(vaas_1, NULL))
stopifnot(inherits(x, "OPMD_Listing"), is.character(x), length(x) == 3,
  !is.null(names(x)))

# create an 'OPMS_Listing' object
(y <- listing(vaas_1, ~ Species + Strain))
stopifnot(inherits(y, "OPMS_Listing"), is.matrix(y), dim(y) == c(1, 3),
  y == x, colnames(y) == names(x), !is.null(rownames(y)))

# including HTML tags
(y <- listing(vaas_1, NULL, html = TRUE))
stopifnot(inherits(y, "OPMD_Listing"), is.character(x), nchar(y) > nchar(x),
  !is.null(names(x)))

# 'OPMS' method
data(vaas_4)

# no grouping, no names (numbering used instead for row names)
(x <- listing(vaas_4[1:2], as.groups = NULL))
stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
(y <- listing(vaas_4[1:2], as.groups = FALSE)) # alternative
stopifnot(identical(x, y))

# in effect no grouping, but names
(x <- listing(vaas_4[1:2], as.groups = list("Species", "Strain")))
stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))

# only single group for all plates
(y <- listing(vaas_4[1:2], as.groups = TRUE))
stopifnot(inherits(y, "OPMS_Listing"), is.matrix(y), dim(y) == c(1, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))

# two groups
(x <- listing(vaas_4, as.groups = list("Species")))
stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
}
\seealso{
  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}},
  \code{\link{substrate_info}},
  \code{\link{well_to_substrate}}
}
\keyword{category}
\keyword{character}

\docType{methods}
\alias{listing-methods}
\alias{listing,OPMD-method}
\alias{listing,OPMS-method}
\usage{
  \S4method{listing}{OPMD}(x, as.groups = NULL,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) 

  \S4method{listing}{OPMS}(x, as.groups, cutoff = opm_opt("min.mode"),
    downcase = TRUE, full = TRUE, in.parens = FALSE, html = FALSE, sep = " ",
    ..., exact = TRUE, strict = TRUE) 

}
