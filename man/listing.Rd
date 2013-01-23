\name{listing}
\alias{listing}
\title{Create textual listing}
\arguments{
  \item{x}{\code{\link{OPMD}} or \code{\link{OPMS}}
  object.}

  \item{downcase}{Logical scalar passed to
  \code{\link{wells}}.}

  \item{full}{Logical scalar passed to
  \code{\link{wells}}.}

  \item{in.parens}{Logical scalar passed to
  \code{\link{wells}}.}

  \item{as.groups}{List or \code{NULL}. If a list, passed
  as \sQuote{key} argument to \code{\link{metadata}}. The
  extracted metadata define groups for which the
  discretized data are aggregated.}

  \item{cutoff}{Numeric scalar used if \sQuote{as.groups}
  is a list. If the relative frequency of the most frequent
  entry within the discretized values to be joined is below
  that cutoff, \code{NA} is used.}

  \item{sep}{Character scalar used for joining the
  \sQuote{as.groups} entries (if any).}

  \item{exact}{Logical scalar passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar also passed to
  \code{\link{metadata}}.}

  \item{...}{Optional arguments passed between the methods
  or to \code{\link{wells}}.}
}
\value{
  Character vector or matrix. See the examples for details.
}
\description{
  This creates a textual listing of the discretized values.
  This is useful to describe
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray results in a manuscript.
}
\note{
  See \code{\link{do_disc}} for generating discretized
  data.
}
\examples{
# 'OPMD' method
data(vaas_1)

# this yields one sentence for each kind of reaction:
(x <- listing(vaas_1))
stopifnot(is.character(x), length(x) == 3, !is.null(names(x)))

# 'OPMS' method
data(vaas_4)

# no grouping, no names
(y <- listing(vaas_4, as.groups = NULL))
stopifnot(is.matrix(y), dim(y) == c(4, 3))
stopifnot(is.null(rownames(y)), !is.null(colnames(y)))

# in effect no grouping, but names
(y <- listing(vaas_4, as.groups = list("Species", "Strain")))
stopifnot(is.matrix(y), dim(y) == c(4, 3))
stopifnot(!is.null(rownames(y)), !is.null(colnames(y)))

# two groups
(y <- listing(vaas_4, as.groups = list("Species")))
stopifnot(is.matrix(y), dim(y) == c(2, 3))
stopifnot(!is.null(rownames(y)), !is.null(colnames(y)))
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
  \S4method{listing}{OPMD}(x, downcase = TRUE, full = TRUE,
    in.parens = FALSE, ...) 

  \S4method{listing}{OPMS}(x, as.groups, cutoff = 0.5, sep = " ",
    exact = TRUE, strict = TRUE, downcase = TRUE, full = TRUE,
    in.parens = FALSE, ...) 

}
