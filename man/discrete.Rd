\name{discrete}
\alias{discrete}
\title{Convert to discrete characters}
\arguments{
  \item{x}{Numeric vector or a \code{\link{MOA}} object
  convertible to a numeric vector. The data-frame method
  first calls \code{\link{extract}}, restricting the
  columns to the numeric ones.}

  \item{range}{In non-\code{gap} mode (see next argument)
  the assumed real range of the data; must contain all
  elements of \code{x}, but can be much wider. In
  \code{gap} mode, it must, in contrast, lie within the
  range of \code{x}. If \code{range} is set to \code{TRUE},
  the empirical range of \code{x} is used in non-\code{gap}
  mode. In \code{gap} mode, the range is determined using
  \code{\link{run_kmeans}} with the number of clusters set
  to \code{3} and then applying \code{\link{borders}} to
  the result.}

  \item{gap}{Logical scalar. If \code{TRUE}, always convert
  to binary or ternary characters, ignoring \code{states}.
  \code{range} then indicates a subrange of \code{x} within
  which character conversion is ambiguous and has to be
  treated as either missing information or intermediate
  character state, depending on \code{middle.na}. If
  \code{FALSE} (the default), apply an
  equal-width-intervals discretization with the widths
  determined from the number of requested \code{states} and
  \code{range}.}

  \item{output}{String determining the output mode:
  \sQuote{character}, \sQuote{integer}, \sQuote{logical},
  \sQuote{factor}, or \sQuote{numeric}.  \sQuote{numeric}
  simply returns \code{x}, but performs the range checks.
  One cannot combine \sQuote{logical} with \code{TRUE}
  values for both \sQuote{gap} and \sQuote{middle.na}.}

  \item{middle.na}{Logical scalar. Only relevant in
  \code{gap} mode: if \code{TRUE}, the middle value yields
  \code{NA} (uncertain whether negative or positive). If
  \code{FALSE}, the middle value lies between the left and
  the right one (i.e., a third character state meaning
  \sQuote{weak}). This is simply coded as 0-1-2 and thus
  cannot be combined with \sQuote{logical} as \code{output}
  setting.}

  \item{states}{Integer or character vector. Ignored in
  \code{gap} mode and if \code{output} is not
  \sQuote{character}. Otherwise, (i) a single-element
  character vector, which is split into its elements, (ii)
  a multi-element character vector which is used directly,
  or (iii) an integer vector indicating the elements to
  pick from the default character states. In the latter
  case, a single integer is interpreted as the upper bound
  of an integer vector starting at 1.}

  \item{as.labels}{Vector of data-frame indices. See
  \code{\link{extract}}.}

  \item{sep}{Character scalar. See \code{\link{extract}}.}

  \item{...}{Optional arguments passed between the methods
  or, if requested, to \code{\link{run_kmeans}} (except
  \code{object} and \code{k}, see there).}
}
\value{
  Double, integer, character or logical vector or factor,
  depending on \code{output}. For the matrix method, a
  matrix composed of a vector as produced by the numeric
  method, the original \code{dimensions} and the original
  \code{dimnames} attributes of \code{x}.
}
\description{
  Convert a vector of continuous characters to discrete
  ones. One of the uses of this functions is to create
  character data suitable for phylogenetic studies with
  programs such as PAUP* and RAxML. These accept only
  discrete characters with at most 32 states, coded as 0 to
  9 followed by A to V. For the full export one
  additionally needs \code{\link{phylo_data}}. The matrix
  method is just a wrapper that takes care of the matrix
  dimensions, and the data-frame method is a wrapper for
  that method.
}
\examples{
# Treat everything between 3.4 and 4.5 as ambiguous
(x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE))
stopifnot(identical(x, c("0", "0", "0", "?", "1")))

# Treat everything between 3.4 and 4.5 as intermediate
(x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE, middle.na = FALSE))
stopifnot(identical(x, c("0", "0", "0", "1", "2")))

# Boring example: real and possible range as well as the number of states
# to code the data have a 1:1 relationship
(x <- discrete(1:5, range = c(1, 5), states = 5))
stopifnot(identical(x, as.character(0:4)))

# Now fit the data into a potential range twice as large, and at the
# beginning of it
(x <- discrete(1:5, range = c(1, 10), states = 5))
stopifnot(identical(x, as.character(c(0, 0, 1, 1, 2))))

# Matrix and data-frame methods
x <- matrix(as.numeric(1:10), ncol = 2)
(y <- discrete(x, range = c(3.4, 4.5), gap = TRUE))
stopifnot(identical(dim(x), dim(y)))
(yy <- discrete(as.data.frame(x), range = c(3.4, 4.5), gap = TRUE))
stopifnot(y == yy)

# K-means based discretization of PM data
data(vaas_4)
x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  in.parens = FALSE)
head(y <- discrete(x, range = TRUE, gap = TRUE))
stopifnot(c("0", "?", "1") \%in\% y)
}
\references{
  Dougherty, J., Kohavi, R., Sahami, M. 1995 Supervised and
  unsupervised discretization of continuous features. In:
  Prieditis, A., Russell, S. (eds.) \emph{Machine Learning:
  Proceedings of the fifth international conference}.

  Ventura, D., Martinez, T. R. 1995 An empirical comparison
  of discretization methods. \emph{Proceedings of the Tenth
  International Symposium on Computer and Information
  Sciences}, p. 443--450.

  Bunuel, L. 1972 \emph{Le charme discret de la
  bourgeoisie.} France/Spain, 96 min.
}
\seealso{
  base::cut

  Other phylogeny-functions: \code{\link{phylo_data}},
  \code{\link{safe_labels}}
}
\keyword{category}
\keyword{character}

\docType{methods}
\alias{discrete-methods}
\alias{discrete,numeric-method}
\alias{discrete,MOA-method}
\alias{discrete,data.frame-method}
\usage{
  \S4method{discrete}{numeric}(x, range, gap = FALSE,
    output = c("character", "integer", "logical", "factor", "numeric"),
    middle.na = TRUE, states = 32L, ...) 

  \S4method{discrete}{MOA}(x, ...) 

  \S4method{discrete}{data.frame}(x, as.labels = NULL, sep = " ", 
    ...) 

}
