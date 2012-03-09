\name{discrete}
\alias{discrete}
\title{Convert to discrete characters}
\arguments{
  \item{x}{Numeric vector or matrix.}

  \item{range}{In non-\code{gap} mode (see next argument)
  the assumed real range of the data; must contain all
  elements of \code{x}, but can be much wider. In
  \code{gap} mode, it must, in contrast, lie within the
  range of \code{x}. If \code{range} is set to \code{TRUE},
  the empirical range of \code{x} is used. This makes not
  much sense in \code{gap} mode, and a warning is issued in
  that case.}

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

  \item{...}{Arguments passed between the methods.}
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
  dimensions.
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

# Matrix method
x <- matrix(1:10, ncol = 2)
(y <- discrete(x, range = c(3.4, 4.5), gap = TRUE))
stopifnot(identical(dim(x), dim(y)))
}
\references{
  Dougherty J, Kohavi R, Sahami M. Supervised and
  unsupervised discretization of continuous features. In:
  Prieditis A, Russell S (eds) Machine Learning:
  Proceedings of the fifth international conference. 1995.

  Ventura D, Martinez TR. An empirical comparison of
  discretization methods. Proceedings of the Tenth
  International Symposium on Computer and Information
  Sciences 1995; 443-450.

  Bunuel L. Le charme discret de la bourgeoisie. 1972;
  France/Spain, 96 min.
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
\alias{discrete,matrix-method}
\usage{
  \S4method{discrete}{numeric}(x, range, gap = FALSE,
    output = c("character", "integer", "logical", "factor", "numeric"),
    middle.na = TRUE, states = 32L) 

  \S4method{discrete}{matrix}(x, ...) 

}
