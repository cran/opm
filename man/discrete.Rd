\name{discrete}
\alias{best_cutoff}
\alias{discrete}
\title{Discretisation functions}
\arguments{
  \item{x}{Numeric vector or array object convertible to a
  numeric vector. The data-frame method first calls
  \code{\link{extract}}, restricting the columns to the
  numeric ones. \code{best_cutoff} only accepts a numeric
  matrix.}

  \item{range}{If a numeric vector, in non-\code{gap} mode
  (see next argument) the assumed real range of the data;
  must contain all elements of \code{x}, but can be much
  wider. In \code{gap} mode, it must, in contrast, lie
  within the range of \code{x}.

  If \code{range} is set to \code{TRUE}, the empirical
  range of \code{x} is used in non-\code{gap} mode. In
  \code{gap} mode, the range is determined using
  \code{\link{run_kmeans}} with the number of clusters set
  to \code{3} and then applying \code{\link{borders}} to
  the result.

  The number of clusters is set to \code{2} if \code{range}
  is \code{FALSE} in \code{gap} mode.}

  \item{gap}{Logical scalar. If \code{TRUE}, always convert
  to binary or ternary characters, ignoring \code{states}.
  \code{range} then indicates a partial range of \code{x}
  within which character conversion is ambiguous and has to
  be treated as either missing information or intermediate
  character state, depending on \code{middle.na}.

  If \code{FALSE} (the default), apply an
  equal-width-intervals discretisation with the widths
  determined from the number of requested \code{states} and
  \code{range}.}

  \item{output}{String determining the output mode:
  \sQuote{character}, \sQuote{integer}, \sQuote{logical},
  \sQuote{factor}, or \sQuote{numeric}.  \sQuote{numeric}
  simply returns \code{x}, but performs the range checks.
  One cannot combine \sQuote{logical} with \code{TRUE}
  values for both \code{gap} and \code{middle.na}.}

  \item{middle.na}{Logical scalar. Only relevant in
  \code{gap} mode. In that case, if \code{TRUE}, the middle
  value yields \code{NA} (uncertain whether negative or
  positive).

  If \code{FALSE}, the middle value lies between the left
  and the right one (i.e., a third character state meaning
  \sQuote{weak}). This is simply coded as 0-1-2 and thus
  cannot be combined with \sQuote{logical} as \code{output}
  setting.}

  \item{states}{Integer or character vector. Ignored in
  \code{gap} mode and if \code{output} is not
  \sQuote{character}. Otherwise, the possible values are
  \itemize{ \item a single-element character vector, which
  is split into its elements; \item a multiple-element
  character vector which is used directly; \item an integer
  vector indicating the elements to pick from the default
  character states.  } In the latter case, a single integer
  is interpreted as the upper bound of an integer vector
  starting at 1.}

  \item{as.labels}{Vector of data-frame indexes. See
  \code{\link{extract}}.  (If given, this argument must be
  named.)}

  \item{sep}{Character scalar. See \code{\link{extract}}.
  (If given, this argument must be named.)}

  \item{y}{Factor or character vector indicating group
  affiliations. Its length must correspond to the number of
  rows of \code{x}.}

  \item{combined}{Logical scalar. If \code{TRUE}, determine
  a single threshold for the entire matrix. If
  \code{FALSE}, determine one threshold for each group of
  rows of \code{x} that corresponds to a level of
  \code{y}.}

  \item{lower}{Numeric scalar. Lower bound for the cutoff
  values to test.}

  \item{upper}{Numeric scalar. Upper bound for the cutoff
  values to test.}

  \item{all}{Logical scalar. If \code{TRUE}, calculate the
  score for all possible cutoffs for \code{x}. This is slow
  and is only useful for plotting complete optimisation
  curves.}

  \item{...}{Optional arguments passed between the methods
  or, if requested, to \code{\link{run_kmeans}} (except
  \code{object} and \code{k}, see there).}
}
\value{
  \code{discrete} generates a double, integer, character or
  logical vector or factor, depending on \code{output}. For
  the matrix method, a matrix composed of a vector as
  produced by the numeric method, the original
  \code{dimensions} and the original \code{dimnames}
  attributes of \code{x}.

  If \code{combined} is \code{TRUE}, \code{best_cutoff}
  yields either a matrix or a vector: If \code{all} is
  \code{TRUE}, a two-column matrix with (i) the cutoffs
  examined and (ii) the resulting scores. If \code{all} is
  \code{FALSE}, a vector with the entries \sQuote{maximum}
  (the best cutoff) and \sQuote{objective} (the score it
  achieved). If \code{combined} is \code{FALSE}, either a
  list of matrices or a matrix. If \code{all} is
  \code{TRUE}, a list of matrices structures like the
  single matrix returned if \code{combined} is \code{TRUE}.
  If \code{all} is \code{FALSE}, a matrix with two columns
  called \sQuote{maximum} \sQuote{objective}, and one row
  per level of \code{y}.
}
\description{
  These are the helper functions called by
  \code{\link{do_disc}} (which is the function normally
  applied by an \pkg{opm} user). \code{discrete} converts
  continuous numeric characters to discrete ones.
  \code{best_cutoff} determines the best cutoff for
  dividing a numeric matrix into two categories by
  minimising within-group discrepancies. That is, for each
  combination of row group and column maximise the number
  of contained elements that are in the category in which
  most of the elements within this combination of row group
  and column are located.
}
\details{
  One of the uses of \code{discrete} is to create character
  data suitable for phylogenetic studies with programs such
  as \acronym{PAUP*} and \acronym{RAxML}. These accept only
  discrete characters with at most 32 states, coded as 0 to
  9 followed by A to V. For the full export one
  additionally needs \code{\link{phylo_data}}. The matrix
  method is just a wrapper that takes care of the matrix
  dimensions, and the data-frame method is a wrapper for
  that method.

  The term \sQuote{character} as used here has no direct
  connection to the eponymous mode or class of \R. Rather,
  the term is borrowed from taxonomic classification in
  biology, where, technically, a single \sQuote{character}
  is stored in one column of a data matrix if each organism
  is stored in one row. Characters are the
  \emph{quasi-independent units} of evolution on the one
  hand and of phylogenetic reconstruction (and thus
  taxonomic classification) on the other hand.

  The scoring function to be maximised by
  \code{best_cutoff} is calculated as follows. All values
  in \code{x} are divided into those larger then the cutoff
  and those at most large as the cutoff. For each
  combination of group and matrix column the frequencies of
  the two categories are determined, and the respective
  larger ones are summed up over all combinations. This
  value is then divided by the frequency over the entire
  matrix of the more frequent of the two categories. This
  is done to avoid trivial solutions with minimal and
  maximal cutoffs, causing all values to be placed in the
  same category.
}
\examples{
# Treat everything between 3.4 and 4.5 as ambiguous
(x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE))
stopifnot(x == c("0", "0", "0", "?", "1"))

# Treat everything between 3.4 and 4.5 as intermediate
(x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE, middle.na = FALSE))
stopifnot(x == c("0", "0", "0", "1", "2"))

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

# K-means based discretisation of PM data (prefer do_disc() for this)
x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  in.parens = FALSE)
(y <- discrete(x, range = TRUE, gap = TRUE))[, 1:3]
stopifnot(c("0", "?", "1") \%in\% y)

## best_cutoff()
x <- matrix(c(5:2, 1:2, 7:8), ncol = 2)
grps <- c("a", "a", "b", "b")

# combined optimisation
(y <- best_cutoff(x, grps))
stopifnot(is.numeric(y), length(y) == 2) # two-element numeric vector
stopifnot(y[["maximum"]] < 4, y[["maximum"]] > 3, y[["objective"]] == 2)
plot(best_cutoff(x, grps, all = TRUE), type = "l")

# separate optimisation
(y <- best_cutoff(x, grps, combined = FALSE))
stopifnot(is.matrix(y), dim(y) == c(2, 2)) # numeric matrix
stopifnot(y["a", "objective"] == 2, y["b", "objective"] == 2)
(y <- best_cutoff(x, grps, combined = FALSE, all = TRUE))
plot(y$a, type = "l")
plot(y$b, type = "l")
}
\references{
  Dougherty, J., Kohavi, R., Sahami, M. 1995 Supervised and
  unsupervised discretisation of continuous features. In:
  Prieditis, A., Russell, S. (eds.) \emph{Machine Learning:
  Proceedings of the fifth international conference}.

  Ventura, D., Martinez, T. R. 1995 An empirical comparison
  of discretisation methods. \emph{Proceedings of the Tenth
  International Symposium on Computer and Information
  Sciences}, p. 443--450.

  Wiley, E. O., Lieberman, B. S. 2011 \emph{Phylogenetics:
  Theory and Practice of Phylogenetic Systematics}.
  Hoboken, New Jersey: Wiley-Blackwell.

  Bunuel, L. 1972 \emph{Le charme discret de la
  bourgeoisie.} France/Spain, 96 min.
}
\seealso{
  base::cut stats::optimize

  Other discretization-functions: \code{\link{do_disc}}
}
\keyword{category}
\keyword{character}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{best_cutoff-methods}
\alias{best_cutoff,matrix,character-method}
\alias{best_cutoff,matrix,factor-method}
\alias{discrete-methods}
\alias{discrete,array-method}
\alias{discrete,data.frame-method}
\alias{discrete,numeric-method}
\usage{
  \S4method{best_cutoff}{matrix,character}(x, y, ...) 
  \S4method{best_cutoff}{matrix,factor}(x, y,
    combined = TRUE, lower = min(x, na.rm = TRUE),
    upper = max(x, na.rm = TRUE), all = FALSE) 

  \S4method{discrete}{array}(x, ...) 
  \S4method{discrete}{data.frame}(x, ..., as.labels = NULL,
    sep = " ") 
  \S4method{discrete}{numeric}(x, range, gap = FALSE,
    output = c("character", "integer", "logical", "factor", "numeric"),
    middle.na = TRUE, states = 32L, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
