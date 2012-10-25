\name{best_cutoff}
\alias{best_cutoff}
\title{Determine best cutoff}
\arguments{
  \item{x}{Numeric matrix.}

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
  and is only useful for plotting complete optimization
  curves.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  If \code{combined} is \code{TRUE}, either a matrix or a
  vector: If \code{all} is \code{TRUE}, a two-column matrix
  with (i) the cutoffs examined and (ii) the resulting
  scores. If \code{all} is \code{FALSE}, a vector with the
  entries \sQuote{maximum} (the best cutoff) and
  \sQuote{objective} (the score it achieved). If
  \code{combined} is \code{FALSE}, either a list of
  matrices or a matrix. If \code{all} is \code{TRUE}, a
  list of matrices structures like the single matrix
  returned if \code{combined} is \code{TRUE}. If \code{all}
  is \code{FALSE}, a matrix with two colums called
  \sQuote{maximum} \sQuote{objective}, and one row per
  level of \code{y}.
}
\description{
  Determine the best cutoff for dividing a numeric matrix
  into two categories by minimizing within-group
  discrepancies. That is, for each combination of row group
  and column maximize the number of contained elements that
  are in the category in which most of the elements within
  this combination of row group and column are located.
}
\details{
  The scoring function to be maximized is calculated as
  follows. All values in \code{x} are divided into those
  larger then the cutoff and those at most large as the
  cutoff. For each combination of group and matrix column
  the frequencies of the two categories are determined, and
  the respective larger ones are summed up over all
  combinations. This value is then divided by the frequency
  over the entire matrix of the more frequent of the two
  categories. This is done to avoid trivial solutions with
  minimal and maximal cutoffs, causing all values to be
  placed in the same category.
}
\examples{
x <- matrix(c(5:2, 1:2, 7:8), ncol = 2)
grps <- c("a", "a", "b", "b")

# combined optimization
(y <- best_cutoff(x, grps))
stopifnot(is.numeric(y), length(y) == 2)
stopifnot(y[["maximum"]] < 4, y[["maximum"]] > 3, y[["objective"]] == 2)
plot(best_cutoff(x, grps, all = TRUE), type = "l")

# separate optimization
(y <- best_cutoff(x, grps, combined = FALSE))
stopifnot(is.matrix(y), dim(y) == c(2, 2))
stopifnot(y["a", "objective"] == 2, y["b", "objective"] == 2)
(y <- best_cutoff(x, grps, combined = FALSE, all = TRUE))
plot(y$a, type = "l")
plot(y$b, type = "l")
}
\seealso{
  stats::optimize

  Other discretization-functions: \code{\link{discrete}},
  \code{\link{do_disc}}
}
\keyword{category}
\keyword{character}

\docType{methods}
\alias{best_cutoff-methods}
\alias{best_cutoff,matrix,character-method}
\alias{best_cutoff,matrix,factor-method}
\usage{
  \S4method{best_cutoff}{matrix,character}(x, y, ...) 

  \S4method{best_cutoff}{matrix,factor}(x, y,
    combined = TRUE, lower = min(x, na.rm = TRUE),
    upper = max(x, na.rm = TRUE), all = FALSE) 

}
