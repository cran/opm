
################################################################################
################################################################################
#
# Character discretization
#


#' Determine best cutoff
#'
#' Determine the best cutoff for dividing a numeric matrix into two categories
#' by minimizing within-group discrepancies. That is, for each combination of
#' row group and column maximize the number of contained elements that are in
#' the category in which most of the elements within this combination of row
#' group and column are located.
#'
#' @param x Numeric matrix.
#' @param y Factor or character vector indicating group affiliations. Its length
#'   must correspond to the number of rows of \code{x}.
#' @param combined Logical scalar. If \code{TRUE}, determine a single threshold
#'   for the entire matrix. If \code{FALSE}, determine one threshold for each
#'   group of rows of \code{x} that corresponds to a level of \code{y}.
#' @param lower Numeric scalar. Lower bound for the cutoff values to test.
#' @param upper Numeric scalar. Upper bound for the cutoff values to test.
#' @param all Logical scalar. If \code{TRUE}, calculate the score for all
#'   possible cutoffs for \code{x}. This is slow and is only useful for plotting
#'   complete optimization curves.
#' @param ... Optional arguments passed between the methods.
#' @return If \code{combined} is \code{TRUE}, either a matrix or a vector: If
#'   \code{all} is \code{TRUE}, a two-column matrix with (i) the cutoffs
#'   examined and (ii) the resulting scores. If \code{all} is \code{FALSE}, a
#'   vector with the entries \sQuote{maximum} (the best cutoff) and
#'   \sQuote{objective} (the score it achieved). If \code{combined} is
#'   \code{FALSE}, either a list of matrices or a matrix. If \code{all} is
#'   \code{TRUE}, a list of matrices structures like the single matrix returned
#'   if \code{combined} is \code{TRUE}. If \code{all} is \code{FALSE}, a matrix
#'   with two colums called \sQuote{maximum} \sQuote{objective}, and one row per
#'   level of \code{y}.
#'
#' @details The scoring function to be maximized is calculated as follows. All
#'   values in \code{x} are divided into those larger then the cutoff and those
#'   at most large as the cutoff. For each combination of group and matrix
#'   column the frequencies of the two categories are determined, and the
#'   respective larger ones are summed up over all combinations. This value is
#'   then divided by the frequency over the entire matrix of the more frequent
#'   of the two categories. This is done to avoid trivial solutions with minimal
#'   and maximal cutoffs, causing all values to be placed in the same category.
#'
#' @export
#' @family discretization-functions
#' @keywords character category
#' @seealso stats::optimize
#'
#' @examples
#' x <- matrix(c(5:2, 1:2, 7:8), ncol = 2)
#' grps <- c("a", "a", "b", "b")
#'
#' # combined optimization
#' (y <- best_cutoff(x, grps))
#' stopifnot(is.numeric(y), length(y) == 2) # two-element numeric vector
#' stopifnot(y[["maximum"]] < 4, y[["maximum"]] > 3, y[["objective"]] == 2)
#' plot(best_cutoff(x, grps, all = TRUE), type = "l")
#'
#' # separate optimization
#' (y <- best_cutoff(x, grps, combined = FALSE))
#' stopifnot(is.matrix(y), dim(y) == c(2, 2)) # numeric matrix
#' stopifnot(y["a", "objective"] == 2, y["b", "objective"] == 2)
#' (y <- best_cutoff(x, grps, combined = FALSE, all = TRUE))
#' plot(y$a, type = "l")
#' plot(y$b, type = "l")
#'
setGeneric("best_cutoff", function(x, y, ...) standardGeneric("best_cutoff"))

setMethod("best_cutoff", c("matrix", "character"), function(x, y, ...) {
  best_cutoff(x, as.factor(y), ...)
}, sealed = SEALED)

setMethod("best_cutoff", c("matrix", "factor"), function(x, y,
    combined = TRUE, lower = min(x, na.rm = TRUE),
    upper = max(x, na.rm = TRUE), all = FALSE) {
  indexes <- function(x) {
    y <- as.character(x)
    sapply(levels(x), function(level) which(y == level), simplify = FALSE)
  }
  all_cutoffs <- function(x) {
    x <- sort.int(unique(as.vector(x)))
    x[-1L] - diff(x) / 2
  }
  freq_score <- function(x) max(tabulate(x, nbins = 2L))
  freq_scores <- function(x) apply(x, 2L, freq_score)
  mat_freq_score <- function(x) {
    sum(unlist(lapply(y, function(i) freq_scores(x[i, , drop = FALSE])))) /
      freq_score(x)
  }
  mat_freq_score_2 <- function(x) sum(freq_scores(x)) / freq_score(x)
  opt_fun <- function(threshold) mat_freq_score((x > threshold) + 1L)
  opt_fun_2 <- function(threshold, x) mat_freq_score_2((x > threshold) + 1L)

  LL(all, upper, lower, combined)
  if (!any(duplicated(na.fail(L(y, .wanted = nrow(x))))))
    stop("'y' contains only singletons")

  y <- indexes(y)
  if (combined) {
    if (all)
      cbind(cutoff = cutoffs <- all_cutoffs(x),
        score = vapply(cutoffs, opt_fun, 1))
    else
      unlist(optimize(f = opt_fun, maximum = TRUE, lower = lower,
        upper = upper))
  } else if (all)
    lapply(y, function(i) {
      cutoffs <- all_cutoffs(m <- x[i, , drop = FALSE])
      cbind(cutoff = cutoffs,
        score = vapply(cutoffs, opt_fun_2, 1, x = m))
    })
  else
    do.call(rbind, lapply(y, function(i) {
      unlist(optimize(f = opt_fun_2, x = x[i, , drop = FALSE],
        maximum = TRUE, lower = lower, upper = upper))
    }))

}, sealed = SEALED)


################################################################################


#' Convert to discrete characters
#'
#' Convert a vector of continuous characters to discrete ones.
#'
#' @param x Numeric vector or a \code{\link{MOA}} object convertible to a
#'   numeric vector. The data-frame method first calls \code{\link{extract}},
#'   restricting the columns to the numeric ones.
#'
#' @param range If a numeric vector, in non-\code{gap} mode (see next argument)
#'   the assumed real range of the data; must contain all elements of \code{x},
#'   but can be much wider. In \code{gap} mode, it must, in contrast, lie within
#'   the range of \code{x}.
#'
#'   If \code{range} is set to \code{TRUE}, the empirical range of \code{x} is
#'   used in non-\code{gap} mode. In \code{gap} mode, the range is determined
#'   using \code{\link{run_kmeans}} with the number of clusters set to \code{3}
#'   and then applying \code{\link{borders}} to the result.
#'
#'   The number of clusters is set to \code{2} if \code{range} is \code{FALSE}
#'   in \code{gap} mode.
#'
#' @param gap Logical scalar. If \code{TRUE}, always convert to binary or
#'   ternary characters, ignoring \code{states}. \code{range} then indicates a
#'   subrange of \code{x} within which character conversion is ambiguous and has
#'   to be treated as either missing information or intermediate character
#'   state, depending on \code{middle.na}.
#'
#'   If \code{FALSE} (the default), apply an equal-width-intervals
#'   discretization with the widths determined from the number of requested
#'   \code{states} and \code{range}.
#'
#' @param output String determining the output mode: \sQuote{character},
#'   \sQuote{integer}, \sQuote{logical}, \sQuote{factor}, or \sQuote{numeric}.
#'   \sQuote{numeric} simply returns \code{x}, but performs the range checks.
#'   One cannot combine \sQuote{logical} with \code{TRUE} values for both
#'   \sQuote{gap} and \sQuote{middle.na}.
#'
#' @param middle.na Logical scalar. Only relevant in \code{gap} mode. In that
#'   case, if \code{TRUE}, the middle value yields \code{NA} (uncertain whether
#'   negative or positive).
#'
#'   If \code{FALSE}, the middle value lies between the left and the right one
#'   (i.e., a third character state meaning \sQuote{weak}). This is simply coded
#'   as 0-1-2 and thus cannot be combined with \sQuote{logical} as \code{output}
#'   setting.
#'
#' @param states Integer or character vector. Ignored in \code{gap} mode and if
#'   \code{output} is not \sQuote{character}. Otherwise, the possible values
#'   are \itemize{
#'   \item a single-element character vector, which is split into its elements;
#'   \item a multi-element character vector which is used directly;
#'   \item an integer vector indicating the elements to pick from the default
#'   character states.
#'   }
#'   In the latter case, a single integer is interpreted as the upper bound of
#'   an integer vector starting at 1.
#'
#' @param as.labels Vector of data-frame indices. See \code{\link{extract}}.
#' @param sep Character scalar. See \code{\link{extract}}.
#'
#' @param ... Optional arguments passed between the methods or, if requested, to
#'   \code{\link{run_kmeans}} (except \code{object} and \code{k}, see there).
#'
#' @details One of the uses of this functions is to create character data
#'   suitable for phylogenetic studies with programs such as \acronym{PAUP*} and
#'   \acronym{RAxML}. These accept only discrete characters with at most 32
#'   states, coded as 0 to 9 followed by A to V. For the full export one
#'   additionally needs \code{\link{phylo_data}}.
#'
#'   The matrix method is just a wrapper that takes care of the matrix
#'   dimensions, and the data-frame method is a wrapper for that method.
#'
#' @note The term \sQuote{character} as used here has nothing to do \emph{per
#'   se} with the eponymous mode or class of \R. Rather, the term is borrowed
#'   from taxonomic classification in biology, where, technically, a single
#'   \sQuote{character} is stored in one column of a data matrix if each
#'   organism is stored in one row. Characters are the \emph{quasi-independent
#'   units} of evolution on the one hand and of phylogenetic reconstruction (and
#'   thus taxonomic classification) on the other hand.
#'
#' @export
#' @return Double, integer, character or logical vector or factor, depending on
#'   \code{output}. For the matrix method, a matrix composed of a vector as
#'   produced by the numeric method, the original \code{dimensions} and the
#'   original \code{dimnames} attributes of \code{x}.
#' @family discretization-functions
#' @seealso base::cut
#' @keywords character category
#' @references Dougherty, J., Kohavi, R., Sahami, M. 1995 Supervised and
#'   unsupervised discretization of continuous features. In: Prieditis, A.,
#'   Russell, S. (eds.) \emph{Machine Learning: Proceedings of the fifth
#'   international conference}.
#' @references Ventura, D., Martinez, T. R. 1995 An empirical comparison of
#'   discretization methods. \emph{Proceedings of the Tenth International
#'   Symposium on Computer and Information Sciences}, p. 443--450.
#' @references Wiley, E. O., Lieberman, B. S. 2011 \emph{Phylogenetics: Theory
#'   and Practice of Phylogenetic Systematics}. Hoboken, New Jersey:
#'   Wiley-Blackwell.
#' @references Bunuel, L. 1972 \emph{Le charme discret de la bourgeoisie.}
#'   France/Spain, 96 min.
#'
#' @examples
#' # Treat everything between 3.4 and 4.5 as ambiguous
#' (x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE))
#' stopifnot(x == c("0", "0", "0", "?", "1"))
#'
#' # Treat everything between 3.4 and 4.5 as intermediate
#' (x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE, middle.na = FALSE))
#' stopifnot(x == c("0", "0", "0", "1", "2"))
#'
#' # Boring example: real and possible range as well as the number of states
#' # to code the data have a 1:1 relationship
#' (x <- discrete(1:5, range = c(1, 5), states = 5))
#' stopifnot(identical(x, as.character(0:4)))
#'
#' # Now fit the data into a potential range twice as large, and at the
#' # beginning of it
#' (x <- discrete(1:5, range = c(1, 10), states = 5))
#' stopifnot(identical(x, as.character(c(0, 0, 1, 1, 2))))
#'
#' # Matrix and data-frame methods
#' x <- matrix(as.numeric(1:10), ncol = 2)
#' (y <- discrete(x, range = c(3.4, 4.5), gap = TRUE))
#' stopifnot(identical(dim(x), dim(y)))
#' (yy <- discrete(as.data.frame(x), range = c(3.4, 4.5), gap = TRUE))
#' stopifnot(y == yy)
#'
#' # K-means based discretization of PM data (prefer do_disc() for this)
#' data(vaas_4)
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   in.parens = FALSE)
#' (y <- discrete(x, range = TRUE, gap = TRUE))[, 1:3]
#' stopifnot(c("0", "?", "1") %in% y)
#'
setGeneric("discrete", function(x, ...) standardGeneric("discrete"))

setMethod("discrete", "numeric", function(x, range, gap = FALSE,
    output = c("character", "integer", "logical", "factor", "numeric"),
    middle.na = TRUE, states = 32L, ...) {

  convert_states <- function(states) {
    if (length(states) == 0L)
      CHARACTER_STATES
    else if (is.numeric(states))
      if (length(states) > 1L)
        CHARACTER_STATES[states]
      else
        CHARACTER_STATES[seq(states)]
    else if (is.character(states))
      if (length(states) == 1L) {
        if (!nzchar(states))
          stop("'states' cannot be the empty string")
        unlist(strsplit(states, "", fixed = TRUE))
      } else if (any(nchar(states) != 1L))
        stop("'states' cannot contain strings of length other than one")
      else
        states
    else
      stop("'states' must be empty or character or numeric vector")
  }

  output <- match.arg(output)

  LL(gap, middle.na)

  if (isTRUE(range)) {
    range <- if (gap)
      borders(run_kmeans(object = x, k = 3L, ...))[[1L]]
    else
      range(x)
  } else if (identical(range, FALSE)) {
    if (gap)
      range <- borders(run_kmeans(object = x, k = 2L, ...))[[1L]]
    else
      stop("if 'gap' is FALSE, 'range' cannot be FALSE")
  } else {
    stopifnot(length(range) %in% c(1L, 2L))
    range <- sort.int(range)
  }

  if (gap) { # binary-state mode with a gap due to ambiguity

    x.range <- range(x)
    if (range[1L] < x.range[1L] || range[length(range)] > x.range[2L])
      stop("in 'gap' mode, 'range' must be within the range of 'x'")
    if (output == "numeric")
      return(x)
    tol <- .Machine$double.eps ^ 0.5
    breaks <- c(x.range[1L], c(range[1L] + tol, range[length(range)] - tol),
      x.range[2L] + tol)
    ints <- cut(x, breaks, labels = FALSE, right = FALSE)
    map <- if (middle.na)
      case(output,
        character = c("0", MISSING_CHAR, "1"),
        integer = c(0L, NA_integer_, 1L),
        logical = c(FALSE, NA, TRUE),
        factor = ordered(c(0L, NA_integer_, 1L))
      )
    else
      case(output,
        character = c("0", "1", "2"),
        integer = c(0L, 1L, 2L),
        logical = stop("one cannot combine 'logical' and 'middle.na'"),
        factor = ordered(c(0L, 1L, 2L))
      )
    structure(map[ints], cutoffs = range, .Names = names(x))

  } else { # binary- to multi-state mode without a gap

    if (any(x > range[2L] | x < range[1L]))
      stop("if not in 'gap' mode, all values must be between ", range[1L],
        " and ", range[2L])
    if (output == "numeric")
      return(x)
    states <- convert_states(states)
    ints <- if ((nstates <- length(states)) > 1L)
      cut(x = c(range[1L:2L], x), breaks = nstates, right = FALSE,
        labels = FALSE)[-1L:-2L]
    else
      rep.int(1L, length(x))
    structure(case(output,
      character = states[ints],
      integer = ints,
      logical = as.logical(ints - 1L),
      factor = ordered(ints)
    ), .Names = names(x))

  }
}, sealed = SEALED)

setMethod("discrete", MOA, function(x, ...) {
  map_values(object = x, mapping = discrete, ...)
}, sealed = SEALED)

setMethod("discrete", "data.frame", function(x, as.labels = NULL, sep = " ",
    ...) {
  discrete(extract_columns(x, what = "numeric", as.labels = as.labels,
    sep = sep, direct = FALSE), ...)
}, sealed = SEALED)


################################################################################


#' Discretize curve parameters
#'
#' Discretize the aggregated kinetic data, i.e. infer discrete values from the
#' curve parameters stored in an \code{\link{OPMA}} or \code{\link{OPMS}}
#' object. Here, only discretization into positive, negative and ambiguous
#' reactions is supported, and only based on the \sQuote{maximum height} curve
#' parameter (which is biologically reasonable though).
#'
#' @param object \code{\link{OPMA}} or \code{\link{OPMS}} object.
#' @param cutoff Determines the discretization approach. If non-empty, passed as
#'   \code{range} argument to \code{discrete} (with \code{gap} set to
#'   \code{TRUE}), thus triggering discretization using either k-means
#'   partitioning or one or two predefined thresholds. If empty (e.g.,
#'   \code{NULL}), a discretization cutoff is determined using
#'   \code{\link{best_cutoff}}, which is only possible for \code{\link{OPMS}}
#'   objects.
#' @param groups List, \code{NULL} or character vector passed as
#'   \sQuote{as.labels} argument to \code{\link{extract}}, or logical scalar.
#'   In that case, if \code{TRUE}, groups are automatically created with one
#'   plate per group. If \code{FALSE}, grouping is not used, i.e. there is
#'   only a single group containing all plates.
#'
#'   Note that if \code{cutoff} is empty and \code{groups} is \code{TRUE}, an
#'   error is raised since \code{\link{best_cutoff}} needs groups with more than
#'   a single element.
#'
#'   The \code{groups} argument has no effect on \code{\link{OPMA}} objects.
#'
#' @param plain Logical scalar indicating whether or not an \code{\link{OPMD}}
#'   or \code{\link{OPMS}} object should be created.
#' @param subset Character scalar passed to \code{\link{extract}}. It is
#'   recommended to use the maximum height (currently called \sQuote{A}).
#' @param unify Logical or numeric scalar indicating whether results should be
#'   unified per group. This works by choosing the most frequent value (mode)
#'   if its frequency is above a given threshold and \code{NA} otherwise. (The
#'   same approach is used by \code{\link{listing}} and
#'   \code{\link{phylo_data}}.)
#'
#'   If \code{unify} is a logical scalar, \code{NA} triggers unification
#'   using 1 as threshold, i.e. all ambiguities are codes as \code{NA}. Using
#'   \code{TRUE} turns on unification with the default threshold given by
#'   \code{opm_opt("min.mode")}, whereas \code{FALSE} turns unification off.
#'
#'   If \code{unify} is a numeric scalar, values below or equal to zero turn
#'   unification off. Values above zero are directly used as unification
#'   threshold, thus values above 1 or numeric \code{NA} make no sense (cause
#'   an error).
#'
#'   See \sQuote{Details} below on the potential consequences of unification. In
#'   the \code{\link{disc_settings}} entries, an according \sQuote{unified}
#'   entry will report the threshold used, with -1 indicating no unification.
#'
#'   The \code{unify} argument has no effect on \code{\link{OPMA}} objects
#'   (because they represent a single group with a single member).
#' @param ... Optional arguments passed to \code{\link{extract}}. Only relevant
#'   for certain settings of \code{groups}, see above.
#'
#' @details If \code{unify} is set to \code{FALSE}, the discretization results
#'   are always consistent (in the sense described for the \code{\link{OPMD}}
#'   class) with the discretized parameter. If \code{unify} is set to
#'   \code{TRUE} this cannot be guaranteed any more. To enforce consistency,
#'   use \code{opm_opt(strict.OPMD = TRUE)}.
#'
#' @note The discretized values can be queried for using \code{\link{has_disc}}
#'   and received using \code{\link{discretized}}.
#'
#' @export
#' @return If \code{plain} is \code{FALSE}, an \code{\link{OPMD}} or
#'   \code{\link{OPMS}} object. Otherwise a logical vector whose length
#'   corresponds to the number of wells in \code{object} with an additional
#'   \sQuote{settings} attribute describing the run. The vector and its
#'   attribute would correspond to the \code{\link{discretized}} and
#'   \code{\link{disc_settings}} entries of a resulting \code{\link{OPMD}}
#'   object, respectively.
#'
#' @family discretization-functions
#' @keywords character category
#'
#' @examples
#'
#' ## OPMA method
#' data(vaas_1)
#'
#' # arbitrary threshold, no ambiguity
#' summary(x <- do_disc(vaas_1, cutoff = 100))
#' stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
#' (y <- disc_settings(x))  # stored discretization settings
#' stopifnot(identical(y$method, "direct"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#'
#' # arbitrary thresholds, allowing intermediate ('weak') reactions
#' summary(x <- do_disc(vaas_1, cutoff = c(75, 125)))
#' # the intermediate reactions are coded as NA
#' stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(is.na(discretized(x))))
#' (y <- disc_settings(x)) # stored discretization settings
#' stopifnot(identical(y$method, "direct"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#'
#' # using k-means, two categories, no intermediate ('weak') reactions
#' summary(x <- do_disc(vaas_1, cutoff = FALSE))
#' stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
#' (y <- disc_settings(x)) # stored discretization settings
#' stopifnot(identical(y$method, "kmeans"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#'
#' # using k-means, now allowing intermediate ('weak') reactions
#' summary(x <- do_disc(vaas_1, cutoff = TRUE))
#' stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(discretized(x)))
#' (y <- disc_settings(x)) # stored discretization settings
#' stopifnot(identical(y$method, "kmeans"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#'
#'
#' ## OPMS method
#' data(vaas_4)
#'
#' # arbitrary threshold, no ambiguity, no groups
#' x <- do_disc(vaas_4, cutoff = 100)
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "direct"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#'
#' # arbitrary threshold, no ambiguity, no groups, with unification
#' x <- do_disc(vaas_4, cutoff = 100, unify = TRUE)
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4))
#' stopifnot(any(is.na(discretized(x)))) # NAs caused by unification
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "direct"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#' # all plates made uniform (makes not much sense)
#'
#' # arbitrary threshold, no ambiguity, with groups, 1 plate per group
#' x <- do_disc(vaas_4, cutoff = 100, groups = TRUE)
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "direct"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#' # here, the plate numbers yield the group names
#'
#' # arbitrary threshold, no ambiguity, with specified groups
#' x <- do_disc(vaas_4, cutoff = 100, groups = "Species")
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "direct"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#' # now, groups are from the metadata (but played no role)
#'
#' # using k-means, no ambiguity, with specified groups
#' x <- do_disc(vaas_4, cutoff = FALSE, groups = "Species")
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "kmeans"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#' # grouping by species, discretized separately
#'
#' # same, with unification
#' x <- do_disc(vaas_4, cutoff = FALSE, groups = "Species", unify = TRUE)
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4))
#' stopifnot(any(is.na(discretized(x)))) # NAs caused by unification
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "kmeans"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#' # grouping by species, discretized separately, then made uniform
#'
#' # using best_cutoff(), groups defined by species affiliation (makes not
#' # much sense and by default yields warnings with these data)
#' x <- do_disc(vaas_4, cutoff = NULL, groups = "Species")
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "best-cutoff"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#' # groups as above, 2 strains per species, but some additional entries
#'
#' # using best_cutoff(), single group for all plates (makes even less sense
#' # and by default also yields warnings with these data)
#' x <- do_disc(vaas_4, cutoff = NULL, groups = FALSE)
#' stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
#' (y <- disc_settings(x)[[1]]) # stored discretization settings
#' stopifnot(identical(y$method, "best-cutoff"))
#' stopifnot(is.list(y), is.list(y$options)) # named lists
#' # no subgroups, all 4 datasets in one group, and some additional entries
#'
setGeneric("do_disc", function(object, ...) standardGeneric("do_disc"))

setMethod("do_disc", OPMA, function(object, cutoff, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), unify = FALSE) {
  if (!length(cutoff))
    stop(sprintf(
      "'cutoff' must be a non-empty vector if applied to %s objects",
      class(object)))
  x <- aggregated(object, subset = map_grofit_names(subset, ci = FALSE)[[1L]],
    ci = FALSE)[1L, ]
  x <- discrete(x, range = cutoff, gap = TRUE, output = "logical")
  settings <- list(if (is.numeric(cutoff))
    "direct"
  else
    "kmeans",
    list(cutoffs = attr(x, "cutoffs"), datasets = 1L, parameter = subset,
      unified = -1))
  settings <- c(settings, as.list(opm_string(version = TRUE)))
  names(settings) <- c(METHOD, OPTIONS, SOFTWARE, VERSION)
  if (L(plain))
    return(structure(c(x), settings = settings))
  new(OPMD, measurements = measurements(object),
    metadata = metadata(object), csv_data = csv_data(object),
    aggregated = aggregated(object), aggr_settings = aggr_settings(object),
    discretized = c(x), disc_settings = settings)
}, sealed = SEALED)

setMethod("do_disc", "OPMS", function(object, cutoff = TRUE, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), unify = !length(cutoff),
    ...) {

  do_bc <- function(x, grp, combined) {
    bc <- best_cutoff(x, grp <- as.factor(grp), combined)
    if (combined) # convert returned vector into appropriate matrix
      bc <- matrix(bc, nrow = length(levels(grp)), ncol = length(bc),
        byrow = TRUE, dimnames = list(levels(grp), names(bc)))
    bc
  }

  add_disc <- function(x, discretized, disc.settings) {
    new(OPMD, measurements = measurements(x),
      metadata = metadata(x), csv_data = csv_data(x),
      aggregated = aggregated(x), aggr_settings = aggr_settings(x),
      discretized = discretized, disc_settings = disc.settings)
  }

  prepare_unify <- function(unify) {
    if (is.logical(L(unify))) {
      if (is.na(unify))
        1L
      else if (unify)
        get("min.mode", OPM_OPTIONS)
      else
        -1L
    } else if (is.numeric(unify)) {
      if (is.na(unify))
        stop("a numeric NA 'unify' value makes no sense")
      if (unify > 0) {
        if (unify > 1)
          stop("a numeric 'unify' value > 1 makes no sense")
        unify
      } else
        -1L
    } else
      stop("'unify' must be a logical or numeric scalar")
  }

  add_as_options <- function(x, y) {
    x[[OPTIONS]] <- y
    x
  }

  if (!all(has_aggr(object)))
    stop("all plates must contain aggregated data to run this function")

  unify <- prepare_unify(unify)

  if (is.logical(groups)) {
    combined <- !L(groups)
    groups <- NULL
  } else
    combined <- !length(groups)

  # extra step necessary here because extract() allows 'disc'
  subset <- unname(match.arg(subset, unlist(map_grofit_names(plain = TRUE))))

  x <- extract(object = object, as.labels = groups, subset = subset,
    ci = FALSE, full = FALSE, dataframe = FALSE, dups = "ignore", ...)

  if (use.best <- !length(cutoff)) {
    if (combined)
      grp <- rep.int(".", nrow(x))
    else {
      if (!length(groups))
        stop("if 'cutoff' is empty, 'groups' must not be empty")
      grp <- rownames(x)
    }
  } else if (combined)
    grp <- NULL
  else if (length(groups))
    grp <- rownames(x)
  else
    grp <- seq_len(nrow(x))

  disc.settings <- list(if (use.best)
    "best-cutoff"
  else if (is.numeric(cutoff))
    "direct"
  else
    "kmeans", list())
  disc.settings <- c(disc.settings, as.list(opm_string(version = TRUE)))
  names(disc.settings) <- c(METHOD, OPTIONS, SOFTWARE, VERSION)

  if (length(grp)) { # unless empty, 'grp' now holds the group names

    disc.settings <- rep.int(list(disc.settings), length(object))
    if (use.best) { # using best_cutoff() instead of discrete()
      bc <- do_bc(x, grp, combined)
      for (idx in split(seq_along(grp), grp)) {
        group <- grp[idx[1L]]
        settings <- list(cutoffs = bc[group, "maximum"], datasets = length(idx),
          score = bc[group, "objective"], group = group, parameter = subset,
          unified = unify)
        if (combined)
          settings$group <- NULL
        x[idx, ] <- x[idx, , drop = FALSE] > settings$cutoffs
        for (i in idx)
          disc.settings[[i]] <- add_as_options(disc.settings[[i]], settings)
      }
    } else { # discrete() partitioning separately per group
      for (idx in split(seq_along(grp), grp)) {
        x[idx, ] <- y <- discrete(x[idx, , drop = FALSE], range = cutoff,
          gap = TRUE, output = "integer")
        settings <- list(cutoffs = attr(y, "cutoffs"), datasets = length(idx),
          group = as.character(grp[idx[1L]]), parameter = subset,
          unified = unify)
        for (i in idx)
          disc.settings[[i]] <- add_as_options(disc.settings[[i]], settings)
      }
    }
    mode(x) <- "logical"
    if (unify > 0)
      for (idx in split(seq_along(grp), grp)) {
        y <- reduce_to_mode.matrix(x[idx, , drop = FALSE], unify, TRUE)
        for (i in idx)
          x[i, ] <- y
      }

  } else if (combined) { # discrete() partitioning with entire dataset at once

    x <- discrete(x, range = cutoff, gap = TRUE, output = "logical")
    disc.settings <- add_as_options(disc.settings,
      list(cutoffs = attr(x, "cutoffs"), datasets = length(object),
      parameter = subset, unified = unify))
    disc.settings <- rep.int(list(disc.settings), length(object))
    if (unify > 0) {
      y <- reduce_to_mode.matrix(x, unify, TRUE)
      for (i in seq_len(nrow(x)))
        x[i, ] <- y
    }

  } else # this should be an impossible combination of settings
    stop(BUG_MSG)

  if (L(plain))
    return(structure(c(x), settings = disc.settings))
  for (i in seq_along(object@plates))
    object@plates[[i]] <- add_disc(object@plates[[i]], discretized = x[i, ],
      disc.settings = disc.settings[[i]])
  object
}, sealed = SEALED)


################################################################################


