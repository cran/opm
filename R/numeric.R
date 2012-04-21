

#' MOA class
#'
#' This is a virtual class facilitating the implementation of functionality for
#' both matrices and arrays. Methods defined for objects from the class can be
#' applied to either kind of object, but this class is not directly dealt with
#' by an \pkg{opm} user.
#'
#' @details
#'   \sQuote{MOA} is an acronym for \sQuote{matrix or array}.
#'
#' @name MOA
#'
#' @docType class
#' @export
#' @family classes
#' @keywords methods
#'
NULL


setClassUnion(MOA, c("matrix", "array"))


################################################################################
################################################################################
#
# Helper functions acting on numeric data
#


setGeneric("ranging", function(object, ...) standardGeneric("ranging"))
#' Conduct ranging
#'
#' Range numbers, i.e. divide them by their maximum. In \sQuote{extended} mode,
#' the minimum is subtracted beforehand. It is possible to replace ranging by
#' standardization (z-scores).
#'
#' @param object Numeric vector or array.
#' @param extended Logical scalar. Subtract the minimum in both numerator and
#'   denominator of the ranging formula? If \code{zscores} is \code{TRUE}, the
#'   meaning is different.
#' @param zscores Logical scalar. Calculate z-scores instead of ranging? If
#'   \code{extended} is \code{FALSE}, this is done using the mean and the
#'   standard deviation; otherwise, the median and the MAD are used.
#' @param na.rm Logical scalar. Remove \code{NA} values when calculating the
#'   relevant aggregated values (minimum, maximum, mean, standard deviation,
#'   median and/or MAD)?
#' @param fac Numeric scalar. After conducting the proper ranging process,
#'   \code{object} is multiplied by \code{fac}.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @return Numeric vector or matrix.
#' @keywords internal
#'
setMethod("ranging", "numeric", function(object, extended = !zscores,
    zscores = FALSE, na.rm = TRUE, fac = 1) {
  assert_length(extended, zscores, na.rm)
  result <- if (zscores) {
    if (extended) {
      center <- median(x, na.rm = na.rm)
      (x - center) / mad(x, center = center, na.rm = na.rm)
    } else
      (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
  } else {
    if (extended) {
      min.x <- min(x, na.rm = na.rm)
      (x - min.x) / (max(x, na.rm = na.rm) - min.x)
    } else
      x / max(abs(x), na.rm = na.rm)
  }
  must(result * fac)
}, sealed = SEALED)


#' @export
#'
setMethod("ranging", MOA, function(object, ...) {
  map_values(object, mapping = ranging, ...)
}, sealed = SEALED)


################################################################################


setGeneric("guess_cex", function(object, ...) standardGeneric("guess_cex"))
#' Estimate cex
#'
#' Guess a suitable \code{cex} parameter for \code{\link{level_plot}}. 0.5 is
#' fine for the original number of wells (96).
#'
#' @param object Numeric vector.
#' @return Numeric vector.
#' @keywords internal
#'
setMethod("guess_cex", "numeric", function(object) {
  0.5 * sqrt(96 / object)
}, sealed = SEALED)


################################################################################


setGeneric("best_layout",
  function(object, ...) standardGeneric("best_layout"))
#' Best two-dimensional layout
#'
#' Determine number of rows/columns in plot for given number of fields.
#'
#' @param object Numeric scalar.
#' @param by Numeric scalar (width/height relation).
#' @return Numeric vector of length 2.
#' @keywords internal
#'
setMethod("best_layout", "numeric", function(object, by = 0.75) {
  assert_length(object, by)
  if (object < 0)
    stop("a negative number of fields makes no sense")
  if (object < 2)
    return(c(object, object))
  large <- ceiling(sqrt((1 / by) * object)) # => error unless 'by' is numeric
  small <- ceiling(object / large)
  c(large, small)
}, sealed = SEALED)


################################################################################


setGeneric("best_range",
  function(object, ...) standardGeneric("best_range"))
#' Best range
#'
#' Determine an optimal range for plotting.
#'
#' @param object Numeric vector.
#' @param target Numeric scalar. Target difference between min and max. If
#'   \code{NULL}, this is simply derived from the range of \code{object}.
#' @param align Character scalar. Where to put the real values relative to min
#'   and max of \code{target}.
#' @param offset Numeric scalar. A minimal distance to the margins.
#' @param prop.offset Numeric scalar. As an alternative to \code{offset}, it
#'   can be specified as a proportion of \code{target}.
#' @return Optimal range (numeric vector of length two).
#' @keywords internal
#'
setMethod("best_range", "numeric", function(object, target,
    align = c("center", "left", "right"),
    offset = 0, prop.offset = 0) {
  orig.range <- range(object)
  orig.diff <- orig.range[2L] - orig.range[1L]
  if (length(target) == 0L)
    target <- orig.diff
  else
    assert_length(target)
  assert_length(offset, prop.offset)
  if (offset == 0)
    offset <- target * prop.offset
  total <- target + 2 * offset
  if (total < orig.diff) {
    fmt <- "target (%s) + 2 * offset (%s) smaller than original range (%s)"
    stop(sprintf(fmt, target, offset, orig.diff))
  }
  switch(match.arg(align),
    center = {
      add <- total / 2
      mean(orig.range) + c(-add, add)
    },
    left = orig.range[1L] + c(-offset, target + offset),
    right = orig.range[2L] + c(-target - offset, offset),
    stop(BUG_MSG)
  )
}, sealed = SEALED)


################################################################################
################################################################################
#
# S3 K-means related methods
#


#' Convert to kmeans
#'
#' Convert an object to one of class \sQuote{kmeans}.
#'
#' @param x Object to be converted.
#' @param y Original numeric vector that was used to create a
#'   \sQuote{Ckmeans.1d.dp} object, or index of an element of a
#'   \sQuote{kmeanss} object.
#' @param ... Optional arguments passed to and from other methods, and/or
#'   between the methods.
#' @return Object of class \sQuote{kmeans}.
#' @keywords manip
#' @family kmeans-functions
#' @seealso Ckmeans.1d.dp::Ckmeans.1d.dp
#' @export
#' @examples
#' x <- c(1, 2, 4, 5, 7, 8)
#' summary(y <- kmeans(x, 3))
#' stopifnot(identical(y, to_kmeans(y)))
#' # see particularly run_kmeans() which uses this internally if clustering is
#' # done with Ckmeans.1d.dp::Ckmeans.1d.dp()
#'
to_kmeans <- function(x, ...) UseMethod("to_kmeans")

#' @rdname to_kmeans
#' @method to_kmeans kmeans
#' @export
#'
to_kmeans.kmeans <- function(x, ...) {
  x
}

#' @rdname to_kmeans
#' @method to_kmeans kmeanss
#' @export
#'
to_kmeans.kmeanss <- function(x, y, ...) {
  x[[y]]
}

#' @rdname to_kmeans
#' @method to_kmeans Ckmeans.1d.dp
#' @export
#'
to_kmeans.Ckmeans.1d.dp <- function(x, y, ...) {
  if (!is.numeric(y) || length(y) != length(x$cluster))
    stop("'y' must correspond to the input data from which 'x' originates")
  x <- unclass(x)
  x$tot.withinss <- sum(x$withinss)
  x$totss <- sum(scale(y, scale = FALSE)^2)
  x$betweenss <- x$totss - x$tot.withinss
  x$centers <- as.matrix(x$centers)
  x <- x[c("cluster", "centers", "totss", "withinss", "tot.withinss",
    "betweenss", "size")]
  class(x) <- "kmeans"
  x
}


################################################################################


#' Calinski-Harabasz statistics
#'
#' Calculate or plot the Calinski-Harabasz statistics from \code{kmeans}
#' results. The result of \code{plot} is a simple scatterplot which can be
#' modified with arguments passed to \code{plot} from the \pkg{graphics}
#' package.
#'
#' @param x Object of class \sQuote{kmeans}, \sQuote{Ckmeans.1d.dp} or
#'   \sQuote{kmeanss}. For \code{plot}, only the latter.
#' @param xlab Character scalar passed to \code{plot} from the \pkg{graphics}
#'   package.
#' @param ylab Character scalar passed to \code{plot}.
#' @inheritParams to_kmeans
#' @return \code{calinksi} returns a numeric vector with one element per
#'   \sQuote{kmeans} object. \code{plot} returns it invisibly. Its
#'   \sQuote{names} attribute indicates the original numbers of clusters
#'   requested.
#' @keywords hplot cluster
#' @family kmeans-functions
#' @export
#' @examples
#' data(vaas_4)
#' x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
#' x.km <- run_kmeans(x, k = 1:10)
#' # the usual arguments of plot() are available
#' show(y <- plot(x.km, col = "blue", pch = 19))
#' stopifnot(is.numeric(y), names(y) == 1:10)
#'
calinski <- function(x, ...) UseMethod("calinski")

#' @method calinski kmeans
#' @rdname calinski
#' @export
#'
calinski.kmeans <- function(x, ...) {
  r.2 <- (x$totss - x$tot.withinss) / x$totss
  # We do not use "$centers" here because there are as many centers per
  # cluster as matrix columns if a matrix was input
  k <- length(unique(x$cluster))
  n <- length(x$cluster)
  (r.2 / (k - 1L)) / ((1L - r.2) / (n - k))
}

#' @rdname calinski
#' @method calinski Ckmeans.1d.dp
#' @export
#'
calinski.Ckmeans.1d.dp <- function(x, y, ...) {
  calinski(to_kmeans(x, y), ...)
}

#' @rdname calinski
#' @method calinski kmeanss
#' @export
#'
calinski.kmeanss <- function(x, ...) {
  sapply(X = x, FUN = calinski, ...)
}

#' @rdname calinski
#' @method plot kmeanss
#' @export
#'
plot.kmeanss <- function(x, xlab = "Number of clusters",
    ylab = "Calinski-Harabasz statistics", ...) {
  x <- as.numeric(names(y <- calinski(x)))
  graphics::plot(x, y, xlab = xlab, ylab = ylab, ...)
  invisible(y)
}


################################################################################


#' Cluster borders
#'
#' Determine the borders between clusters of one-dimensional data. They are
#' calculated as the mean of the maximum of the cluster with the lower values
#' and the minimum of the neighboring cluster with the higher values. The
#' \code{hist} method plots a histogram of one-dimensional data subjected to
#' k-means partitioning in which these borders can be drawn.
#'
#' @param x Object of class \sQuote{kmeans}, \sQuote{Ckmeans.1d.dp} or
#'   \sQuote{kmeanss}.
#' @param y Vector of original data subjected to clustering. Automatically
#'   determined for the \sQuote{kmeanss} methods.
#' @param k Numeric vector or \code{NULL}. If non-empty, it indicates
#'   the number of groups (previously used as input for \code{kmeans}) for
#'   which vertical lines should be drawn in the plot that represent the
#'   cluster borders. If empty, the smallest non-trivial number of clusters is
#'   chosen.
#' @param col Graphical parameter passed to \code{abline}. If several values of
#'   \code{k} are given, \code{col} is recycled as necessary.
#' @param lwd Like \code{col}.
#' @param lty Like \code{col}.
#' @param main Passed to \code{hist.default}.
#' @param xlab Passed to \code{hist.default}.
#' @param ... Optional arguments passed to and from other methods. For the
#'   \code{hist} method, optional arguments passed to \code{hist.default}.
#' @export
#' @return Numeric vector or list of such vectors. For the \code{hist} method,
#'   like \code{hist.default}; see there for details.
#' @keywords cluster hplot
#' @family kmeans-functions
#' @note \code{y} must also be in the order it has been when subjected to
#'   clustering, but this is not checked. Using \sQuote{kmeanss} objects thus
#'   might preferable in most cases because they contain a copy of the input
#'   data.
#' @seealso graphics::hist graphics::abline
#' @examples
#'
#' data(vaas_4)
#' x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
#' x.km <- run_kmeans(x, k = 1:10)
#'
#' # borders() method
#' (x.b <- borders(x.km))
#' stopifnot(is.list(x.b), length(x.b) == 10, sapply(x, is.numeric))
#' stopifnot(sapply(x.b, length) == as.numeric(names(x.b)) - 1)
#'
#' # hist() methods
#' y <- hist(x.km[[2]], x, col = "blue", lwd = 2)
#' stopifnot(inherits(y, "histogram"))
#' y <- hist(x.km, 3:4, col = c("blue", "red"), lwd = 2)
#' stopifnot(inherits(y, "histogram"))
#'
borders <- function(x, ...) UseMethod("borders")

#' @rdname borders
#' @method borders kmeans
#' @export
#'
borders.kmeans <- function(x, y, ...) {
  if (sum(siz <- x$size) != length(y))
    stop("'y' must be a vector with the same number of items than 'x'")
  if (length(siz) == 1L)
    return(numeric())
  ranges <- sapply(seq_along(siz), function(i) range(y[x$cluster == i]))
  colMeans(matrix(sort(ranges)[c(-1L, -length(ranges))], nrow = 2L))
}

#' @rdname borders
#' @method borders Ckmeans.1d.dp
#' @export
#'
borders.Ckmeans.1d.dp <- function(x, y, ...) {
  borders(to_kmeans(x), y, ...)
}

#' @rdname borders
#' @method borders kmeanss
#' @export
#'
borders.kmeanss <- function(x, ...) {
  sapply(x, FUN = borders, y = attr(x, "input"), ..., simplify = FALSE)
}

#' @rdname borders
#' @method hist kmeans
#' @export
#'
hist.kmeans <- function(x, y, col = "black", lwd = 1L, lty = 1L, main = NULL,
    xlab = "Clustered values", ...) {
  b <- borders(x, y)
  result <- hist(y, main = main, xlab = xlab, ...)
  lapply(b, function(num) abline(v = num, col = col, lwd = lwd, lty = lty))
  invisible(result)
}

#' @rdname borders
#' @method hist Ckmeans.1d.dp
#' @export
#'
hist.Ckmeans.1d.dp <- function(x, y, ...) {
  hist(to_kmeans(x), y, ...)
}

#' @rdname borders
#' @method hist kmeanss
#' @export
#'
hist.kmeanss <- function(x, k = NULL, col = "black", lwd = 1L, lty = 1L,
    main = NULL, xlab = "Clustered values", ...) {
  smallest_k <- function(x) {
    y <- (y <- as.integer(names(x)))[y > 1L]
    if (length(y) > 0L)
      min(y)
    else
      integer()
  }
  result <- hist(y <- attr(x, "input"), main = main, xlab = xlab, ...)
  if (length(k) == 0L && length(k <- smallest_k(x)) == 0L)
    return(invisible(result))
  mapply(function(k.val, col.val, lwd.val, lty.val) {
    b <- borders(x[[as.character(k.val)]], y)
    lapply(b, function(num) {
      abline(v = num, col = col.val, lwd = lwd.val, lty = lty.val)
    })
  }, k, col, lwd, lty, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(result)
}


################################################################################
################################################################################
#
# S4 K-means related methods
#


setGeneric("run_kmeans",
  function(object, ...) standardGeneric("run_kmeans"))
#' Conduct k-means partitioning
#'
#' Run a k-means partitioning analysis. This function is currently only useable
#' for \strong{one-dimensional} data. It is used by \code{\link{discrete}} in
#' \sQuote{gap} mode to automatically determine the range of ambiguous data.
#'
#' @param object Numeric vector.
#' @param k Numeric vector. Number of clusters requested.
#' @param program Character scalar. The underlying clustering program to use.
#'   \sQuote{Ckmeans.1d.dp} is recommended because it exactly solves the
#'   k-means optimization problem for one-dimensional data, but it requires the
#'   installation of the eponymous package.
#' @param kmeans.args List of optional arguments passed to \sQuote{kmeans} from
#'   the \pkg{stats} package.
#' @return S3 object of class \sQuote{kmeanss}.
#' @family kmeans-functions
#' @seealso stats::kmeans Ckmeans.1d.dp::Ckmeans.1d.dp
#' @keywords cluster
#' @export
#' @examples
#' data(vaas_4)
#' x <- as.vector(extract(vaas_4, as.labels = NULL, subset = "A"))
#' summary(x.km <- run_kmeans(x, k = 1:10))
#' stopifnot(inherits(x.km, "kmeanss"), length(x.km) == 10)
#' stopifnot(sapply(x.km, class) == "kmeans", names(x.km) == 1:10)
#'
setMethod("run_kmeans", "numeric", function(object, k,
    program = c("Ckmeans.1d.dp", "kmeans"), kmeans.args = list()) {
  improve_k <- function(x) {
    vector_centers <- function(n) {
      n <- 2L * n
      n <- seq.int(1L, n - 1L, 2L) / n
      quantile(object, n, names = FALSE)
    }
    y <- as.list(x)
    y[large] <- lapply(y[large <- x > 1L], vector_centers)
    names(y) <- names(x)
    y
  }
  k <- unique(must(as.integer(k)))
  if (length(k) < 1L || any(k) < 1L)
    stop("'k' must contain positive numbers throughout")
  names(k) <- k
  if ((program <- match.arg(program)) == "Ckmeans.1d.dp")
    if (!require(Ckmeans.1d.dp, quietly = TRUE, warn.conflicts = FALSE)) {
      warning(program, " requested but not available")
      program <- "kmeans"
    }
  switch(program,
    kmeans = {
      kmeans.args <- insert(as.list(kmeans.args), x = object, .force = TRUE)
      result <- sapply(improve_k(k), function(k.val) {
        kmeans.args$centers <<- k.val
        do.call(stats::kmeans, kmeans.args)
      }, simplify = FALSE)
    },
    Ckmeans.1d.dp = {
      result <- sapply(k, Ckmeans.1d.dp::Ckmeans.1d.dp, x = object,
        simplify = FALSE)
      result <- lapply(result, to_kmeans, y = object)
    },
    stop(BUG_MSG)
  )
  class(result) <- "kmeanss"
  attr(result, "input") <- object
  result
}, sealed = SEALED)


################################################################################



