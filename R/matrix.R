


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
# Fast curve-parameter estimation
#


pe_and_ci <- function(x, ...) UseMethod("pe_and_ci")
#' CI and point-estimate calculation
#'
#' Get point estimates and CIs (if possible) from the result of \code{boot}.
#'
#' @param x Object of class \sQuote{boot}.
#' @param ci Numeric scalar. See \code{\link{fast_estimate}}.
#' @param as.pe Character scalar. See \code{\link{fast_estimate}}.
#' @param type Character scalar. See \code{\link{boot.ci}} from the
#'   \pkg{boot} package.
#' @param ... Optional arguments passed to \code{\link{boot.ci}} from the
#'   \pkg{boot} package.
#' @return See \code{\link{fast_estimate}}.
#' 
#' @method pe_and_ci boot
#' @keywords internal
#'
pe_and_ci.boot <- function(x, ci = 0.95, as.pe = c("median", "mean", "pe"), 
    type = c("basic", "perc", "norm"), ...) {
  last <- function(x, i) {
    len <- length(x)
    x[c(len - i + 1L, len)]
  }
  as.pe <- match.arg(as.pe)
  type <- match.arg(type)
  if (nrow(x$t) == 0L) {
    if (as.pe != "pe") {
      warning("zero bootstrap replicates -- using real point estimate")
      as.pe <- "pe"
    }
    cis <- matrix(nrow = 2L, ncol = length(x$t0), data = NA_real_)
  } else {  
    cis <- lapply(seq_along(x$t0), FUN = boot::boot.ci, boot.out = x,
      conf = ci, type = type, ...)
    ok <- !unlist(lapply(cis, is.null))
    cis[!ok] <- list(c(NA_real_, NA_real_))
    cis[ok] <- lapply(cis[ok], `[[`, type, exact = FALSE)
    cis[ok] <- lapply(cis[ok], FUN = last, i = 2L)
    cis <- do.call(cbind, cis)
  }
  rownames(cis) <- c("ci.low", "ci.high")
  point.est <- switch(as.pe,
    median = apply(x$t, 2L, median),
    mean = colMeans(x$t),
    pe = x$t0,
    stop(BUG_MSG)
  )
  rbind(point.est, cis)
}


################################################################################


setGeneric("fast_estimate", function(x, ...) standardGeneric("fast_estimate"))
#' Fast curve-parameter estimation
#'
#' Quickly estimate the curve parameters AUC (area under the curve) or A
#' (maximum height). This is not normally directly called by an \pkg{opm} user
#' but via \code{\link{do_aggr}}.
#'
#' @param x Matrix as output by \code{\link{measurements}}, i.e. with the
#'   time points in the first columns and the measurements in the remaining 
#'   columns (there must be at least two). For deviations from this scheme see
#'   \code{time.pos} and \code{transposed}.
#' @param what Character scalar. Which parameter to estimate. Currently only
#'   two are supported.
#' @param boot Integer scalar. Number of bootstrap replicates. Note that under
#'   the default settings for \code{as.pe}, bootstrapping is also necessary to
#'   obtain the point estimate.
#' @param ci Confidence interval to use in the output. Ignored if \code{boot} 
#'   is not positive.
#' @param as.pe Character scalar determining what to output as the point 
#'   estimate. Either \sQuote{median}, \sQuote{mean} or \sQuote{pe}; the first
#'   two calculate the point estimate from the bootstrapping replicates, the
#'   third one use the real point estimate. If \code{boot} is 0, \code{as.pe}
#'   is reset to \sQuote{pe}, if necessary, and a warning is issued.
#' @param ci.type Character scalar determining the way the confidence intervals
#'   are calculated. Either \sQuote{norm}, \sQuote{basic} or \sQuote{perc}; see
#'   \code{boot.ci} from the \pkg{boot} package for details. 
#' @param time.pos Character or integer scalar indicating the position of the
#'   column (or row, see next argument) with the time points.
#' @param transposed Character or integer scalar indicating whether the matrix 
#'   is transposed compared to the default.
#' @param raw Logical scalar. Return the raw bootstrapping result without CI
#'   estimation and construction of the usually resulting matrix?
#' @param ... Optional arguments passed to \code{boot} from the eponymous
#'   package.
#'
#' @export
#' @return Numeric matrix with three rows (point estimate, lower and upper CI)
#'   and as many columns as data columns (or rows) in \code{x}. If \code{raw}
#'   is \code{TRUE}, an object of the class \sQuote{boot}.
#' @family aggregation-functions
#' @seealso boot::boot grofit::grofit
#' @keywords smooth
#'
#' @examples 
#' data("vaas_1")
#' x <- fast_estimate(measurements(vaas_1))
#' stopifnot(identical(dim(x), c(3L, 96L)))
#'
setMethod("fast_estimate", "matrix", function(x, what = c("AUC", "A"), 
    boot = 100L, ci = 0.95, as.pe = "median", ci.type = "norm",
    time.pos = 1L, transposed = FALSE, raw = FALSE, ...) {
  assert_length(time.pos, boot, ci, transposed, raw)
  if (transposed)
    x <- t(x)
  y <- x[, time.pos]
  x <- x[, -time.pos, drop = FALSE]
  x.colnames <- colnames(x)
  switch(what <- match.arg(what),
    A = boot_fun <- function(x, w) apply(x[w, ], 2L, max),
    AUC = {  
      n.obs <- nrow(x)
      y <- y[-1L] - y[-n.obs]
      x <- 0.5 * (x[-1L, , drop = FALSE] + x[-n.obs, , drop = FALSE])
      boot_fun <- function(x, w) colSums(x[w, , drop = FALSE] * y[w])
    },
    stop(BUG_MSG)
  )      
  result <- boot::boot(data = x, statistic = boot_fun, R = boot, ...)
  if (raw)
    return(result)
  result <- pe_and_ci(result, ci = ci, as.pe = as.pe, type = ci.type)
  colnames(result) <- x.colnames
  rownames(result) <- paste(what, rownames(result), sep = ".")
  result
}, sealed = SEALED)


################################################################################
################################################################################
#
# Heatmap functions for matrices and dataframes
#


setGeneric("heat_map", function(object, ...) standardGeneric("heat_map"))
#' Heat map
#'
#' A wrapper for \code{heatmap} from the \pkg{stats} package and 
#' \code{heatmap.2} from the \pkg{gplots} package with some adaptations
#' likely to be useful for OmniLog(R) data. The dataframe method extracts a 
#' numeric matrix from a given dataframe and passes the result to the matrix
#' method.
#'
#' @param object Matrix or dataframe. The matrix method is mainly designed for
#'   curve-parameter matrices as created by \code{\link{extract}} but can be
#'   used with any numeric matrix. If a dataframe, it must contain at least
#'   one column with numeric data.
#'
#' @param as.labels Character, numeric or logical vector indicating the 
#'   positions of the columns to be joined and used as row labels. If 
#'   \code{NULL} or empty, the row names of \code{object} are used.
#'
#' @param as.groups Character, numeric or logical vector indicating the 
#'   positions of the columns to be joined and used as group indicators. If 
#'   \code{NULL} or empty, groups are ignored.
#'
#' @param sep Character scalar determining how to join row and group names.
#'
#' @param hclustfun Determines the clustering method used. If a function, used
#'   directly. If a character scalar, used as the \sQuote{method} argument of
#'   \code{hclust}. If a list, passed as argument list to \code{hclust}.
#' @param distfun Determines the distance method used. If a function, used
#'   directly. If a character scalar, used as the \sQuote{method} argument of
#'   \code{dist}. If a list, passed as argument list to \code{dist}.
#'
#' @param scale Character scalar. See \code{heatmap} for details. The default
#'   was changed to no rescaling because the curve parameters estimated from
#'   OmniLog(R) data have the same scale. If the relative changes per substrate
#'   are of interest, \sQuote{column} should be used.
#'
#' @param r.groups Determines the plotting of a color bar indicating row 
#'   groups. If \code{NULL}, ignored. If a function, applied to the row names 
#'   of \code{object}; should then yield one group name for each row name. If a
#'   character scalar, the name of an attribute of \code{object} that contains
#'   the row group affiliations (ignored if this is not found). Otherwise, 
#'   coerced to \sQuote{character} mode. Finally the groups are converted to a
#'   factor and used for selecting from \code{r.col}.
#' @param r.col Character vector of color names used by \code{r.groups}. 
#'   Ignored if that is \code{NULL}.
#' @param c.groups Determines the plotting of a color bar indicating column 
#'   groups. If \code{NULL}, ignored. If a function, applied to the column 
#'   names of \code{object}; should then yield one group name for each column
#'   name. If a character scalar, the name of an attribute of \code{object} 
#'   that contains the column group affiliations (ignored if this is not
#'   found). Otherwise, coerced to \sQuote{character} mode. Finally the groups 
#'   are converted to a factor and used for selecting from \code{c.col}.
#' @param c.col Character vector of color names used by \code{c.groups}. 
#'   Ignored if that is \code{NULL}.
#'
#' @param magnif Numeric vector. Factor(s) used per default by \code{cexRow}  
#'   and \code{cexCol}.
#' @param cexRow Magnification of the row labels.
#' @param cexCol Magnification of the column labels.
#'
#' @param borders Numeric vector. Factor(s) used per default by \code{margin}  
#'   and \code{cexCol}.
#' @param margins Two-element numeric vector determining the relative size of
#'   the margin (i) at the bottom and (ii) at the left.
#'
#' @param col Character vector containing the proper heatmap colors.
#' @param ... Optional arguments passed to \code{heatmap} or \code{heatmap.2}.
#'   Note that some defaults of \code{heatmap.2} are overwritten even though
#'   this is not transparent from the argument list of \code{heat_map}. If set
#'   explicitly, the default \code{heatmap.2} behaviour is restored.
#'   \code{...} also represents all arguments passed from the data frame to the
#'   matrix method.
#'
#' @param use.fun Character scalar. If \sQuote{gplots}, it is attempted to load 
#'   the \pkg{gplots} package and use its \code{heatmap.2} function (the 
#'   default). If this fails, a warning is issued, and \code{heatmap} from the
#'   \pkg{stats} package (the default) is called instead.
#'
#' @export
#' @return A list as output by \code{heatmap} or \code{heatmap.2} with the 
#'   additional entries \sQuote{rowColMap} or \sQuote{colColMap} giving the
#'   mapping(s) of group names to colors as named character vector(s), if this 
#'   feature was used.
#'   
#' @family plotting-functions
#' @seealso stats::heatmap gplots::heatmap.2
#' @keywords hplot
#'
#' @examples 
#'
#' data("vaas_4")
#'
#' # Matrix method
#' x <- extract(vaas_4, as.labels = list("Strain"), 
#'   as.groups = list("Species"))
#' hm <- heat_map(x)
#' stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))
#' 
#' # Dataframe method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
#' hm <- heat_map(x, as.labels = "Strain", as.groups = "Species")
#' stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))
#'
setMethod("heat_map", "matrix", function(object, 
    hclustfun = "ward", distfun = "euclidean", scale = "none",
    r.groups = "row.groups", r.col = select_colors(),
    c.groups = "col.groups", c.col = select_colors(), 
    magnif = 4, cexRow = magnif[1L] / sqrt(nrow(object)), 
    cexCol = magnif[length(magnif)] / sqrt(ncol(object)),
    borders = c(0.55, 0.75),
    margins = if (use.fun[1L] == "gplots") 
      c(borders[1L] * cexCol * max(nchar(colnames(object))), 
      borders[length(borders)] * cexRow * max(nchar(rownames(object))))
    else
      c(5, 5),
    col = topo.colors(120L),
    ...,
    use.fun = c("gplots", "stats")) {
  
  get_fun <- function(infun, usefun) {
    if (is.character(infun))
      function(x) usefun(x, method = infun)
    else if (is.list(infun))
      function(x) do.call(usefun, c(list(x), infun))
    else
      infun
  }
  
  get_side_colors <- function(groups, colors, for.rows) {
    if (is.null(groups))
      return(NULL)
    if (is.function(groups)) {
      groups <- if (for.rows)
        groups(rownames(object))
      else
        groups(colnames(object))
    } else if (is.character(groups)) {
      if (length(groups) == 1L) {
        groups <- attr(object, groups)
        if (is.null(groups))
          return(NULL)
      }
    } else
      groups <- as.character(groups)
    groups <- as.factor(groups)
    if (length(colors) < length(levels(groups)))
      stop("more groups than colours given")
    structure(colors[groups], names = as.character(groups))
  }
  
  clustfun <- get_fun(hclustfun, hclust)
  dfun <- get_fun(distfun, dist)
  arg.list <- list(object, scale = scale, cexRow = cexRow, cexCol = cexCol,
    hclustfun = clustfun, distfun = dfun, margins = margins, col = col, ...)

  row.side.colors <- get_side_colors(r.groups, r.col, for.rows = TRUE)
  if (!is.null(row.side.colors))
    arg.list$RowSideColors <- row.side.colors
  col.side.colors <- get_side_colors(c.groups, c.col, for.rows = FALSE)
  if (!is.null(col.side.colors))
    arg.list$ColSideColors <- col.side.colors
  
  switch(match.arg(use.fun),
    gplots = {
      if (require(gplots, quietly = TRUE, warn.conflicts = FALSE)) {
        arg.list <- insert(arg.list, trace = "none", .force = FALSE)
        heatmap_fun <- gplots::heatmap.2
      } else {
        warning("package 'gplots' requested, but not available")
        heatmap_fun <- stats::heatmap
      }
    },
    stats = heatmap_fun <- stats::heatmap,
    stop(BUG_MSG)
  )
  
  result <- do.call(heatmap_fun, arg.list)
  result$colColMap <- col.side.colors
  result$rowColMap <- row.side.colors
  invisible(result)

}, sealed = SEALED)

#' @export
#'
setMethod("heat_map", "data.frame", function(object, as.labels, 
    as.groups = NULL, sep = " ", ...) {
  find_numeric <- function(x) {
    is.num <- which(sapply(x, is.numeric))
    if (length(is.num) == 0L)
      stop("no numeric data found")
    as.matrix(x[, is.num, drop = FALSE])
  }
  get_and_join <- function(what) {
    apply(object[, what, drop = FALSE], 1L, FUN = paste, collapse = sep)
  }
  mat <- find_numeric(object)
  if (length(as.labels))
    rownames(mat) <- get_and_join(as.labels)
  if (length(as.groups))
    attr(mat, "row.groups") <- as.factor(get_and_join(as.groups))
  invisible(heat_map(mat, ...))
}, sealed = SEALED)


################################################################################


