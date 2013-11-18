to_kmeans <- function(x, ...) UseMethod("to_kmeans")

to_kmeans.kmeans <- function(x, ...) {
  x
}

to_kmeans.kmeanss <- function(x, y, ...) {
  x[[y]]
}

to_kmeans.Ckmeans.1d.dp <- function(x, y, ...) {
  if (!is.numeric(y) || length(y) != length(x$cluster))
    stop("'y' must correspond to the input data from which 'x' originates")
  x <- unclass(x)
  x$tot.withinss <- sum(x$withinss)
  x$totss <- sum(scale(y, scale = FALSE) ^ 2L)
  x$betweenss <- x$totss - x$tot.withinss
  x$centers <- as.matrix(x$centers)
  x <- x[c("cluster", "centers", "totss", "withinss", "tot.withinss",
    "betweenss", "size")]
  x$iter <- 1L # new entry as of R 3.0.1 patched; trivially 1 here
  x$ifault <- 0L
  class(x) <- "kmeans"
  x
}

calinski <- function(x, ...) UseMethod("calinski")

calinski.kmeans <- function(x, ...) {
  r.2 <- (x$totss - x$tot.withinss) / x$totss
  # We do not use "$centers" here because there are as many centers per
  # cluster as matrix columns if a matrix was input
  k <- length(unique(x$cluster))
  n <- length(x$cluster)
  (r.2 / (k - 1L)) / ((1L - r.2) / (n - k))
}

calinski.Ckmeans.1d.dp <- function(x, y, ...) {
  calinski(to_kmeans(x, y), ...)
}

calinski.kmeanss <- function(x, ...) {
  vapply(X = x, FUN = calinski, FUN.VALUE = 1, ...)
}

plot.kmeanss <- function(x, xlab = "Number of clusters",
    ylab = "Calinski-Harabasz statistics", ...) {
  x <- as.numeric(names(y <- calinski(x)))
  plot(x, y, xlab = xlab, ylab = ylab, ...)
  invisible(y)
}

borders <- function(x, ...) UseMethod("borders")

borders.kmeans <- function(x, y, ...) {
  if (sum(siz <- x$size) != length(y))
    stop("'y' must be a vector with the same number of items than 'x'")
  if (length(siz) == 1L)
    return(numeric())
  ranges <- vapply(seq_along(siz), function(i) range(y[x$cluster == i]),
    numeric(2L))
  colMeans(matrix(sort.int(ranges)[c(-1L, -length(ranges))], nrow = 2L))
}

borders.Ckmeans.1d.dp <- function(x, y, ...) {
  borders(to_kmeans(x), y, ...)
}

borders.kmeanss <- function(x, ...) {
  sapply(x, FUN = borders, y = attr(x, "input"), ..., simplify = FALSE)
}

hist.kmeans <- function(x, y, col = "black", lwd = 1L, lty = 1L, main = NULL,
    xlab = "Clustered values", ...) {
  b <- borders(x, y)
  result <- hist(y, main = main, xlab = xlab, ...)
  mapply(abline, v = b, col = col, lwd = lwd, lty = lty, SIMPLIFY = FALSE,
    USE.NAMES = FALSE)
  invisible(result)
}

hist.Ckmeans.1d.dp <- function(x, y, ...) {
  hist(to_kmeans(x), y, ...)
}

hist.kmeanss <- function(x, k = NULL, col = "black", lwd = 1L, lty = 1L,
    main = NULL, xlab = "Clustered values", ...) {
  smallest_k <- function(x) {
    y <- (y <- as.integer(names(x)))[y > 1L]
    case(length(y), integer(), min(y))
  }
  result <- hist(y <- attr(x, "input"), main = main, xlab = xlab, ...)
  if (!length(k) && !length(k <- smallest_k(x)))
    return(invisible(result))
  b <- lapply(as.character(k), function(key) borders(x[[key]], y))
  mapply(abline, v = b, col = col, lwd = lwd, lty = lty, SIMPLIFY = FALSE,
    USE.NAMES = FALSE)
  invisible(result)
}

prepare_k <- function(k) {
  k <- sort.int(unique.default(must(as.integer(k))))
  if (length(k) < 1L || any(is.na(k)) || any(k < 1L))
    stop("'k' must contain positive numbers throughout")
  names(k) <- k
  k
}

setGeneric("run_kmeans",
  function(object, k, ...) standardGeneric("run_kmeans"))

setMethod("run_kmeans", c("numeric", "numeric"), function(object, k,
    cores = 1L) {
  result <- mclapply(prepare_k(k), Ckmeans.1d.dp, x = object,
    mc.cores = cores)
  structure(lapply(result, to_kmeans, y = object), class = "kmeanss",
    input = object)
}, sealed = SEALED)

setMethod("run_kmeans", c("matrix", "numeric"), function(object, k,
    cores = 1L, nstart = 10L, ...) {
  result <- if (ncol(object) < 2L)
    run_kmeans(as.vector(object), k, cores)
  else
    structure(mclapply(prepare_k(k), function(centers) {
      kmeans(x = object, centers = centers, nstart = nstart, ...)
    }, mc.cores = cores), class = "kmeanss")
  attr(result, "input") <- object
  result
}, sealed = SEALED)

