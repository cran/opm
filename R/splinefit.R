fit_spline <- function (y, x = "Hour", data, options = set_spline_options(),
    weights = NULL) {

  call <- match.call()
  type <- options$type
  method <- options$est.method
  knots <- options$knots
  class <- options$class
  correlation <- options$correlation

  if (is.null(weights))
    weights <- rep(1, nrow(data))

  ## compute number of knots adaptive to the number of unique observations
  ## (equal to behavior of smooth.spline(..., nknots = NULL) )
  if (is.null(knots))
    knots <- n_knots(length(unique(data[, x])))
  ## PERHAPS USE OTHER METHOD TO GET NUMBER OF KNOTS. E.G. BASED ON ROUGHNESS
  ## OF THE DATA

  ## set up model formulae
  if (type == "p.spline" || type == "tp.spline") {
      if (is.null(options$s.par)) {
          fm <- paste0(y, " ~ s(", x, ", bs = '", class, "', k = ", knots, ")")
      } else {
          fm <- paste0(y, " ~ s(", x, ", bs = '", class, "', k = ", knots, ",",
                      options$s.par, ")")
      }
      fm <- as.formula(fm)
  } else {
      ## REMOVED ", ..." after knots.
      ## Reintroduce possibility to "..." via options
      fm <- paste0("list(x = data$", x, ", y = data$", y,
                  ", penalty = ", options$gamma, ", nknots = ", knots,
                  ", w = weights)")
  }
  if (type == "p.spline" || type == "tp.spline") {
      ## REMOVED ... from gam.
      ## Reintroduce possibility to "..." via options
      if (!is.null(correlation)) {
        mod <- gamm(fm, data = data, gamma = options$gamma, method = method,
          weights = weights, correlation = correlation)
      } else {
        mod <- gam(fm, data = data, gamma = options$gamma, method = method,
          weights = weights)
      }
      mod$call <- call
  } else {
      mod <- do.call("smooth.spline", eval(parse(text = fm)))
      mod$call <- call
      mod$names <- c(x, y)
  }
  class(mod) <- c("opm_model", paste0(class, "_model"), class(mod))
  return(mod)
}

set_spline_options <- function(type = c("tp.spline",
    "p.spline", "smooth.spline"),
    knots = NULL, gamma = 1, est.method = c("REML", "ML", "GCV"), s.par = NULL,
    correlation = NULL, save.models = FALSE, filename = NULL, ...) {

  if (!missing(...))
    warning(sQuote("..."), " currently not passed to fitting functions")
  type <- match.arg(type)
  class <- ifelse(type == "tp.spline", "tp",
    ifelse(type == "p.spline", "psp", "smooth.spline"))

  method <- match.arg(est.method)
  if (est.method == "ML" && is.null(correlation))
    stop(sQuote(paste0("est.method = ", dQuote("ML"))), " can only be used if ",
      sQuote("correlation"), " is specified")
  if (est.method == "GCV" && !is.null(correlation))
    stop(sQuote(paste0("est.method = ", dQuote("GCV"))),
      " can only be used if no ", sQuote("correlation"), " is specified")
  if (type == "smoothing-splines" && !is.null(s.par))
    warning(sQuote("s.par"), " ignored if ",
      sQuote('type = "smoothing-splines"'))
  if (!is.null(filename) && !save.models) {
    save.models <- TRUE
    warning(sQuote("filename"), " specified, ", sQuote("save.models"),
      " set to TRUE")
  }
  list(type = type, knots = knots, gamma = gamma, est.method = method,
    s.par = s.par, correlation = correlation, save.models = save.models,
    filename = filename, class = class, ...)
}

predict.smooth.spline_model <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    ## get data from fitted model
    newdata <- get_data(object)
  }
  newX <- newdata[, object$names[1]]
  # stats:::predict.smooth.spline(object, x = newX, deriv = 0)$y
  NextMethod("predict", object, x = newX, deriv = 0)$y
}

plot.opm_model <- function(x, plot.data = TRUE, plot.spline = TRUE,
  add.parameters = FALSE, confint = TRUE, level = 0.95, col = rgb(0, 0, 0, 0.3),
  pch = 20, col.spline = "red", lty.spline = "solid", lwd.spline = 1,
  lty.confint = "dashed", lwd.confint = 1, ...) {

  x <- as.gam(x)
  if (!plot.data && ! plot.spline)
    stop("Nothing to be plotted")
  data <- get_data(x)
  plot_helper(data, col = col, pch = pch, plot.data = plot.data, ...)
  if (plot.spline)
    lines(x, add.parameters = add.parameters, confint = confint, level = level,
      col = col.spline, lty = lty.spline, lwd = lwd.spline,
      lty.confint = lty.confint, lwd.confint = lwd.confint, ...)
}

plot.opm_models <- function(x, which = NULL, plot.data = TRUE,
  plot.spline = TRUE, add.parameters = FALSE, confint = TRUE, level = 0.95,
  col = rgb(0, 0, 0, 0.3), pch = 20, col.spline = "red",
  lty.spline = "solid", lwd.spline = 1, lty.confint = "dashed", lwd.confint = 1,
  ...) {

  if (is.null(which))
    which <- 1:length(x)
  lapply(x[which], plot, plot.data = plot.data,
    plot.spline = plot.spline, add.parameters = add.parameters,
    confint = confint, level = level, col = col,
    pch = pch, col.spline = col.spline, lty.spline = lty.spline,
    lwd.spline = lwd.spline, lty.confint = lty.confint,
    lwd.confint = lwd.confint, ...)
  invisible(x)
}

lines.opm_model <- function(x, add.parameters = FALSE, confint = TRUE,
  level = 0.95, col = "red", lty = "solid", lwd = 1, lty.confint = "dashed",
  lwd.confint = 1, ...) {
  mod <- x
  if (inherits(x, "smooth.spline")) {
    pred <- predict(x)
    x <- get_data(x)[, 1]
    ix <- order(x)
    lines(x[ix], pred[ix], col = col, lty = lty, lwd = lwd, ...)
  } else {
    x <- as.gam(x)
    pred <- predict(x, se = TRUE)
    x <- get_data(x)[, 1]
    ix <- order(x)
    lines(x[ix], pred$fit[ix], col = col, lty = lty, lwd = lwd, ...)
    if (confint) {
      quantile <- 1 - (1 - level) * 0.5
      lines(x[ix], pred$fit[ix] + qnorm(quantile) * pred$se.fit[ix], col = col,
        lty = lty.confint, lwd = lwd.confint, ...)
      lines(x[ix], pred$fit[ix] - qnorm(quantile) * pred$se.fit[ix], col = col,
        lty = lty.confint, lwd = lwd.confint, ...)
    }
  }
  if (add.parameters)
    add_parameters(mod, col = col, ...)
}

lines.opm_models <- function(x, which = NULL, add.parameters = FALSE,
  confint = TRUE, level = 0.95, col = "red", lty = "solid", lwd = 1,
  lty.confint = "dashed", lwd.confint = 1, ...) {

  if (is.null(which))
    which <- 1:length(x)

  invisible(lapply(x[which], lines, add.parameters = add.parameters,
    confint = confint, level = level, col = col, lty = lty, lwd = lwd,
    lty.confint = lty.confint, lwd.confint = lwd.confint, ...))
}

AUC <- function(x, y, index = NULL) {
  if (!is.null(index)) {
      x <- x[index]
      y <- y[index]
  }
  n.obs <- length(x)
  sum(diff(x) * (0.5 * (y[-1L] + y[-n.obs])))
}

smooth.construct.psp.smooth.spec <- function (object, data, knots) {
    if (length(object$p.order) == 1)
        m <- rep(object$p.order, 2)
    else m <- object$p.order
    m[is.na(m)] <- 2
    object$p.order <- m
    if (object$bs.dim < 0)
        object$bs.dim <- max(10, m[1] + 1)
    nk <- object$bs.dim - m[1]
    if (nk <= 0)
        stop("basis dimension too small for b-spline order")
    if (length(object$term) != 1)
        stop("Basis only handles 1D smooths")
    x <- data[[object$term]]
    k <- knots[[object$term]]
    if (is.null(k)) {
        xl <- min(x)
        xu <- max(x)
    }
    else if (length(k) == 2) {
        xl <- min(k)
        xu <- max(k)
        if (xl > min(x) || xu < max(x))
            stop("knot range does not include data")
    }
    if (is.null(k) || length(k) == 2) {
        xr <- xu - xl
        xl <- xl - xr * 0.001
        xu <- xu + xr * 0.001
        dx <- (xu - xl) / (nk - 1)
        k <- seq(xl - dx * (m[1] + 1), xu + dx * (m[1] + 1),
            length = nk + 2 * m[1] + 2)
    }
    else {
        if (length(k) != nk + 2 * m[1] + 2)
            stop(paste("there should be ", nk + 2 * m[1] + 2,
                " supplied knots"))
    }
    object$X <- spline.des(k, x, m[1] + 2, x * 0, outer.ok = TRUE)$design
    if (!is.null(k)) {
        if (sum(colSums(object$X) == 0) > 0)
            warning("knot range is so wide that there is *no* information",
                    " about some basis coefficients")
    }
    S <- diag(object$bs.dim)
    if (m[2])
        for (i in 1:m[2]) S <- diff(S)
    object$S <- list(t(S) %*% S)
    object$S[[1]] <- (object$S[[1]] + t(object$S[[1]])) / 2
    object$rank <- object$bs.dim - m[2]
    object$null.space.dim <- m[2]
    object$knots <- k
    object$m <- m
    class(object) <- "psp.smooth"
    object
}

Predict.matrix.psp.smooth <- function (object, data) {
    X <- spline.des(object$knots, data[[object$term]], object$m[1] + 2,
                    outer.ok = TRUE)$design
    X
}

add_parameters <- function(model, add.deriv = FALSE, col = "red",
  deriv.col = "grey", lty = "dashed", ...) {

    x <- extract_curve_params.opm_model(model, all = TRUE)
    data <- get_data(model)[, 1]
    if (add.deriv) {
        lines(data[-1], x$deriv, col = deriv.col, lty = lty, ...)
        abline(h = 0, lty = lty, col = deriv.col)
    }
    abline(a = x$intercept, b = x$mu, col = col, lty = lty, ...)
    points(x$lambda, 0, col = col, ...)
    abline(h = x$A, col = col, lty = lty, ...)
}

plot_helper <- function(data, plot.data = TRUE, col = rgb(0, 0, 0, 0.3),
                        pch = 20, ...) {
    ## generate empty plot of appropriate size
    plot(data[, 1], data[, 2],
      ylab = names(data)[2],
      xlab = names(data)[1],
      type = "n", ...)
    if (plot.data) {
        points(data[, 1], data[, 2], col = col, pch = pch, ...)
    }
}

get_data <- function(x)
    UseMethod("get_data")

get_data.psp_model <- get_data.tp_model <- function(x) {
    x <- as.gam(x)
    data <- x$model
    data <- data[, c(2, 1)]
    return(data)
}

get_data.smooth.spline_model <- function(x) {
    x <- as.gam(x)
    data <- as.data.frame(x$data)
    names(data)[1:2] <- x$names
    return(data)
}

as.gam <- function(x, ...)
  UseMethod("as.gam")

as.gam.opm_model <- function(x, ...) {
  ret <- x
  if (inherits(x, "gamm")) {
    ## keep special classes
    classes <- class(x)[1:2]
    ret <- x$gam
    class(ret) <- c(classes, class(ret))
  }
  return(ret)
}

n_knots <- function (n) {
  if (n < 50L)
    n
  else trunc({
    a1 <- log2(50)
    a2 <- log2(100)
    a3 <- log2(140)
    a4 <- log2(200)
    if (n < 200L)
      2 ^ (a1 + (a2 - a1) * (n - 50) / 150)
    else if (n < 800L)
      2 ^ (a2 + (a3 - a2) * (n - 200) / 600)
    else if (n < 3200L)
      2 ^ (a3 + (a4 - a3) * (n - 800) / 2400)
    else
      200 + (n - 3200) ^ 0.2
  })
}

