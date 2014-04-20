setGeneric("to_grofit_time",
  function(object, ...) standardGeneric("to_grofit_time"))

setMethod("to_grofit_time", OPM, function(object) {
  tp <- hours(object, "all")
  as.data.frame(matrix(rep.int(tp, length(wells(object))), ncol = length(tp),
    byrow = TRUE))
}, sealed = SEALED)

setGeneric("to_grofit_data",
  function(object, ...) standardGeneric("to_grofit_data"))

setMethod("to_grofit_data", OPM, function(object) {
  w <- wells(object)
  names <- matrix(nrow = length(w), ncol = 3L,
    dimnames = list(well = w, value = c("well", "plate_id", "concentration")))
  names[, 1L] <- w
  names[, 2L] <- paste(csv_data(object, what = "setup_time"),
    csv_data(object, what = "position"), collapse = "-")
  names <- as.data.frame(names, stringsAsFactors = FALSE)
  names[, 3L] <- 1L # dummy concentration
  cbind(names, as.data.frame(t(measurements(object)[, -1L, drop = FALSE])))
}, sealed = SEALED)

extract_curve_params <- function(x, ...) UseMethod("extract_curve_params")

extract_curve_params.grofit <- function(x, ...) {
  settings <- c(x$control)
  x <- summary(x$gcFit)
  map <- map_param_names()
  structure(t(as.matrix(x[, names(map)])),
    dimnames = list(map, x[, "TestId"]), settings = settings)
}

extract_curve_params.opm_model <- function(x, all = FALSE, ...) {
  if (!inherits(x, "smooth.spline"))
    x <- as.gam(x)
  pred <- fitted(x)
  x <- get_data(x)[, 1]
  ## quick and dirty
  deriv <- diff(pred) / diff(x)
  slope <- max(deriv)
  ## index of max. slope
  idx <- which.max(deriv):(which.max(deriv) + 1)
  ## x-value of max. slope
  x_ms <- mean(x[idx])
  ## y-value of max. slope
  y_ms <- mean(pred[idx])
  ## intercept
  intercept <- y_ms - slope * x_ms
  ## lag
  lag <- - (intercept / slope)
  ## maximum
  maximum <- max(pred)
  ## AUC
  AUC <- AUC(x, pred)
  if (all)
      return(list(mu = slope, lambda = lag, A = maximum, AUC = AUC,
        derivative = deriv, intercept = intercept))
  return(data.frame(mu = slope, lambda = lag, A = maximum, AUC = AUC))
}

summary.splines_bootstrap <- function (object, ...) {

  cnames <- unlist(map_param_names(), use.names = FALSE)

  res <- data.frame(t(sapply(object, extract_curve_params.opm_model)))
  res$mu <- unlist(res$mu)
  res$lambda <- unlist(res$lambda)
  res$A <- unlist(res$A)
  res$AUC <- unlist(res$AUC)

  mu <- mean(res$mu, na.rm = TRUE)
  lambda <- mean(res$lambda, na.rm = TRUE)
  A <- mean(res$A, na.rm = TRUE)
  AUC <- mean(res$AUC, na.rm = TRUE)
  mu.sd <- sd(res$mu, na.rm = TRUE)
  lambda.sd <- sd(res$lambda, na.rm = TRUE)
  A.sd <- sd(res$A, na.rm = TRUE)
  AUC.sd <- sd(res$AUC, na.rm = TRUE)
  table <- c(mu, lambda, A, AUC,
    mu - qnorm(0.975) * mu.sd,
    lambda - qnorm(0.975) * lambda.sd,
    A - qnorm(0.975) * A.sd,
    AUC - qnorm(0.975) * AUC.sd,
    mu + qnorm(0.975) * mu.sd,
    lambda + qnorm(0.975) * lambda.sd,
    A + qnorm(0.975) * A.sd,
    AUC + qnorm(0.975) * AUC.sd)
  table <- data.frame(t(table))
  colnames(table) <- cnames
  return(table)
}

pe_and_ci <- function(x, ...) UseMethod("pe_and_ci")

pe_and_ci.boot <- function(x, ci = 0.95, as.pe = c("median", "mean", "pe"),
    type = c("basic", "perc", "norm"), fill.nas = FALSE, ...) {
  LL(ci, fill.nas)
  as.pe <- match.arg(as.pe)
  type <- match.arg(type)
  if (nrow(x$t)) {
    cis <- lapply(seq_along(x$t0), FUN = boot.ci, boot.out = x, conf = ci,
      type = type, ...)
    ok <- !vapply(cis, is.null, NA)
    cis[!ok] <- list(c(NA_real_, NA_real_))
    cis[ok] <- lapply(cis[ok], `[[`, type, exact = FALSE)
    cis[ok] <- lapply(lapply(cis[ok], c), tail, 2L)
    cis <- do.call(cbind, cis)
  } else {
    if (as.pe != "pe") {
      warning("zero bootstrap replicates -- using real point estimate")
      as.pe <- "pe"
    }
    cis <- matrix(nrow = 2L, ncol = length(x$t0), data = NA_real_)
  }
  rownames(cis) <- c("ci.low", "ci.high")
  point.est <- case(as.pe,
    median = apply(x$t, 2L, median),
    mean = colMeans(x$t),
    pe = x$t0
  )
  if (fill.nas) {
    boot.nas <- !is.na(x$t0) & is.na(cis[1L, ]) & is.na(cis[2L, ])
    cis[2L, boot.nas] <- cis[1L, boot.nas] <- x$t0[boot.nas]
  }
  rbind(point.est, cis)
}

setGeneric("do_aggr", function(object, ...) standardGeneric("do_aggr"))

setMethod("do_aggr", OPM, function(object, boot = 100L, verbose = FALSE,
    cores = 1L, options = list(), method = "grofit", plain = FALSE) {

  # Convert to OPMA
  integrate_in_opma <- function(object, result) {
    items <- c(METHOD, OPTIONS, SOFTWARE, VERSION)
    settings <- attributes(result)[items]
    for (item in items)
      attr(result, item) <- NULL
    new(OPMA, measurements = measurements(object),
      metadata = metadata(object), csv_data = csv_data(object),
      aggregated = result, aggr_settings = settings)
  }

  # Add our own changes of the default
  make_grofit_control <- function(verbose, boot, add) {
    result <- grofit.control()
    orig.class <- class(result)
    result <- insert(unclass(result), interactive = FALSE,
      suppress.messages = !verbose, fit.opt = "s", nboot.gc = boot,
      .force = TRUE)
    result <- insert(result, as.list(add), .force = TRUE)
    class(result) <- orig.class
    result
  }

  run_grofit <- function(time, data, control) {
    extract_curve_params(grofit(time = time, data = data,
      ec50 = FALSE, control = control))
  }

  run_mgcv <- function(x, y, data, options, boot) {
    mod <- fit_spline(y = y, x = x, data = data, options = options)
    if (boot > 0) {
      ## draw bootstrap sample
      folds <- rmultinom(boot, nrow(data), rep(1 / nrow(data), nrow(data)))
      res <- lapply(1:boot,
        function(i) {
          fit_spline(y = y, x = x, data = data, options = options,
            weights = folds[, i])
      })
      class(res) <- "splines_bootstrap"
      params <- as.vector(summary(res))
      return(list(params = params, model = mod, bootstrap = res))
    }
    list(params = extract_curve_params(mod), model = mod)
  }

  copy_A_param <- function(x) {
    map <- unlist(map_param_names(opm.fast = TRUE))
    result <- matrix(data = NA_real_, nrow = length(map), ncol = length(x),
      dimnames = list(unname(map), names(x)))
    result[map[["A.point.est"]], ] <- x
    result
  }

  if ((plate_type(object) %in% SPECIAL_PLATES ||
      custom_plate_is(plate_type(object))) && dim(object)[1] < 2L) {
    result <- copy_A_param(well(object))
    attr(result, METHOD) <- "shortcut"
    attr(result, OPTIONS) <- list(boot = boot)
  } else {
    case(method <- match.arg(method, KNOWN_METHODS$aggregation),
      grofit = {
        control <- make_grofit_control(verbose, boot, add = options)
        grofit.time <- to_grofit_time(object)
        grofit.data <- to_grofit_data(object)
        result <- mclapply(X = as.list(seq_len(nrow(grofit.data))),
          FUN = function(row) {
            run_grofit(grofit.time[row, , drop = FALSE],
              grofit.data[row, , drop = FALSE], control)
          }, mc.cores = cores)
        result <- do.call(cbind, result)
        attr(result, OPTIONS) <- unclass(control)
      },
      `opm-fast` = {
        options <- insert(as.list(options), boot = boot, .force = FALSE)
        mat <- measurements(object)
        result <- rbind(
          do.call(do_aggr, c(list(object = mat, what = "AUC"), options)),
          do.call(do_aggr, c(list(object = mat, what = "A"), options)),
          matrix(nrow = 6L, ncol = ncol(mat) - 1L, data = NA_real_)
        )
        rownames(result)[7L:9L] <- sub("^[^.]+", "lambda",
          rownames(result)[1L:3L], FALSE, TRUE)
        rownames(result)[10L:12L] <- sub("^[^.]+", "mu",
          rownames(result)[1L:3L], FALSE, TRUE)
        map <- map_param_names(opm.fast = TRUE)
        result <- result[names(map), , drop = FALSE]
        rownames(result) <- as.character(map)
        attr(result, OPTIONS) <- options
      },
      splines = {
        ## extract data
        data <- as.data.frame(measurements(object))
        ## get well names
        wells <- wells(object)
        indx <- as.list(seq_len(length(wells)))
        result <- mclapply(X = indx,
          FUN = function(i) {
            run_mgcv(x = HOUR, y = wells[i], data = data, options = options,
              boot = boot)
          }, mc.cores = cores)
        options <- insert(as.list(options), boot = boot)

        if (options$save.models) {
            opm_models <- lapply(result, function(x) x$model)
            if (boot > 0) {
              opm_bootstrap <- lapply(result, function(x) x$bootstrap)
            } else {
              opm_bootstrap <- NA
            }
            names(opm_models) <- wells
            class(opm_models) <- "opm_models"
            if (is.null(options$filename))
              options$filename <- paste0("opm_models_",
                format(Sys.time(), "%Y-%m-%d_%H:%M:%S"), ".RData")
            save("opm_models", "opm_bootstrap", file = options$filename)
            cat("Models saved as 'opm_models' on disk in file\n  ",
              getwd(), "/", options$filename, "\n\n", sep = "")
        }
        result <- sapply(result, function(x) x$params)
        rn <- rownames(result)
        result <- matrix(unlist(result),
          ncol = ncol(result), nrow = nrow(result))
        rownames(result) <- rn
        ## attach bootstrap CIs if necessary
        if (boot <= 0)
          result <- rbind(result,
            matrix(NA, nrow = 8L, ncol = ncol(result)))
        ## dirty hack:
        map <- map_param_names(opm.fast = TRUE)
        rownames(result) <- as.character(map)
        colnames(result) <- wells
        attr(result, OPTIONS) <- unclass(options)
      }
    )
    attr(result, METHOD) <- method
  }

  tmp <- opm_string(version = TRUE)
  attr(result, SOFTWARE) <- tmp[1L]
  attr(result, VERSION) <- tmp[2L]

  if (L(plain))
    return(result)
  integrate_in_opma(object, result)

}, sealed = SEALED)

setMethod("do_aggr", "OPMS", function(object, ...) {
  object@plates <- lapply(X = object@plates, FUN = do_aggr, ...)
  object
}, sealed = SEALED)

setMethod("do_aggr", "MOPMX", function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = do_aggr, ...)
  object
}, sealed = SEALED)

setMethod("do_aggr", "matrix", function(object, what = c("AUC", "A"),
    boot = 100L, ci = 0.95, as.pe = "median", ci.type = "norm",
    time.pos = 1L, transposed = FALSE, raw = FALSE, ...) {
  LL(time.pos, boot, ci, transposed, raw)
  if (transposed)
    object <- t(object)
  y <- object[, time.pos]
  object <- object[, -time.pos, drop = FALSE]
  object.colnames <- colnames(object)
  ## i arguments are required by boot
  case(what <- match.arg(what),
    A = boot_fun <- function(x, i) apply(x[i, ], 2L, max),
    AUC = {
      n.obs <- nrow(object)
      y <- y[-1L] - y[-n.obs]
      object <- 0.5 * (object[-1L, , drop = FALSE] +
        object[-n.obs, , drop = FALSE])
      boot_fun <- function(x, i) colSums(x[i, , drop = FALSE] * y[i])
    }
  )
  result <- boot(data = object, statistic = boot_fun, R = boot, ...)
  if (raw)
    return(result)
  result <- pe_and_ci(result, ci = ci, as.pe = as.pe, type = ci.type,
    fill.nas = what == "A")
  colnames(result) <- object.colnames
  rownames(result) <- paste(what, rownames(result), sep = ".")
  result
}, sealed = SEALED)

