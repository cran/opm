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
    structure(map[ints], cutoffs = range, names = names(x))

  } else { # binary- to multiple-state mode without a gap

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
    ), names = names(x))

  }
}, sealed = SEALED)

setMethod("discrete", MOA, function(x, ...) {
  map_values(object = x, mapping = discrete, ...)
}, sealed = SEALED)

setMethod("discrete", "data.frame", function(x, ..., as.labels = NULL,
    sep = " ") {
  discrete(extract_columns(x, what = "numeric", as.labels = as.labels,
    sep = sep, direct = FALSE), ...)
}, sealed = SEALED)

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

setGeneric("do_disc", function(object, ...) standardGeneric("do_disc"))

setMethod("do_disc", OPMA, function(object, cutoff, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), unify = FALSE) {
  if (!length(cutoff))
    stop(sprintf(
      "'cutoff' must be a non-empty vector if applied to %s objects",
      class(object)))
  x <- aggregated(object, subset = map_param_names(subset, ci = FALSE)[[1L]],
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
  subset <- unname(match.arg(subset, unlist(map_param_names(plain = TRUE))))

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

setMethod("do_disc", "MOPMX", function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = do_disc, ...)
  object
}, sealed = SEALED)

