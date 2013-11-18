setGeneric("summary")

setMethod("summary", OPM, function(object, ...) {
  result <- list(
    Class = class(object),
    `From file` = csv_data(object, what = "filename"),
    `Hours measured` = hours(object),
    `Number of wells` = length(wells(object)),
    `Plate type` = plate_type(object),
    Position = csv_data(object, what = "position"),
    `Setup time` = csv_data(object, what = "setup_time"),
    Metadata = sum(rapply(object@metadata, function(item) 1L)),
    Aggregated = has_aggr(object),
    Discretized = has_disc(object),
    ...
  )
  class(result) <- "OPM_Summary"
  result
}, sealed = SEALED)

setMethod("summary", OPMS, function(object, ...) {
  result <- lapply(X = object@plates, FUN = summary, ...)
  attr(result, "overall") <- list(Dimensions = dim(object),
    Aggregated = sum(has_aggr(object)), Discretized = sum(has_disc(object)),
    Plate.type = plate_type(object))
  class(result) <- "OPMS_Summary"
  result
}, sealed = SEALED)

setMethod("summary", MOPMX, function(object, ...) {
  select_parts <- function(x) if (is.null(y <- attr(x, "overall")))
      c(list(Length = 1L, Plate.type = x[["Plate type"]]),
        x[c("Aggregated", "Discretized")])
    else
      c(list(Length = y[["Dimensions"]][[1L]]),
        y[c("Plate.type", "Aggregated", "Discretized")])
  if (length(object)) {
    result <- lapply(lapply(X = object, FUN = summary, ...), select_parts)
    result <- do.call(rbind, lapply(result, as.data.frame))
    if (!is.null(n <- names(object)))
      rownames(result) <- make.names(n, TRUE)
  } else {
    result <- as.data.frame(matrix(NA, 0L, 4L, FALSE))
    colnames(result) <- c("Length", "Plate.type", "Aggregated", "Discretized")
  }
  class(result) <- c("MOPMX_Summary", oldClass(result))
  result
}, sealed = SEALED)

setMethod("show", OPMX, function(object) {
  print(summary(object))
}, sealed = SEALED)

setMethod("show", MOPMX, function(object) {
  print(summary(object))
}, sealed = SEALED)

setMethod("show", CMAT, function(object) {
  if (typeof(object) == "list") {
    object[] <- lapply(object, paste, collapse = "/")
    storage.mode(object) <- "character"
  }
  callNextMethod()
}, sealed = SEALED)

NULL

print.OPMD_Listing <- function(x, ...) {
  cat(formatDL(x = x, ...), sep = "\n")
  invisible(x)
}

print.OPMS_Listing <- function(x, ...) {
  for (name in rownames(x)) {
    cat(name, gsub(".", "-", name, FALSE, TRUE), sep = "\n")
    cat(formatDL(x[name, ], ...), sep = "\n")
    cat("\n")
  }
  invisible(x)
}

print.OPM_Summary <- function(x, ...) {
  lapply(formatDL(x = names(x), y = unlist(x), ...), FUN = cat, sep = "\n")
  invisible(x)
}

print.OPMS_Summary <- function(x, ...) {
  for (i in seq_along(x)) {
    cat(i, sep = "\n")
    print(x[[i]])
    cat("\n")
  }
  tmpl <- "=> %s object with %i plates (%i aggregated, %i discretized)"
  tmpl <- paste(tmpl, "of type '%s', %i well(s) and about %i time point(s).")
  y <- attr(x, "overall")
  cat(sprintf(tmpl, OPMS, y$Dimensions[1L], y$Aggregated, y$Discretized,
    y$Plate.type, y$Dimensions[3L], y$Dimensions[2L]), sep = "\n")
  invisible(x)
}

print.MOPMX_Summary <- function(x, ...) {
  NextMethod()
  cat("", sprintf(
    "=> MOPMX object with %i element(s), details are shown above.", nrow(x)),
    " Access the elements with [[ or $ to apply certain methods.",
    sep = "\n")
  invisible(x)
}

print.print_easy <- function(x, ...) {
  to_map <- function(items) if (is.null(names(items)))
    items
  else
    as.list(items)
  cat(as.yaml(if (is.list(x))
    rapply(x, to_map, "ANY", NULL, "replace")
  else
    to_map(x)))
  invisible(x)
}

setGeneric("ranging", function(object, ...) standardGeneric("ranging"))

setMethod("ranging", "numeric", function(object, extended = !zscores,
    zscores = FALSE, na.rm = TRUE, fac = 1) {
  LL(extended, zscores, na.rm)
  result <- if (zscores) {
    if (extended) {
      center <- median(object, na.rm = na.rm)
      (object - center) / mad(object, center = center, na.rm = na.rm)
    } else
      (object - mean(object, na.rm = na.rm)) / sd(object, na.rm = na.rm)
  } else {
    if (extended) {
      min.object <- min(object, na.rm = na.rm)
      (object - min.object) / (max(object, na.rm = na.rm) - min.object)
    } else
      object / max(abs(object), na.rm = na.rm)
  }
  must(result * fac)
}, sealed = SEALED)

setMethod("ranging", MOA, function(object, ...) {
  map_values(object = object, mapping = ranging, ...)
}, sealed = SEALED)

setMethod("ranging", "list", function(object, ...) {
  relist(ranging(unlist(as.relistable(object)), ...))
}, sealed = SEALED)

setGeneric("guess_cex", function(object, ...) standardGeneric("guess_cex"))

setMethod("guess_cex", "numeric", function(object) {
  0.5 * sqrt(96 / object)
}, sealed = SEALED)

setGeneric("best_layout",
  function(object, ...) standardGeneric("best_layout"))

setMethod("best_layout", "numeric", function(object, by = 0.75) {
  LL(object, by)
  if (object < 0)
    stop("a negative number of fields makes no sense")
  if (object < 2)
    return(c(object, object))
  large <- ceiling(sqrt((1 / by) * object)) # => error unless 'by' is numeric
  small <- ceiling(object / large)
  c(large, small)
}, sealed = SEALED)

setGeneric("best_range",
  function(object, ...) standardGeneric("best_range"))

setMethod("best_range", "numeric", function(object, target,
    align = c("center", "left", "right"),
    offset = 0, prop.offset = 0) {
  orig.range <- range(object)
  orig.diff <- orig.range[2L] - orig.range[1L]
  case(length(target), target <- orig.diff, LL(target))
  LL(offset, prop.offset)
  if (offset == 0)
    offset <- target * prop.offset
  total <- target + 2 * offset
  if (total < orig.diff) {
    fmt <- "target (%s) + 2 * offset (%s) smaller than original range (%s)"
    stop(sprintf(fmt, target, offset, orig.diff))
  }
  case(match.arg(align),
    center = {
      add <- total / 2
      mean(orig.range) + c(-add, add)
    },
    left = orig.range[1L] + c(-offset, target + offset),
    right = orig.range[2L] + c(-target - offset, offset)
  )
}, sealed = SEALED)

setGeneric("improved_max",
  function(object, ...) standardGeneric("improved_max"))

setMethod("improved_max", "numeric", function(object, by = 10) {
  LL(by)
  ceiling(max(object) / by) * by + by # => error unless 'by' is numeric
}, sealed = SEALED)

setMethod("improved_max", OPMX, function(object, theor.max = TRUE, by = 10) {
  if (L(theor.max))
    return(THEOR_RANGE[2L])
  improved_max(max(object), by)
}, sealed = SEALED)

setGeneric("draw_ci", function(object, ...) standardGeneric("draw_ci"))

setMethod("draw_ci", "numeric", function(object, col = "blue", cex = 1,
    na.action = c("warn", "error", "ignore")) {
  LL(object, .wanted = 4L)
  if (any(is.na(c(left <- object[1L], right <- object[3L])))) {
    msg <- "cannot draw CI because left or right margin is 'NA'"
    case(match.arg(na.action),
      warn = warning(msg),
      error = stop(msg),
      ignore = NULL
    )
  }
  if (is.na(y <- object[4L]))
    stop("position on y axis must be provided")
  segments(left, y, right, y, lwd = cex, col = col)
  text(left, y, "(", col = col, cex = cex)
  text(right, y, ")", col = col, cex = cex)
  if (!is.na(point <- object[2L]))
    points(point, y, col = col, lwd = cex, pch = 19L, cex = cex)
  invisible(object)
}, sealed = SEALED)

setGeneric("negative_control",
  function(object, ...) standardGeneric("negative_control"))

setMethod("negative_control", OPMX, function(object, neg.ctrl) {
  if (!length(neg.ctrl) || is.numeric(neg.ctrl))
    neg.ctrl
  else if (is.character(neg.ctrl)) {
    result <- vapply(neg.ctrl, function(x)
      tryCatch(minmax(object, neg.ctrl), error = function(e) {
        warning("cannot get negative control from selected position ",
        "(deleted?); error was: ", conditionMessage(e))
        -1
      }), numeric(1L))
    result[result >= 0]
  } else if (is.logical(neg.ctrl)) {
    if (L(neg.ctrl))
      minmax(object)
    else
      NULL
  } else
    stop("object 'neg.ctrl' must be either empty or a 'character', 'logical' ",
      "or 'numeric' vector")
}, sealed = SEALED)

setGeneric("main_title", function(object, ...) standardGeneric("main_title"))

setMethod("main_title", OPMX, function(object, settings) {
  if (is.character(settings) || is.expression(settings))
    settings <- list(predef = settings)
  else if (is.logical(settings))
    settings <- list(use = settings)
  else if (is.numeric(settings))
    settings <- list(max = settings)
  else
    settings <- as.list(settings)
  if (!is.null(settings$predef) && nzchar(settings$predef))
    return(settings$predef) # nzchar() works for expressions, too
  settings <- insert(settings, use = TRUE, full = TRUE, .force = FALSE)
  if (settings$use) {
    settings$use <- NULL
    do.call(plate_type, c(list(object = object), settings))
  } else
    NULL
}, sealed = SEALED)

try_select_colors <- function(set) {
  tryCatch(select_colors(set), error = function(e) set)
}

default_color_regions <- function(colors, space, bias, n) {
  colorRampPalette(colors = unique(colors), space = space, bias = bias)(n)
}

setGeneric("xy_plot", function(x, ...) standardGeneric("xy_plot"))

setMethod("xy_plot", OPM, function(x, col = "midnightblue", lwd = 1,
    neg.ctrl = "A01", base.col = "grey10", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE,
    strip.fmt = list(), striptext.fmt = list(),
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPMS

  # Setup
  layout <- best_layout(dim(x)[2L])
  y.max <- improved_max(x, theor.max)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  ## END must be synchronized with xy_plot,OPMS

  # Plot
  xyplot(
    # Principally unchangeable arguments
    create_formula("`%s` ~ `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = flatten(x, ...), type = "l", layout = layout,
    as.table = TRUE,
    # Curve colour and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Main annotation
    main = main, xlab = xlab, ylab = ylab,
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # The panels
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    })

}, sealed = SEALED)

setMethod("xy_plot", OPMS, function(x, col = opm_opt("colors"), lwd = 1,
    neg.ctrl = "A01", base.col = "black", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE, space = "top",
    strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE,
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPM

  # Setup
  layout <- best_layout(dim(x)[3L])
  y.max <- improved_max(x, theor.max)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  ## END must be synchronized with xy_plot,OPM

  # OPMS-specific addition of defaults
  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  # Selection of a colour set
  col <- try_select_colors(col)

  # Conversion
  data <- flatten(x, ...)

  # Assignment of colours to plates
  param <- flattened_to_factor(object = data, sep = legend.sep)
  key.text <- levels(param)
  if (length(col) < length(key.text))
    stop("colour should be by plate or metadata, but there are too few colours")
  key.col <- col[seq_along(key.text)]
  col <- col[param]

  names(data)[match(RESERVED_NAMES[["plate"]], names(data))] <- "_GROUPING"

  # Plot
  xyplot(
    # Principally unchangeable arguments
    create_formula("`%s` ~ `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = data, type = "l", layout = layout,
    as.table = TRUE, groups = `_GROUPING`,
    # Curve colours and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    }
  )

}, sealed = SEALED)

setMethod("xy_plot", "data.frame", function(x, f, groups,
    col = opm_opt("colors"), lwd = 1, neg.ctrl = NULL, base.col = "black",
    base.lwd = lwd, main = groups, xlab = elem(f, 3L:2L), ylab = elem(f, 2L),
    draw.grid = TRUE, space = "top", strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE, ...) {

  elem <- function(x, i) {
    pos <- 1L
    while (length(x) > 1L) {
      x <- x[[i[[pos]]]]
      if (pos < length(i))
        pos <- pos + 1L
    }
    as.character(x)
  }

  f <- as.formula(f)
  groups <- as.character(groups)

  # Layout
  xvar <- as.factor(x[, elem(f, 3L:3L)])
  layout <- best_layout(length(levels(xvar)))

  # Put grouping variable together
  pos <- match(groups, names(x))
  if (any(isna <- is.na(pos)))
    stop(sprintf("could not find '%s' in 'x'", groups[isna][1L]))
  x$`_GROUPING` <- do.call(paste, c(x[, pos, drop = FALSE], sep = legend.sep))
  x$`_GROUPING` <- as.factor(x$`_GROUPING`)

  # Assignment of colours
  col <- try_select_colors(col)
  key.text <- levels(x$`_GROUPING`)
  if (length(key.text) > length(col))
    stop("number of colours must be at least as large as number of groups")
  key.col <- col[seq_along(key.text)]
  col <- key.col

  # Adding default to settings lists. insert() is used here: for some reason
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  legend.fmt <- insert(as.list(legend.fmt), space = space, .force = FALSE)

  xyplot(
    # Principally unchangeable arguments
    x = f, data = x, type = "l", layout = layout,
    as.table = TRUE, groups = `_GROUPING`,
    # Curve colours (panel height is omitted)
    col = col,
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(strip.custom, strip.fmt), par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        panel.grid(h = -1, v = -1)
      if (length(neg.ctrl))
        panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      panel.xyplot(..., lwd = lwd)
    },
    ...
  )
}, sealed = SEALED)

setGeneric("level_plot", function(x, ...) standardGeneric("level_plot"))

setMethod("level_plot", OPM, function(x, main = list(),
    colors = opm_opt("color.borders"), panel.headers = FALSE, cex = NULL,
    strip.fmt = list(), striptext.fmt = list(), legend.sep = " ",
    space = "Lab", bias = 0.5, num.colors = 200L, ...) {
  if (is.null(cex))
    cex <- guess_cex(dim(x)[2L])
  main <- main_title(x, main)
  levelplot(create_formula("`%s` ~ `%s` * `%s`",
      RESERVED_NAMES[c("value", "time", "well")]),
    data = flatten(x, ...), main = main,
    col.regions = default_color_regions(colors, space, bias, num.colors),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)

setMethod("level_plot", OPMS, function(x, main = list(),
    colors = opm_opt("color.borders"), panel.headers = TRUE, cex = NULL,
    strip.fmt = list(), striptext.fmt = list(), legend.sep = " ",
    space = "Lab", bias = 0.5, num.colors = 200L, ...) {
  dims <- dim(x)
  if (is.null(cex))
    cex <- guess_cex(dims[3L])
  data <- flatten(x, ...)
  if (is.null(panel.headers) || (is.logical(panel.headers) && !panel.headers))
    strip.fmt <- FALSE
  else {
    if (is.logical(panel.headers))
      panel.headers <- flattened_to_factor(object = data, sep = legend.sep)
    if (!is.expression(panel.headers))
      panel.headers <- as.character(panel.headers)
    strip.fmt <- insert(as.list(strip.fmt), bg = "grey90",
      factor.levels = panel.headers)
    strip.fmt <- do.call(strip.custom, strip.fmt)
  }
  levelplot(create_formula("`%s` ~ `%s` * `%s` | `%s`",
      RESERVED_NAMES[c("value", "time", "well", "plate")]),
    data = data, main = main_title(x, main),
    col.regions = default_color_regions(colors, space, bias, num.colors),
    strip = strip.fmt, as.table = TRUE, layout = c(dims[1L], 1L),
    par.strip.text = as.list(striptext.fmt),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)

setGeneric("ci_plot", function(object, ...) standardGeneric("ci_plot"))

setMethod("ci_plot", "data.frame", function(object, rowname.sep = " ",
    prop.offset = 0.04, align = "center", col = "blue", na.action = "warn",
    draw.legend = TRUE, legend.field = c(1, 1), x = "topleft", xpd = TRUE,
    vline = 0, split.at = param_names("split.at"), crr = 0.75, ...) {

  single_plot <- function(col.pos) {
    plot(x = NULL, y = NULL, xlim = ranges[, col.pos], ylim = ylim,
      main = colnames(object)[col.pos], yaxt = "n", xlab = "", ylab = "")
    if (length(vline))
      abline(v = vline, lty = 2L, col = "gray60")
    axis(2L, at = chunk.pos, labels = row.names)
    vapply(chunk.pos, FUN = function(pos) {
      pe <- object[pos, col.pos]
      left <- object[pos + 1L, col.pos]
      right <- object[pos + 2L, col.pos]
      draw_ci(c(left, pe, right, pos), col = col, na.action = na.action)
    }, numeric(4L))
  }

  # Determine the position used for splitting the data frame
  param.pos <- assert_splittable_matrix(object, split.at)

  # Check the triplet structure and determine all triplet start positions
  if (nrow(object) %% 3L != 0L)
    stop("need data frame with 3 * n rows")
  chunk.pos <- seq_len(nrow(object))
  chunk.pos <- chunk.pos[chunk.pos %% 3L == 1L]
  row.names <- as.character(seq_along(chunk.pos))

  # Reorder the matrix and construct the legend if necessary
  if (param.pos > 1L) {
    factor.pos <- seq.int(1L, param.pos - 1L)
    ordering <- do.call(order, as.list(object[, factor.pos, drop = FALSE]))
    object <- object[ordering, , drop = FALSE]
    legend <- as.matrix(object[chunk.pos, factor.pos, drop = FALSE])
    legend <- apply(legend, 1L, paste, collapse = rowname.sep)
    legend <- paste(row.names, legend, sep = ": ")
  } else
    legend <- NULL

  # Reduce to the numeric part of matrix
  object <- as.matrix(object[, seq.int(param.pos + 1L, ncol(object)),
    drop = FALSE])

  # Determine field range (which is set to be uniform)
  ranges <- apply(object, 2L, range, na.rm = TRUE)
  max.range <- max(apply(ranges, 2L, FUN = function(x) x[2L] - x[1L]))
  ranges <- apply(ranges, 2L, FUN = best_range, target = max.range,
    align = align, prop.offset = prop.offset)
  ylim <- best_range(chunk.pos, target = NULL, prop.offset = prop.offset)

  # Panel layout and plotting of individual panels
  old.par <- par(mfcol = best_layout(ncol(object), crr))
  on.exit(par(old.par))
  lapply(seq_len(ncol(object)), FUN = single_plot)

  # Legend
  if (draw.legend && !is.null(legend)) {
    if (length(legend.field) > 1L)
      par(mfg = legend.field[1L:2L])
    else
      par(mfcol = c(1L, 1L))
    legend(x = x, legend = legend, xpd = xpd, ...)
  }
  invisible(legend)

}, sealed = SEALED)

setMethod("ci_plot", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), ...) {
  ci_plot(extract(object, as.labels = as.labels, subset = subset,
    dataframe = TRUE, ci = TRUE), split.at = param_names("split.at"), ...)
}, sealed = SEALED)

setGeneric("heat_map", function(object, ...) standardGeneric("heat_map"))

setMethod("heat_map", "matrix", function(object,
    hclustfun = "ward", distfun = "euclidean", scale = "none",
    r.groups = "row.groups", r.col = opm_opt("colors"),
    c.groups = "col.groups", c.col = opm_opt("colors"),
    magnif = 4, cexRow = magnif[1L] / sqrt(nrow(object)),
    cexCol = magnif[length(magnif)] / sqrt(ncol(object)),
    borders = c(0.55, 0.75),
    margins = if (use.fun[1L] == "gplots")
      c(borders[1L] * cexCol * max(nchar(colnames(object))),
      borders[length(borders)] * cexRow * max(nchar(rownames(object))))
    else
      c(5, 5),
    col = opm_opt("heatmap.colors"), asqr = FALSE, lmap = 1L:3L,
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
    colors <- try_select_colors(colors)
    groups <- as.factor(groups)
    if (length(colors) < length(levels(groups)))
      stop("more groups than colours given")
    structure(colors[groups], names = as.character(groups))
  }

  do_asqr <- function(x, percent) {
    if (percent) {
      if (any(x < 0, na.rm = TRUE) || any(x > 100, na.rm = TRUE))
        warning("in 'percent' mode, 'x' should be between 0 and 100")
      else if (all(x <= 1, na.rm = TRUE))
        warning("percentages expected, but everything < 1")
      return(100 * asin(sqrt(x / 100)))
    }
    asin(sqrt(x))
  }

  clustfun <- get_fun(hclustfun, hclust)
  dfun <- get_fun(distfun, dist)
  arg.list <- list(scale = scale, cexRow = cexRow, cexCol = cexCol,
    hclustfun = clustfun, distfun = dfun, margins = margins, col = col, ...)

  row.side.colors <- get_side_colors(r.groups, r.col, for.rows = TRUE)
  if (!is.null(row.side.colors))
    arg.list$RowSideColors <- row.side.colors
  col.side.colors <- get_side_colors(c.groups, c.col, for.rows = FALSE)
  if (!is.null(col.side.colors))
    arg.list$ColSideColors <- col.side.colors

  case(match.arg(use.fun),
    gplots = {
      if (suppressMessages(suppressWarnings(require(gplots, quietly = TRUE,
          warn.conflicts = FALSE)))) {
        arg.list <- insert(arg.list, trace = "none", .force = FALSE)
        heatmap_fun <- gplots::heatmap.2
      } else {
        warning("package 'gplots' requested, but not available")
        heatmap_fun <- heatmap
      }
    },
    stats = heatmap_fun <- heatmap
  )

  if (typeof(object) == "logical")
    if (length(lmap))
      object[] <- map_values(c(object), lmap)
    else
      storage.mode(object) <- "integer"

  if (is.na(L(asqr)) || asqr)
    object[] <- do_asqr(object, is.na(asqr))

  result <- do.call(heatmap_fun, c(list(x = object), arg.list))
  result$colColMap <- col.side.colors
  result$rowColMap <- row.side.colors
  invisible(result)

}, sealed = SEALED)

setMethod("heat_map", "data.frame", function(object, as.labels,
    as.groups = NULL, sep = " ", ...) {
  invisible(heat_map(extract_columns(object, what = "numeric", direct = FALSE,
    as.labels = as.labels, as.groups = as.groups, sep = sep), ...))
}, sealed = SEALED)

setMethod("heat_map", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), as.groups = NULL, sep = " ",
    extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = as.groups, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(heat_map(do.call(extract, extract.args), ...))
}, sealed = SEALED)

setGeneric("radial_plot", function(object, ...) standardGeneric("radial_plot"))

setMethod("radial_plot", "matrix", function(object, as.labels = NULL,
    subset = TRUE, sep = " ", extract.args = list(), rp.type = "p",
    radlab = FALSE, show.centroid = TRUE, show.grid.labels = 1, lwd = 3,
    mar = c(2, 2, 2, 2), line.col = opm_opt("colors"), draw.legend = TRUE,
    x = "bottom", y = NULL, xpd = TRUE, pch = 15, legend.args = list(),
    point.symbols = NA, point.col = NA, poly.col = NA,
    main = paste0(as.labels, sep = sep), ...) {
  LL(radlab, show.centroid, show.grid.labels, draw.legend, xpd, pch)
  line.col <- try_select_colors(line.col)
  changed.par <- NULL
  on.exit(if (!is.null(changed.par))
    par(changed.par))
  changed.par <- radial.plot(lengths = object[, subset, drop = FALSE],
    labels = colnames(object), rp.type = rp.type, radlab = radlab,
    show.centroid = show.centroid, lwd = lwd, mar = mar,
    show.grid.labels = show.grid.labels, line.col = line.col,
    point.symbols = point.symbols, point.col = point.col, poly.col = poly.col,
    main = main, ...)
  if (!is.null(rn <- rownames(object))) {
    if (draw.legend) {
      legend.args <- insert(as.list(legend.args), x = x, y = y, col = line.col,
        legend = rn, pch = pch, .force = TRUE)
      do.call(legend, legend.args)
    }
    result <- suppressWarnings(cbind(rn, line.col))
    result <- result[seq_len(nrow(object)), , drop = FALSE]
    result <- structure(result[, 2L], names = as.vector(result[, 1L]))
  } else
    result <- NULL
  invisible(result)
}, sealed = SEALED)

setMethod("radial_plot", "data.frame", function(object, as.labels,
    subset = "numeric", sep = " ", extract.args = list(), ...) {
  invisible(radial_plot(extract_columns(object, what = subset,
    direct = FALSE, as.labels = as.labels, sep = sep), ...))
}, sealed = SEALED)

setMethod("radial_plot", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), sep = " ", extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = NULL, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(radial_plot(do.call(extract, extract.args), ...))
}, sealed = SEALED)

