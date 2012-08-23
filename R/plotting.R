


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
#' @return Numeric vector or matrix.
#' @keywords internal
#'
setMethod("ranging", "numeric", function(object, extended = !zscores,
    zscores = FALSE, na.rm = TRUE, fac = 1) {
  LL(extended, zscores, na.rm)
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
  LL(object, by)
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


################################################################################
################################################################################
#
# Paper sizes and adapted PDF creation
#


#' Paper size
#'
#' Determine the size for a given standard paper format.
#'
#' @param object Numeric vector, character vector, or missing. If missing, the
#'   default \sQuote{papersize} entry is queried using \code{getOption}, and
#'   the result passed to the other methods. If a numeric vector, specifying
#'   the paper size in the \sQuote{DIN} series. If a character vecotr, 
#' @param inches Logical scalar. If \code{TRUE}, output unit is inches, 
#'   otherwise millimeters.
#' @param series Character scalar indicating the \sQuote{DIN} series to assume.
#' @param landscape Logical vector. Where \code{FALSE}, \sQuote{portrait} paper
#'   orientation is assumed. For the character method, this has only an effect
#'   for paper size specifiers such as \sQuote{letter} that do not belong to 
#'   the \sQuote{DIN} series. For the \sQuote{DIN} series, append \sQuote{R} to
#'   the specifier to obtain \sQuote{landscape} orientation.
#' @param ... Optional arguments passed between the methods.
#' @return Numeric matrix with columns \sQuote{width} and \sQuote{height} and
#'   \code{object} as row names (if ot was a character vector).
#' @seealso base::getOption
#' @family plotting-functions
#' @keywords dplot
#' @export
#' @examples
#' query <- c("A4", "Letter", "unknown")
#' (x <- paper_size(query))
#' stopifnot(is.matrix(x), is.numeric(x), rownames(x) == query)
#' stopifnot(colnames(x) == c("height", "width"), is.na(x["unknown", ]))
#' (y <- paper_size(4))
#' stopifnot(identical(y, x[1L, , drop = FALSE]))
#' @references \url{http://en.wikipedia.org/wiki/Paper_size}
#'
setGeneric("paper_size", function(object, ...) standardGeneric("paper_size"))

setMethod("paper_size", "missing", function(...) {
  paper_size(object = getOption("papersize"), ...)
}, sealed = SEALED)

setMethod("paper_size", "numeric", function(object, series = c("A", "B", "C"), 
    landscape = FALSE, ...) {
  pattern <- sprintf("%s%%i", match.arg(series))
  must(pattern[landscape] <- sprintf("%sR", pattern[landscape]))
  paper_size(sprintf(pattern, abs(object)), ...)
}, sealed = SEALED)

setMethod("paper_size", "character", function(object, inches = FALSE, 
    landscape = FALSE) {
  parse_din_string <- function(x) {
    get_orientation <- function(x) {
      x <- toupper(sub("^.*\\d", "", x, perl = TRUE))
      x[!nzchar(x)] <- "L"
      x
    }  
    get_series <- function(x) toupper(substr(x, 1L, 1L))
    get_size <- function(x) {
      as.integer(gsub("[A-Z]", "", x, perl = TRUE, ignore.case = TRUE))
    }
    y <- gsub("\\W", "", x, perl = TRUE)
    data.frame(series = get_series(y), size = get_size(y), 
               orientation = get_orientation(y), 
               row.names = x, stringsAsFactors = FALSE)
  }
  long_size_in_mm <- function(series, size) {
    get_size <- function(n, m) 0.2 + 1000 * 2^-(0.5 * n - m)
    m <- numeric(length = length(series))
    m[series == "A"] <- 0.25
    m[series == "B"] <- 0.5
    m[series == "C"] <- 0.375
    m[m == 0] <- NA_real_
    get_size(n = size, m = m)
  }
  LL(inches, landscape)
  x <- parse_din_string(object)
  x$height <- long_size_in_mm(x$series, x$size)
  across <- x$orientation == "R"
  x[!across, "width"] <- x[!across, "height"] / sqrt(2)
  x[across, "width"] <- x[across, "height"]
  x[across, "height"] <- x[across, "width"] / sqrt(2)
  x <- as.matrix(x[, 4L:5L])
  wanted <- is.na(x[, 1L]) & 
    tolower(rownames(x)) %in% rownames(SPECIAL_PAPER_SIZES)
  x[wanted, ] <- SPECIAL_PAPER_SIZES[tolower(rownames(x)[wanted]), 2L:1L]
  if (landscape)
    x[wanted, ] <- x[wanted, 2L:1L]
  if (inches)
    x <- x / 25.4
  x
}, sealed = SEALED)


#' Create PDF file
#'
#' A slightly adapted wrapper for \code{pdf} from the \pkg{grDevices} package.
#'
#' @param file See \code{grDevices::pdf}.
#' @param paper Like \code{grDevices::pdf}, but its \code{\link{paper_size}} is
#'   used to determine \sQuote{width} and \sQuote{height} of the plotting 
#'   region.
#' @param prop Numeric vector. Its values should be between 0 and 1. Its first
#'   element is multiplied with the width of \code{paper} to yield the width of
#'   the plotting region. Its last element is multiplied with the height of 
#'   \code{paper} to yield the height of the plotting region.
#' @param ... Optional arguments passed to \code{grDevices::pdf}.
#' @export
#' @return \code{NULL}. As a side effect, \code{file} is opened.
#' @family plotting-functions
#' @seealso grDevices::pdf
#' @keywords IO
#' @examples
#' \dontrun{
#'   mypdf("example.pdf")
#'   ## create some plots...
#'   dev.off()
#' }
#'
mypdf <- function(file, paper = "a4r", prop = 0.9, ...) {
  paper.size <- paper_size(paper)
  width <- prop[1L] * paper.size[, "width"]
  height <- prop[length(prop)] * paper.size[, "height"]
  pdf(file = file, paper = paper, width = width, height = height, ...)
}


################################################################################
################################################################################
#
# Colours
#


## NOTE: not an S4 method because conversion is done

#' Sort colours
#'
#' A helper function for methods such as \code{\link{xy_plot}}.
#' Arrange colours to achieve that neighboring colours are most distinct with
#' respect to their RGB coordinates. This is done as follows: (1) Euclidean
#' distances between the RGB coordinates of the input colours are calculated;
#' (2) the distances are inversed; (3) a principal-coordinate analysis is
#' conducted on these inversed distances; (4) the input colours are sorted
#' according to the first principal coordinate.
#'
#' @param col Vector. Names or hexadecimal codes of the colours to be sorted.
#'   Might also be an integer vector, see \code{col2rgb} from the
#'   \pkg{grDevices} package for details. Duplicate RGB coordinates and
#'   unknown names will cause an error.
#' @export
#' @return Character vector (rearranged input names).
#' @family plotting-functions
#' @seealso grDevices::col2rgb
#' @keywords color
#' @note The resulting vector could as well be used in reverse order.
#' @examples
#' (x <- max_rgb_contrast(c("darkred", "darkblue", "blue", "red")))
#' y <- c("darkblue", "red", "blue", "darkred")
#' stopifnot(identical(x, y) || identical(x, rev(y)))
#'
max_rgb_contrast <- function(col) {
  col.rgb <- t(col2rgb(col))
  rownames(col.rgb) <- col
  pco <- cmdscale(1 / log(dist(col.rgb)), k = 1L)
  names(sort(pco[, 1L]))
}


################################################################################


## NOTE: not an S4 method because check is done using match.arg()

#' Select colours
#'
#' Select a set of colours for plotting. See \code{\link{xy_plot}} for usage
#' example. This is not normally directly called
#' by an \pkg{opm} user but could be used for testing before doing some serious
#' plotting.
#'
#' @param set Character scalar. Name of the colour vector to use. Colour vectors
#'   have been optimized for maximum contrast between adjacent colours, either
#'   manually or using \code{\link{max_rgb_contrast}}. Names
#'   ending in \sQuote{.i} indicate vectors in inverse order (compared to the
#'   vector with the same name except \sQuote{.i}).
#' @export
#' @return Character vector (names of colours).
#' @family plotting-functions
#' @keywords color
#' @seealso grDevices::colors grDevices::rainbow grDevices::grey
#' @references \url{http://www.colorbrewer.org}
#' @examples
#' (x <- select_colors("nora"))
#' (y <- select_colors("nora.i"))
#' stopifnot(is.character(x), length(x) > 0L, identical(x, rev(y)))
#'
select_colors <- function(
    set = c("w3c", "w3c.i", "nora", "nora.i", "brewer", "brewer.i", 
      "roseobacter", "roseobacter.i")) {
  case(match.arg(set),
    w3c = W3C_COLORS[W3C_NAMES_MAX_CONTRAST],
    w3c.i = rev(W3C_COLORS[W3C_NAMES_MAX_CONTRAST]),
    nora = NORAS_COLORS,
    nora.i = rev(NORAS_COLORS),
    brewer = BREWER_COLORS,
    brewer.i = rev(BREWER_COLORS),
    roseobacter = ROSEOBACTER_COLORS,
    roseobacter.i = rev(ROSEOBACTER_COLORS)
  )
}


################################################################################
################################################################################
#
# Plotting helper functions
#


#' Draw CI
#'
#' Draw a confidence interval.
#'
#' @param object Four-element numeric vector containing (i) the left margin of
#'   the CI; (ii) the point estimate; (iii) the right margin; (iv) the position
#'   on the y axis. The point estimate can be \code{NA} at any time; whether
#'   the margins can also be \code{NA} depends on \code{na.action}.
#' @param col Character scalar. Name of the colour to be used.
#' @param cex Numeric scalar. Magnification for CI margin symbols and point
#'   estimate. Also affects line width, and proportionally so.
#' @param na.action Character scalar. What to do if a margin value is
#'   \code{NA}.
#' @return \code{object}, returned invisibly.
#' @keywords internal
#'
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



################################################################################


setGeneric("xy_plot", function(x, ...) standardGeneric("xy_plot"))
#' XY plot
#'
#' Customized plotting of a single or multiple PM plate(s), using \code{xyplot}
#' from the \pkg{lattice} package. The optimal number of rows and columns is
#' estimated  from the number of selected wells. An optimal font size of the
#' panel headers is also chosen automatically, but can also be adapted by the
#' user, much like most aspects of the resulting graphics output.
#' In the case of the \code{\link{OPMS}} method, if metadata are selected,
#' curve colours are determined according to the combinations of these metadata
#' entries, otherwise each plate gets its own colour.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} object.
#'
#' @param col For the \code{\link{OPM}} method, just a character scalar
#'   (colour name) determining the line colour. For the \code{\link{OPMS}}
#'   method, either a character vector with colour codes or one of
#'   the arguments of \code{\link{select_colors}} (for picking one of the
#'   predefined colour sets). It is an error if fewer colours are chosen than
#'   the number of plate grouping levels (see the \code{...} argument below).
#'   For user-chosen colour sets, keep in mind that the sets are not checked
#'   for duplicates, and see \code{\link{max_rgb_contrast}} as a method for
#'   optimally arranging user-defined colours.
#' @param lwd Numeric scalar determining the line width.
#'
#' @param neg.ctrl Determine the height of a horizontal baseline drawn in each
#'   panel. If \code{NULL} or \code{FALSE}, no baseline will be drawn. If
#'   \code{TRUE}, the baseline's height is the value of \code{\link{minmax}}.
#'   If a character scalar, \code{neg.ctrl} is interpreted as the name of the
#'   wells regarded as negative control, and the baseline's height becomes the
#'   value of \code{\link{minmax}} applied to these wells only. Set
#'   \code{neg.ctrl} to a numeric value for assigning the height directly
#'   (at your own risk).
#' @param base.col Character scalar. Baseline colour (ignored if no baseline is
#'   drawn).
#' @param base.lwd Numeric scalar determining the width of the baseline
#'   (ignored if no baseline is drawn).
#'
#' @param main The settings controlling the construction of the main title.
#'   If a list, a named list with the following entries (if
#'   missing, they are replaced by the respective defaults):
#'   \describe{
#'     \item{predef}{Character scalar or expression. Predefined title. If set,
#'       the other entries are ignored.}
#'     \item{use}{Logical scalar. If \code{FALSE}, returns \code{NULL}.}
#'     \item{...}{Other arguments are passed to \code{\link{plate_type}}.}
#'   }
#'   If \code{settings} is not a list but a character scalar or an expression,
#'   this is used as the \sQuote{predef} entry of the above-mentioned list. If
#'   not a list but a logical scalar, it is used as the \sQuote{use} entry of
#'   this list. If not a list but a numeric value, it is used as the
#'   \sQuote{max} entry of this list.
#' @param xlab Character scalar. Title of x-axis. Use \code{NULL} to turn it
#'   off.
#' @param ylab Character scalar. Title of y-axis. Use \code{NULL} to turn it
#'   off.
#'
#' @param theor.max Logical scalar. Use the theoretical maximum as maximum of
#'   the y-axis? If \code{FALSE}, use the empirical maximum with a small
#'   offset.
#'
#' @param draw.grid Logical scalar. Insert background grid?
#'
#' @param space Character scalar indicating the position of the legend; either
#'   \sQuote{top}, \sQuote{bottom}, \sQuote{left} or \sQuote{right}. Might be
#'   overwritten by \code{legend.fmt}.
#'
#' @param strip.fmt List controlling the format of the description strip above
#'   each panel. For instance, the background colour is set using the
#'   \sQuote{bg} key. For further details, see \code{strip.custom} from the
#'   \pkg{lattice} package. Note that the \strong{content} of these
#'   descriptions is determined by arguments passed from \code{xy_plot} to
#'   \code{\link{wells}}; see there for details.
#' @param striptext.fmt List controlling the textual description at the top of
#'   each panel. For instance, the relative text size is set using the
#'   \sQuote{cex} key, the colour by \sQuote{col}, the font by \sQuote{font}
#'   and the number of lines by \sQuote{lines}. The latter might be of interest
#'   in conjunction with the \code{paren.sep} argument of \code{\link{wells}}.
#'   See the argument \sQuote{par.strip.text} of \code{xyplot} from the
#'   \pkg{lattice} package for details.
#'
#' @param legend.fmt List controlling where and how to draw the legend. The
#'   content of the legend (mainly a description of the assignment of the
#'   colours to the curves) is determined automatically. See argument
#'   \sQuote{key} of \code{xyplot} from the \pkg{lattice} package for details.
#' @param legend.sep Character scalar. Relevant only if more than one columns
#'   of metadata have been selected; will then be used as separator to join
#'   their names in the legend.
#' @param draw.legend Logical scalar. If \code{FALSE}, no legend is drawn, and
#'   the two aforementioned arguments are ignored.
#'
#' @param ... Arguments that are passed to \code{\link{flatten}}. For the
#'   \code{\link{OPMS}} method, \code{include} is particularly important:
#'   the selected metadata are joined
#'   into a single factor, and the assignment of plates to this factor's levels
#'   determines the curve colour for each plate. That is, each combination of
#'   metadata entries as chosen using \code{include} yields one colour. If no
#'   metadata are selected (the default), each plate gets a colour of its own.
#'   Also note that arguments passed via \code{\link{flatten}} to
#'   \code{\link{wells}} can be given here which determine the content of the
#'   panel description.
#'
#' @export
#' @family plotting-functions
#' @return An object of class \sQuote{trellis}. See \code{xyplot} from the
#'   \pkg{lattice} package for details.
#' @references Sarkar, D. 2008 \emph{Lattice: Multivariate Data Visualization
#'   with R.} New York: Springer, 265 p.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#' @keywords hplot
#' @seealso lattice::xyplot
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' xy_plot(vaas_1) # note the default main title built from the plate type
#'
#' x <- vaas_1[, 11:22]
#' # Gives a warning message: we have deleted the default negative control:
#' xy_plot(x)
#' # Turn the baseline off => no warning:
#' xy_plot(x, neg.ctrl = NULL)
#' # Or guess a baseline
#' xy_plot(x, neg.ctrl = 100)
#' # Some like it ugly:
#' xy_plot(x, neg.ctrl = 100, col = "pink", base.col = "yellow", main = "Ugly")
#'
#' # OPMS method
#' data(vaas_4)
#' # Color by species and strain; note default main title
#' xy_plot(vaas_4, include = c("Species", "Strain"))
#' # Use the largest of the negative-control maxima as baseline
#' xy_plot(vaas_4, include = c("Species", "Strain"),
#'   neg.ctrl = max(vaas_4, "A01"))
#'
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
  lattice::xyplot(
    # Principally unchangeable arguments
    Value ~ Time | Well, data = flatten(x, ...), type = "l", layout = layout,
    as.table = TRUE,
    # Curve colour and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Main annotation
    main = main, xlab = xlab, ylab = ylab,
    # Description above each panel
    strip = do.call(lattice::strip.custom, strip.fmt),
      par.strip.text = striptext.fmt,
    # The panels
    panel = function(...) {
      if (draw.grid)
        lattice::panel.grid(h = -1, v = -1)
      if (!is.null(neg.ctrl))
        lattice::panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      lattice::panel.xyplot(..., lwd = lwd)
    })

}, sealed = SEALED)

setMethod("xy_plot", OPMS, function(x, col = opm_opt("xy.colors"), lwd = 1,
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
  col <- tryCatch(select_colors(col), error = function(e) col)

  # Conversion
  data <- flatten(x, ...)

  # Assignment of colours to plates
  param <- flattened_to_factor(object = data, sep = legend.sep)
  key.text <- levels(param)
  if (length(col) < length(key.text))
    stop("colour should be by plate or metadata, but there are too few colours")
  key.col <- col[seq_along(key.text)]
  col <- col[param]

  # Plot
  lattice::xyplot(
    # Principally unchangeable arguments
    Value ~ Time | Well, data = data, type = "l", layout = layout,
    as.table = TRUE, groups = Plate,
    # Curve colours and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(lattice::strip.custom, strip.fmt),
      par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        lattice::panel.grid(h = -1, v = -1)
      if (!is.null(neg.ctrl))
        lattice::panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      lattice::panel.xyplot(..., lwd = lwd)
    }
  )

}, sealed = SEALED)


################################################################################


## NOTE: not an S4 method because conversion is done

#' Colour regions
#'
#' Create default colour regions for use with \code{\link{level_plot}}.
#'
#' @param colors Character or integer vector with at least two distinct
#'   colours. If \code{NULL} or empty, default colours are chosen.
#' @param space Passed to \code{colorRampPalette}.
#' @param bias Passed to \code{colorRampPalette}.
#' @param n Passed to the function returned by \code{colorRampPalette}.
#' @return Character vector of colour codes.
#' @keywords internal
#'
default_color_regions <- function(colors, space, bias, n) {
  #if (length(colors) == 0L)
  #  colors <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  #brewer.div <- colorRampPalette(unique(colors), space = "Lab", bias = 0.5)
  #brewer.div(200)
  colorRampPalette(colors = unique(colors), space = space, bias = bias)(n)
}


################################################################################


setGeneric("level_plot", function(x, ...) standardGeneric("level_plot"))
#' Levelplot
#'
#' Levelplot for \code{\link{OPM}} and \code{\link{OPMS}} objects using the
#' function from the \pkg{lattice} package.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}}  object.
#'
#' @param main The settings controlling the construction of the main title.
#'   Works like the \code{main} argument of \code{\link{xy_plot}}.
#' @param colors Character vector indicating the colours (at least two).
#'
#' @param panel.headers \code{NULL}, logical scalar, expression or character
#'   vector. \code{NULL} and \code{FALSE} turn panel headers off. \code{TRUE}
#'   causes the panel headers to be constructed from the plate numbers or those
#'   metadata that were included by \code{\link{flatten}} (see there).
#'   Character vectors and expressions are directly used for the text within
#'   these panel headers.
#' @param cex Numeric scalar. Magnification of axis annotation. If \code{NULL},
#'   automatically adapted to the number of wells (at least a good guess is
#'   made).
#'
#' @param strip.fmt List controlling the format of the description strip above
#'   each panel. For instance, the background colour is set using the
#'   \sQuote{bg} key. For further details, see \code{strip.custom} from the
#'   \pkg{lattice} package. \code{strip.fmt} is ignored if panel.headers is
#'   \code{FALSE}.
#' @param striptext.fmt List controlling the format of the text within the
#'   strip above each panel. See \code{\link{xy_plot}} for details, which has
#'   an argument of the same name.
#' @param legend.sep Character scalar. This works like the eponymous argument
#'   to \code{\link{flatten}} (see there); it is ignored unless metadata are
#'   chosen for constructing the panel headers.
#'
#' @param space Character scalar passed to \code{colorRampPalette} from the 
#'   \pkg{grDevices} package. These and the following arguments are for 
#'   fine-tuning the colour palette used for plotting.
#' @param bias Numeric scalar also passed to \code{colorRampPalette}.
#' @param num.colors Numeric scalar passed to the function returned by 
#'   \code{colorRampPalette}.
#'
#' @param ... Arguments that are passed to \code{\link{flatten}}.
#'
#' @export
#' @return An object of class \sQuote{trellis}. See \code{levelplot} from the
#'   \pkg{lattice} package for details.
#' @family plotting-functions
#' @keywords hplot
#'
#' @references Jacobsen, J. S., Joyner, D. C., Borglin, S. E., Hazen, T. C.,
#'   Arkin, A. P. et al. 2007 Visualization of growth curve data from phenotype
#'   microarray experiments. \emph{11th International Conference on Information
#'   Visualization (IV07).} Zuerich, Switzerland, July 4-6 2007. Published by
#'   the IEEE Computer Society.
#' @references Sarkar, D. 2008 \emph{Lattice: Multivariate Data Visualization
#'   with R.} New York: Springer, 265 p.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#'
#' @seealso lattice::levelplot grDevices::colorRampPalette
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' level_plot(vaas_1, main = "Levelplot example")
#'
#' # OPMS method
#' data(vaas_4)
#' # headers include species and strain
#' level_plot(vaas_4, include = c("Species", "Strain"))
#'
setMethod("level_plot", OPM, function(x, main = list(), 
    colors = opm_opt("color.borders"), cex = NULL, space = "Lab", bias = 0.5,
    num.colors = 200L, ...) {
  if (is.null(cex))
    cex <- guess_cex(dim(x)[2L])
  main <- main_title(x, main)
  lattice::levelplot(Value ~ Time * Well, data = flatten(x, ...), main = main,
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
    strip.fmt <- do.call(lattice::strip.custom, strip.fmt)
  }
  lattice::levelplot(Value ~ Time * Well | Plate, data = data,
    main = main_title(x, main), 
    col.regions = default_color_regions(colors, space, bias, num.colors),
    strip = strip.fmt, as.table = TRUE, layout = c(dims[1L], 1L),
    par.strip.text = as.list(striptext.fmt),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)


################################################################################
################################################################################
#
# CI plot methods
#


setGeneric("ci_plot", function(object, ...) standardGeneric("ci_plot"))
#' Plot point estimates with CIs
#'
#' Draw point estimates with their confidence intervals. The data frame method
#' is not normally directly called by an \pkg{opm} user but via the
#' \code{\link{OPMS}} method. This one is used for comparing aggregated values
#' together with their confidence intervals between plates. This method can in
#' most cases \strong{not} be applied to entire plates but to selected wells
#' only.
#'
#' @param object Dataframe or \code{\link{OPMS}} object. If an
#'   \code{\link{OPMS}} object, it is in most cases necessary to restrict
#'   the plates to at most about one dozen wells. See \code{\link{[}}
#'   for how to achieve this. The data frame should be as exported by
#'   \code{\link{extract}} with \code{ci}
#'   set to \code{TRUE}. There must be a column named \sQuote{Parameter}
#'   followed by columns with only numeric values. Columns before the
#'   \sQuote{Parameter} column, if any, are used for grouping. The rows must
#'   entirely comprise triplets representing (i) the point estimate, (ii)
#'   the lower and (iii) the upper confidence interval.
#'
#' @param as.labels List. Metadata to be joined and used to draw a legend.
#'   Ignored if \code{NULL}.
#' @param subset Character scalar. The parameter to plot. Only a single one
#'   can be selected. See \code{\link{param_names}} for the options.
#'
#' @param rowname.sep Character scalar. Used when joining explanatory columns
#'   into row labels of the plots.
#'
#' @param prop.offset Numeric scalar. A proportional offset that is added to
#'   the vertical range of the panels (after determining the maximum range
#'   among all panels to ensure consistency within the plot).
#' @param align Character scalar. How to apply the offset; one of
#'   \sQuote{center}, \sQuote{left} and \sQuote{right}.
#'
#' @param col Character scalar. Colour to be used.
#'
#' @param na.action Character scalar. What to do if a confidence interval
#'   contains \code{NA} values; one of \sQuote{ignore}, \sQuote{warn} and
#'   \sQuote{error}.
#'
#' @param draw.legend Logical scalar. Ignored if there are no explanatory
#'   columns.
#' @param legend.field Two-element numeric vector. Indicates the panel in which
#'   the legend is drawn. Subsequent arguments work then relative to this
#'   panel. If \code{legend.field} has less then two fields, the number of
#'   panels is set to 1 (the entire plot), and the legend is drawn relative to
#'   that.
#' @param x Legend position, passed to \code{legend} from the \pkg{graphics}
#'   package. Ignored unless \code{draw.legend} is \code{TRUE}.
#' @param xpd Logical scalar. Also passed to that function.
#' @param vline Numeric scalar with the position on the y-axis of a vertical 
#'   line to be drawn. Ignored if \code{NULL}.
#' @param ... Optional other arguments passed to \code{legend}, or arguments
#'   passed from the \code{\link{OPMS}} method to the data frame method.
#'
#'
#' @note \itemize{
#'  \item The default placement of the legend is currently not necessarily very
#'    useful.
#'  \item When plotting entire PM plates, the \sQuote{mar} parameter of
#'     \code{par} most likely would need to be set to a lower value, but it
#'     is recommended to plot only subsets of plates, i.e. selected wells.
#'  }
#'
#' @references Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. 2012
#'   Visualization and curve parameter estimation strategies for efficient
#'   exploration of Phenotype Microarray kinetics. \emph{PLoS ONE} \strong{7},
#'   e34846.
#'
#' @return Character vector describing the plot's legend, returned invisibly.
#' @export
#' @family plotting-functions
#' @seealso graphics::plot
#' @keywords hplot
#' @examples
#'
#' data(vaas_4)
#'
#' # most of the parameters used here are explained under the data.frame
#' # method of ci_plot()
#' x <- ci_plot(vaas_4[, , 1:3], as.labels = list("Species", "Strain"),
#'    subset = "A", x = "bottomright", legend.field = NULL)
#' # note that the values on the y axes are drawn to scale
#' x
#' stopifnot(is.character(x), identical(length(x), 4L))
#' # ... and that the return value contains the legend (even if it is not drawn)
#'
setMethod("ci_plot", "data.frame", function(object, rowname.sep = " ",
    prop.offset = 0.04, align = "center", col = "blue", na.action = "warn",
    draw.legend = TRUE, legend.field = c(1, 1), x = "topleft", xpd = TRUE,
    vline = 0, ...) {

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

  # Check the triplet structure and determine all triplet start positions
  if (nrow(object) %% 3L != 0L)
    stop("need data frame with 3 * n rows")
  chunk.pos <- seq.int(nrow(object))
  chunk.pos <- chunk.pos[chunk.pos %% 3L == 1L]
  row.names <- as.character(seq_along(chunk.pos))

  # Determine the 'Parameter' position, used for splitting the data frame
  param.pos <- which(colnames(object) == "Parameter")
  if (length(param.pos) != 1L)
    stop("need data frame with one column called 'Parameter'")
  if (param.pos == ncol(object))
    stop("the data columns are missing")

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
  old.par <- par(mfcol = best_layout(ncol(object)))
  on.exit(par(old.par))
  lapply(seq.int(ncol(object)), FUN = single_plot)

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

setMethod("ci_plot", OPMS, function(object, as.labels, subset = "A", ...) {
  ci_plot(extract(object, as.labels = as.labels, subset = subset,
    dataframe = TRUE, ci = TRUE), ...)
}, sealed = SEALED)


################################################################################
################################################################################
#
# Heatmap functions for matrices and data frames
#


setGeneric("heat_map", function(object, ...) standardGeneric("heat_map"))
#' Heat map
#'
#' A wrapper for \code{heatmap} from the \pkg{stats} package and
#' \code{heatmap.2} from the \pkg{gplots} package with some adaptations
#' likely to be useful for OmniLog(R) data. The data-frame and \sQuote{OPMS}
#' methods extract a numeric matrix from a given data frame or \sQuote{OPMS}
#' object and pass the result to the matrix method.
#'
#' @param object Matrix, data frame or \sQuote{OPMS} object. The matrix method
#'   is mainly designed for curve-parameter matrices as created by 
#'   \code{\link{extract}} but can be used with any numeric matrix. If a data 
#'   frame, it must contain at least one column with numeric data.
#'
#' @param as.labels Character, numeric or logical vector indicating the
#'   positions of the columns to be joined and used as row labels. If
#'   \code{NULL} or empty, the row names of \code{object} are used. See
#'   \code{\link{extract}} for details.
#'
#' @param as.groups Character, numeric or logical vector indicating the
#'   positions of the columns to be joined and used as group indicators. If
#'   \code{NULL} or empty, groups are ignored.
#'
#' @param sep Character scalar determining how to join row and group names. See
#'   \code{\link{extract}} for details.
#'
#' @param subset Character scalar passed to the \sQuote{OPMS} method of
#'   \code{\link{extract}}.
#' @param extract.args Optional list of arguments passed to that method.
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
#' @param r.groups Determines the plotting of a colour bar indicating row
#'   groups. If \code{NULL}, ignored. If a function, applied to the row names
#'   of \code{object}; should then yield one group name for each row name. If a
#'   character scalar, the name of an attribute of \code{object} that contains
#'   the row group affiliations (ignored if this is not found). Otherwise,
#'   coerced to \sQuote{character} mode. Finally the groups are converted to a
#'   factor and used for selecting from \code{r.col}.
#' @param r.col Character vector of colour names used by \code{r.groups}.
#'   Ignored if that is \code{NULL}.
#' @param c.groups Determines the plotting of a colour bar indicating column
#'   groups. If \code{NULL}, ignored. If a function, applied to the column
#'   names of \code{object}; should then yield one group name for each column
#'   name. If a character scalar, the name of an attribute of \code{object}
#'   that contains the column group affiliations (ignored if this is not
#'   found). Otherwise, coerced to \sQuote{character} mode. Finally the groups
#'   are converted to a factor and used for selecting from \code{c.col}.
#' @param c.col Character vector of colour names used by \code{c.groups}.
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
#' @param col Character vector containing the proper heatmap colours.
#' @param ... Optional arguments passed to \code{heatmap} or \code{heatmap.2}.
#'   Note that some defaults of \code{heatmap.2} are overwritten even though
#'   this is not transparent from the argument list of \code{heat_map}. If set
#'   explicitly, the default \code{heatmap.2} behaviour is restored.
#'   \code{...} also represents all arguments passed from the \sQuote{OPMS} or
#'   data-frame methods to the matrix method.
#'
#' @param use.fun Character scalar. If \sQuote{gplots}, it is attempted to load
#'   the \pkg{gplots} package and use its \code{heatmap.2} function (the
#'   default). If this fails, a warning is issued, and \code{heatmap} from the
#'   \pkg{stats} package (the default) is called instead.
#'
#' @export
#' @return A list as output by \code{heatmap} or \code{heatmap.2} with the
#'   additional entries \sQuote{rowColMap} or \sQuote{colColMap} giving the
#'   mapping(s) of group names to colours as named character vector(s), if this
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
#' # 'OPMS' method
#' hm.2 <- heat_map(vaas_4, as.labels = "Strain", as.groups = "Species")
#' stopifnot(identical(hm[-3], hm.2[-3]))
#'
#' # Data-frame method
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
        heatmap_fun <- stats::heatmap
      }
    },
    stats = heatmap_fun <- stats::heatmap
  )

  if (storage.mode(object) == "logical")
    storage.mode(object) <- "integer"
  
  result <- do.call(heatmap_fun, c(list(x = object), arg.list))
  result$colColMap <- col.side.colors
  result$rowColMap <- row.side.colors
  invisible(result)

}, sealed = SEALED)

setMethod("heat_map", "data.frame", function(object, as.labels,
    as.groups = NULL, sep = " ", ...) {
  invisible(heat_map(extract(object, as.labels = as.labels, 
    as.groups = as.groups, sep = sep), ...))
}, sealed = SEALED)

setMethod("heat_map", OPMS, function(object, as.labels, subset = "A",
    as.groups = NULL, sep = " ", extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = as.groups, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(heat_map(do.call(extract, extract.args), ...))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Radial plot
#


setGeneric("radial_plot", function(object, ...) standardGeneric("radial_plot"))
#' Radial plot
#'
#' A wrapper for \code{radial.plot} from the \pkg{plotrix} package with some
#' adaptations likely to be useful for OmniLog(R) data. The data frame and
#' \sQuote{OPMS} methods extract a numeric matrix from a given data frame or
#' \sQuote{OPMS} object and pass the result to the matrix method.
#'
#' @param object Data frame, numeric matrix or \sQuote{OPMS} object (with
#'   aggregated values) to be plotted.
#'
#' @param rp.type Character vector. These and the following arguments are
#'   passed to \code{plotrix::radial.plot}. See there for details.
#' @param radlab Logical scalar.
#' @param show.centroid Logical scalar.
#' @param show.grid.labels Logical scalar.
#' @param lwd Numeric scalar.
#' @param mar Numeric vector of length 4.
#' @param line.col Character or numeric vector.
#' @param ... Optional arguments passed to \code{plotrix::radial.plot}.
#'
#' @param draw.legend Logical scalar. Whether to draw a legend. Ignored unless
#'   \code{object} has row names (because these are used to generate the
#'   description).
#' @param x Legend position, passed to \code{legend} from the \pkg{graphics}
#'   package. Ignored unless \code{draw.legend} is \code{TRUE}.
#' @param y Optional 2nd legend coordinate. Also passed to that function.
#' @param xpd Logical scalar. Also passed to that function.
#' @param pch Integer scalar. Also passed to that function.
#' @param legend.args List of optional other arguments passed to that function.
#'
#' @param as.labels Character, numeric or logical vector indicating the
#'   positions of the columns to be joined and used as row labels. If
#'   \code{NULL} or empty, the row names of \code{object} are used. See
#'   \code{\link{extract}} for details.
#' @param sep Character scalar determining how to join row names. See
#'   \code{\link{extract}} for details.
#'
#' @param subset Character scalar passed to the \sQuote{OPMS} method of
#'   \code{\link{extract}}.
#' @param extract.args Optional list of arguments passed to that method.
#'
#' @export
#' @family plotting-functions
#' @seealso plotrix::radial.plot graphics::legend
#' @keywords hplot
#'
#' @return A vector with the row names of \code{object} as names and the
#'   corresponding colours as values, equivalent to the legend; \code{NULL} if
#'   no row names are present.
#'
#' @note The default positioning of the legend is not necessarily very useful,
#'   but suitable combinations of \code{margin}, \code{x} and \code{y} can be
#'   found for given data sizes. Plotting entire plates usually makes not much
#'   sense (see the examples).
#'
#' @examples
#'
#' data("vaas_4")
#'
#' # Matrix method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"))
#' (y <- radial_plot(x[, 1:5]))
#' stopifnot(is.character(y), names(y) == rownames(x))
#'
#' # 'OPMS' method
#' (yy <- radial_plot(vaas_4[, , 1:5], as.labels = list("Species", "Strain")))
#' stopifnot(identical(y, yy))
#'
#' # Data-frame method
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
#' (y <- radial_plot(x[, 1:8], as.labels = c("Species", "Strain")))
#' stopifnot(is.character(y), names(y) == paste(x$Species, x$Strain))
#'
setMethod("radial_plot", "matrix", function(object, rp.type = "p",
    radlab = FALSE, show.centroid = TRUE, show.grid.labels = 1, lwd = 3,
    mar = c(2, 2, 2, 2), line.col = select_colors(), draw.legend = TRUE,
    x = "bottom", y = NULL, xpd = TRUE, pch = 15, legend.args = list(), ...) {
  LL(radlab, show.centroid, show.grid.labels, draw.legend, xpd, pch)
  on.exit(par(changed.par))
  changed.par <- plotrix::radial.plot(lengths = object,
    labels = colnames(object), rp.type = rp.type, radlab = radlab,
    show.centroid = show.centroid,  lwd = lwd, mar = mar,
    show.grid.labels = show.grid.labels, line.col = line.col, ...)
  if (!is.null(rn <- rownames(object))) {
    if (draw.legend) {
      legend.args <- insert(as.list(legend.args), x = x, y = y, col = line.col,
        legend = rn, pch = pch, .force = TRUE)
      do.call(graphics::legend, legend.args)
    }
    result <- suppressWarnings(cbind(rn, line.col))
    result <- result[seq.int(nrow(object)), , drop = FALSE]
    result <- structure(.Data = result[, 2L], .Names = as.vector(result[, 1L]))
  } else
    result <- NULL
  invisible(result)
}, sealed = SEALED)

setMethod("radial_plot", "data.frame", function(object, as.labels, sep = " ",
    ...) {
  invisible(radial_plot(extract(object, as.labels = as.labels, sep = sep), ...))
}, sealed = SEALED)

setMethod("radial_plot", OPMS, function(object, as.labels, subset = "A",
    sep = " ", extract.args = list(), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = NULL, subset = subset,
    dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  invisible(radial_plot(do.call(extract, extract.args), ...))
}, sealed = SEALED)


################################################################################


setGeneric("main_title", function(object, ...) standardGeneric("main_title"))
#' Create main title
#'
#' Create a title used as value of the \sQuote{main} argument of the plotting
#' functions.
#'
#' @param object \code{\link{OPMX}} object.
#' @param settings See the \code{main} argument of \code{\link{xy_plot}}.
#' @return Character scalar or \code{NULL}.
#' @keywords internal
#'
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


################################################################################


setGeneric("negative_control",
  function(object, ...) standardGeneric("negative_control"))
#' Negative control
#'
#' Helper function to determine the value of a measurement interpretable as
#' negative control.
#'
#' @param object \code{\link{OPMX}} object.
#' @param neg.ctrl If \code{NULL} or \code{FALSE}, ignore \code{data} and
#'   return \code{NULL}. If \code{TRUE}, call \code{minmax} with \code{data} as
#'   sole argument. If a character scalar, call \code{max} with \code{data} as
#'   first and \code{neg.ctrl} as second argument. If \code{neg.ctrl} is a
#'   numeric value, it is returned.
#' @return Numeric scalar or \code{NULL}.
#' @keywords internal
#'
setMethod("negative_control", OPMX, function(object, neg.ctrl) {
  if (is.numeric(neg.ctrl) || is.null(neg.ctrl))
    neg.ctrl
  else if (is.character(neg.ctrl))
    tryCatch(minmax(object, neg.ctrl), error = function(e) {
      warning("cannot get negative control from selected position ",
        "(deleted?); error was: ", conditionMessage(e))
      NULL
    })
  else if (is.logical(neg.ctrl)) {
    if (neg.ctrl)
      minmax(object)
    else
      NULL
  } else
    stop("object 'neg.ctrl' must be either 'NULL', 'character', 'logical' or ",
      "'numeric'")
}, sealed = SEALED)


################################################################################


setGeneric("improved_max",
  function(object, ...) standardGeneric("improved_max"))
#' Maximum plus offset
#'
#' Return the maximal value of an object plus a certain offset.
#'
#' @param object Numeric vector or \sQuote{OPMX} object.
#' @param theor.max Logical scalar. Use the theoretical or the real improved
#'   maximum? If \code{TRUE}, \code{by} is ignored.
#' @param by Numeric scalar.
#' @return Numeric scalar. Let \code{n} be the smallest integer value for which
#'   \code{n * by >= object} holds. The result is then equal to
#'   \code{(n + 1) * by}.
#' @keywords internal
#'
setMethod("improved_max", "numeric", function(object, by = 10) {
  LL(by)
  ceiling(max(object) / by) * by + by # => error unless 'by' is numeric
}, sealed = SEALED)

setMethod("improved_max", OPMX, function(object, theor.max = TRUE, by = 10) {
  if (L(theor.max))
    return(THEOR_MAX)
  improved_max(max(object), by)
}, sealed = SEALED)


################################################################################


