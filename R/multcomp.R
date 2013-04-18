

################################################################################


#' Multiple comparison of group means
#'
#' This function provides linear-hypothesis testing and multiple comparisons for
#' group means of curve parameters. Note that for most \code{output} modes (see
#' below) it would be an error if this function was called and the package
#' \pkg{multcomp} was unavailable (even though that package is not formally a
#' dependency of \pkg{opm}).
#'
#' @param object Either an \code{\link{OPMS}} object, or a data frame derived
#'   via \code{\link{extract}} containing factor variables that determine
#'   experimental groups for multiple comparison of means and can be selected
#'   using \code{model}.
#'
#' @param model A model formula, or a character vector or a list containing the
#'   names of factors to be included in the model for fitting. For model
#'   specifications using formulas in general, see \code{formula} (in the
#'   \pkg{stats} package). For the way models are used by \pkg{opm} for
#'   selecting metadata entries, see \code{\link{metadata}}.
#'
#'   If \code{object} is of class \code{\link{OPMS}}, \code{model} is passed to
#'   \code{\link{extract}} after removal of the relevant reserved names (see
#'   \code{\link{param_names}}, which can nevertheless be included in the model
#'   formula as they are always contained in the resulting data frame. If
#'   \code{model} is a list or vector, it is used for selecting metadata, too,
#'   and for running the tests automatically converted to a formula, using one
#'   to several operators as specified with \code{ops}.
#'
#'   If the \code{output} argument is set to \sQuote{model}, the function
#'   returns the converted \code{model} argument, which is useful in exploring
#'   the conducted conversions before running the proper tests.
#'
#' @param linfct A specification of the linear hypotheses to be tested
#'   analogously to \code{linfct} in \code{glht}. A variety of objects can be
#'   used for this argument: \itemize{
#'   \item One of the classes of objects accepted by \code{glht} from the
#'   \pkg{multcomp} package as \code{linfct} argument. Such objects will not be
#'   modified. Linear functions can be specified by either the matrix of
#'   coefficients or by symbolic descriptions of one or more linear hypotheses.
#'   See also \code{contrMat} from the \pkg{multcomp} package.
#'   \item An object inheriting from the \sQuote{AsIs} as created by \code{I}
#'   from the \pkg{base} package. Such objects will be converted to an argument
#'   list for and then passed to \code{mcp} from the \pkg{multcomp} package.
#'   \item Other objects will be treated as a selection of factors from the data
#'   just like \code{model}, i.e. they will be converted like any
#'   \code{\link{metadata}} key (but note that character vectors would be passed
#'   to \code{glht}). If this yielded a numeric or logical vector, it would be
#'   treated as specifying the positions of factors within \code{model}. If
#'   names were present, they would be used as the values of the arguments
#'   passed to \code{mcp}. Otherwise \code{opm_opt("contrast.type")} would be
#'   used. (See the \code{type} argument of \code{contrMat}.) The modified
#'   object would then be used as the argument list in a call to \code{mcp}.
#'   }
#'   After the conversions, if any, this argument is passed to \code{glht} as
#'   \code{linfct} argument.
#'
#'   If the \code{output} argument is set to \sQuote{linfct}, the function
#'   returns the converted \code{linfct} argument, which is useful in exploring
#'   the conducted conversions before running the proper tests.
#'
#' @param m.type Character scalar indicating which of the following model types
#'   to use in model fitting: \sQuote{glm}, \sQuote{aov} or \sQuote{lm}. See
#'   the eponymous functions in the \pkg{stats} package for details.
#'
#' @param glht.args List of additional arguments for the multiple comparison
#'   procedure passed to \code{glht}. See \code{glht} in the \pkg{multcomp}
#'   package for details.
#'
#' @param ops Character vector. \code{ops} is ignored if \code{model} is
#'   directly provided as a formula. Otherwise the provided list or character
#'   vector is converted to formula, and \code{ops} then specifies the
#'   operator(s) to insert between the variables in the right part of the
#'   \code{model} formula. Thus, \code{ops} should contain \sQuote{+},
#'   \sQuote{*}, or \sQuote{:} as elements. \sQuote{+} is the default, and the
#'   elements are recycled as usual if necessary. See the description of
#'   \code{formula} for further details (in the \pkg{stats} package).
#'
#' @param output Character scalar determining the main output mode. See below
#'   under \sQuote{Value}.
#'
#' @param split.at Character vector. See \code{\link{extract}}. Cannot be set in
#'   the case of the \code{\link{OPMS}} method.
#'
#' @param ... Optional argument passed to \code{\link{extract}}.
#'
#' @return The kind of object returned by this function are determined by the
#'   \code{output} argument: \describe{
#'     \item{mcp}{The default. An object of class \sQuote{glht} as the result of
#'     the multiple comparison of means. Methods for \code{print},
#'     \code{summary}, \code{confint}, \code{coef} and \code{vcov} are available
#'     for this class. See \code{glht} in the \pkg{multcomp} package for
#'     details.}
#'     \item{data}{Reshaped (\sQuote{flattened}) data frame. It contains one
#'     column for the measured values, one factorial variable determining the
#'     well, one factorial variable for the curve parameter (see
#'     \code{\link{param_names}}) and additional factorial variables selected by
#'     \code{model} as factors. Such a data frame might be of use for
#'     model-building approaches not covered by this function.}
#'     \item{model}{The \code{model} argument \emph{after} the conversions
#'     conducted by \code{opm_mcp}, if any.}
#'     \item{linfct}{The \code{linfct} argument \emph{after} the conversions
#'     conducted by \code{opm_mcp}, if any.}
#'     \item{contrast}{A list of contrast matrices as specified by \code{model}
#'     and \code{linfct}. As these matrices would be guaranteed to fit to
#'     \code{object}, they could serve as template matrices to be further
#'     specified by the user. Note that this only works if \code{linfct} either
#'     is an object of class \sQuote{mcp} or convertible to such an object,
#'     and if its values are character scalars that can be passed as \code{type}
#'     argument to \code{contrMat} from the \pkg{multcomp} package.}
#'   }
#'
#' @keywords htest
#' @export
#'
#' @author Lea A.I. Vaas, Markus Goeker
#' @family multcomp-functions
#' @seealso multcomp::glht multcomp::contrMat stats::lm stats::formula
#'
#' @details This function internally reshapes the data in \code{object} into a
#'   \sQuote{flat} data frame the structure of which is described under
#'   \sQuote{value}. In the default \code{output} mode, \code{glht} from the
#'   \pkg{multcomp} package is applied to this data frame. This causes (general
#'   linear) models and, by indicating a contrast type, user-defined
#'   simultaneous multiple testing procedures to be inferred.
#'
#'   Since either the user or this function itself makes use of \code{mcp}, we
#'   refer to the \sQuote{Details} section of the \code{glht} function. The
#'   \code{mcp} function must be used with care when defining parameters of
#'   interest in two-way ANOVA or ANCOVA models. The definition of treatment
#'   differences might be problem-specific. An automated determination of the
#'   parameters of interest would be impossible and thus only comparisons for
#'   the main effects (ignoring covariates and interactions) would be generated
#'   and a warning issued.
#'
#' @examples
#'
#' # helper function for plotting with better suitable margins
#' plot_with_margin <- function(x, mar, ...) {
#'   old.mar <- par(mar = mar)
#'   on.exit(par(old.mar))
#'   plot(x, ...)
#' }
#'
#' ## OPMS method
#' data(vaas_4)
#'
#' # return the intermediary data frame, do not conduct statistical tests
#' head(x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   output = "data"))
#' stopifnot(is.data.frame(x), dim(x) == c(384, 5))
#'
#' # watch the converted 'model' argument
#' (x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   output = "model"))
#' stopifnot(inherits(x, "formula")) # left side is set automatically
#'
#' # watch the converted 'linfct' argument
#' (x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   linfct = c(Dunnett = 1), output = "linfct"))
#' stopifnot(inherits(x, "mcp"))
#'
#' # create a template contrast matrix
#' (x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
#'   linfct = c(Dunnett = 1), output = "contrast"))
#' stopifnot(is.list(x), sapply(x, inherits, "contrMat"),
#'   names(x) == "Species") # the selection is as specified by 'linfct'
#'
#' # comparison using specified model comparing 'Species' pooled over
#' # complete plates
#' (x <- opm_mcp(vaas_4, model = list("Species"), m.type = "lm",
#'   linfct = c(Dunnett = 1))) # refers to 'Species'
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 1)
#' plot_with_margin(x, c(3, 20, 3, 2)) # creating an informative plot
#'
#' # comparison of only A01 - A04 against each other, Tukey style
#' # note that the left side of the model is set automatically
#' (x <- opm_mcp(vaas_4[, , 1:4],
#'   model = ~ Well + Species, m.type = "lm",
#'   linfct = c(Tukey = 1))) # refers to 'Well'
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 6)
#' plot_with_margin(x, c(3, 18, 3, 2)) # creating an informative plot
#'
#' # Dunnett-type comparison
#' (x <- opm_mcp(vaas_4[, , 1:4], model = ~ Well,
#'   m.type = "lm", linfct = c(Dunnett = 1)))
#' stopifnot(inherits(x, "glht"), length(coef(x)) == 3)
#' plot_with_margin(x, c(3, 20, 3, 2)) # creating an informative plot
#'
#' # manually defined contrast matrix
#' (contr <- opm_mcp(vaas_4[, , 1:4], linfct = c(Tukey = 1),
#'   model = ~ Well, output = "contrast")) # create template, Tukey style
#' contr <- contr$Well[c(1:3, 6), ] # select comparisons of interest
#' (x <- opm_mcp(vaas_4[, , 1:4],
#'   model = ~ Well, m.type = "lm", linfct = contr)) # run tests
#' plot_with_margin(x, c(3, 20, 3, 2)) # creating an informative plot
#'
#' ## data-frame method (usually unnecessary to directly apply it)
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
#'   dataframe = TRUE)
#'
#' # without performing the MCP
#' head(y <- opm_mcp(x, output = "data", model = list("Species", "Strain")))
#' stopifnot(is.data.frame(y), dim(y) == c(384, 5)) # same result as above
#'
#' # now with conducting the test
#' (y <- opm_mcp(x, model = "Species", m.type = "lm",
#'   linfct = c(Dunnett = 1)))
#' stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
#' plot_with_margin(y, c(3, 20, 3, 2)) # creating an informative plot
#'
#' # testing for subsets of object
#' (y <- opm_mcp(subset(x, x$Species == "Escherichia coli"),
#'   linfct = c(Dunnett = 1), model = "Strain", m.type = "lm"))
#' stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
#' plot_with_margin(y, c(3, 15, 3, 2)) # creating an informative plot
#'
setGeneric("opm_mcp",
  function(object, ...) standardGeneric("opm_mcp"))

setMethod("opm_mcp", OPMS, function(object, model, linfct = 1L,
    m.type = "glm", glht.args = list(), ops = "+", output = "mcp", ...) {
  object <- extract(object = object, dataframe = TRUE, ...,
    as.labels = metadata_key(model, FALSE, ops = ops,
      remove = RESERVED_NAMES[c("well", "value", "parameter")]))
  opm_mcp(object = object, model = model, linfct = linfct, ops = ops,
    m.type = m.type, split.at = param_names("split.at"), glht.args = glht.args,
    output = output)
}, sealed = SEALED)

setMethod("opm_mcp", "data.frame", function(object, model, linfct = 1L,
    m.type = c("glm", "lm", "aov"), glht.args = list(), ops = "+",
    output = c("mcp", "data", "model", "linfct", "contrast"),
    split.at = param_names("split.at")) {

  # helper functions
  convert_model <- function(model, ops) {
    enforce_left_side <- function(f) {
      if (length(f) < 3L) # f must be a formula
        f[[3L]] <- f[[2L]]
      f[[2L]] <- as.name(RESERVED_NAMES[["value"]])
      f
    }
    enforce_left_side(metadata_key(model, TRUE, ops = ops))
  }
  convert_hypothesis_spec <- function(linfct, model) {
    if (!length(linfct))
      stop("hypothesis definition 'linfct' must not be empty")
    # note that the glht() methods actually dispatch over 'linfct'...
    used.by.glht <- c("mcp", "matrix", "expression", "character", "means")
    if (inherits(linfct, used.by.glht))
      return(linfct)
    if (inherits(linfct, "AsIs")) # created using I()
      return(do.call(multcomp::mcp, as.list(linfct)))
    linfct <- metadata_key(linfct, FALSE)
    if (is.list(linfct))
      result <- names(linfct)
    else if (is.numeric(linfct) || is.logical(linfct)) {
      result <- names(metadata_key(model, FALSE))[linfct]
      names(result) <- names(linfct)
    } else if (is.character(linfct))
      result <- linfct
    else
      stop("invalid object passed as 'linfct' argument")
    if (is.null(names(result)))
      names(result) <- rep(get("contrast.type", OPM_OPTIONS),
        length.out = length(result))
    result <- structure(names(result), .Names = result)
    do.call(multcomp::mcp, as.list(result))
  }
  convert_data <- function(object, split.at) {
    param.pos <- assert_splittable_matrix(object, split.at)
    # create reshaped data frame and set temporary helper column '_ID' to avoid
    # non-unique values when setting 'row.names'; note according shift of column
    # positions!
    object <- reshape(cbind(`_ID` = seq.int(nrow(object)), object),
      direction = "long",
      idvar = c("_ID", colnames(object)[seq.int(param.pos - 1L)]),
      varying = colnames(object)[seq.int(param.pos + 1L, ncol(object))],
      v.names = RESERVED_NAMES[["value"]],
      timevar = RESERVED_NAMES[["well"]],
      times = colnames(object)[seq.int(param.pos + 1L, ncol(object))])
    rownames(object) <- NULL
    object[, RESERVED_NAMES[["well"]]] <- as.factor(
      object[, RESERVED_NAMES[["well"]]])
    object$`_ID` <- NULL
    object
  }
  contrast_matrices <- function(object, linfct) {
    if (!inherits(linfct, "mcp"))
      stop("in 'contrast' mode, 'linfct' must yield an object of class 'mcp'")
    n <- lapply(object[, names(linfct), drop = FALSE], table)
    mapply(multcomp::contrMat, n = n, type = linfct, SIMPLIFY = FALSE)
  }

  # conversions and early returns, if requested
  case(match.arg(output),
    data = return(convert_data(object, split.at)),
    model = return(convert_model(model, ops)),
    linfct = return(convert_hypothesis_spec(linfct,
      convert_model(model, ops))),
    contrast = return(contrast_matrices(
      convert_data(object, split.at),
      convert_hypothesis_spec(linfct, convert_model(model, ops)))),
    mcp = NULL
  )
  object <- convert_data(object, split.at)
  model <- convert_model(model, ops)
  linfct <- convert_hypothesis_spec(linfct, model)

  # necessary at this stage because otherwise glht() does not find its
  # dependencies
  if (!suppressWarnings(suppressPackageStartupMessages(require(
      multcomp, quietly = TRUE, warn.conflicts = FALSE))))
    stop("package 'multcomp' must be available to run this function")

  # fit the linear model according to 'm.type', then run glht()
  model <- do.call(match.arg(m.type), list(formula = model, data = object))
  glht.args <- c(list(model = model, linfct = linfct), as.list(glht.args))
  result <- do.call(glht, glht.args)

  # check the number of calculated tests
  if (length(confint(result)$confint[, 1L]) > 20L)
    warning("number of performed comparisons exceeds 20")

  result
}, sealed = SEALED)


################################################################################



