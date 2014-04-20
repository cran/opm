\name{opm_mcp}
\alias{opm_mcp}
\title{Multiple comparison of group means}
\arguments{
  \item{object}{Either an \code{\link{OPMS}} object, or a
  data frame derived via \code{\link{extract}} containing
  factor variables that determine experimental groups for
  multiple comparison of means and can be selected using
  \code{model}.}

  \item{model}{A model formula, or a character vector or a
  list containing the names of factors to be included in
  the model for fitting.  In order to join two or more
  metadata-variables into one factor use pseudo-function
  \code{J} (described in \code{\link{extract}}). This is
  necessary especially when \code{linfct = Pairs}, see the
  examples below.  For model specifications using formulae
  in general, see \code{formula} (in the \pkg{stats}
  package).  For the way models are used by \pkg{opm} for
  selecting metadata entries, see \code{\link{metadata}}.

  If \code{object} is of class \code{\link{OPMS}},
  \code{model} is passed to \code{\link{extract}} after
  removal of the relevant reserved names (see
  \code{\link{param_names}}, which can nevertheless be
  included in the model formula as they are always
  contained in the resulting data frame. If \code{model} is
  a list or vector, it is used for selecting metadata, too,
  and for running the tests automatically converted to a
  formula, using one to several operators as specified with
  \code{ops}. Non-syntactical names within \code{formula}
  are converted to syntactical ones for use with
  \code{glht}. This is done in the same way in the data
  frame passed to that function.

  If the \code{output} argument is set to \sQuote{model},
  the function returns the converted \code{model} argument,
  which is useful in exploring the conducted conversions
  before running the proper tests.}

  \item{linfct}{A specification of the linear hypotheses to
  be tested analogously to \code{linfct} in \code{glht}. A
  variety of objects can be used for this argument:
  \itemize{

  \item One of the classes of objects accepted by
  \code{glht} from the \pkg{multcomp} package as
  \code{linfct} argument. Such objects will not be
  modified. Linear functions can be specified by either the
  matrix of coefficients or by symbolic descriptions of one
  or more linear hypotheses.  The set of existing types of
  contrast is extended with the contrast type
  \sQuote{Pairs}. Here all pair-wise comparison concerning
  the first entry in \code{model} are computed.
  Alternatively, the factor whose levels should determine
  the pairs can be addressed directly with, for example,
  \code{linfct = c(Pairs.Well = 1)}.

  The Dunnett-type contrast has the special feature, that
  users are free to directly define the group which should
  serve as the control or base in the set of contrasts. In
  analogy to \code{Pairs}, the name of the level, separated
  from the string \sQuote{Dunnett} by any sign, can be
  stated. See examples below and in the vignettes and
  further \code{contrMat} from the \pkg{multcomp} package.

  In situations where metadata have non-syntactic names,
  special signs are exchanged against dots. When applying
  \code{linfct = c(Pairs = 1)} or \code{linfct = c(Dunnett
  = 1)} with the above mentioned extension, the sign
  between the \code{linfct} name and the metadata name must
  not be a dot.

  \item An object inheriting from the \code{AsIs} as
  created by \code{I} from the \pkg{base} package. Such
  objects, irrespective of their class, will be converted
  to an argument list for and then passed to \code{mcp}
  from the \pkg{multcomp} package.

  \item Other objects will be treated as a selection of
  factors from the data just like \code{model}, i.e. they
  will be converted like any \code{\link{metadata}} key
  (but note that character vectors would be passed to
  \code{glht}). If this yielded a numeric or logical
  vector, it would be treated as specifying the positions
  of factors within \code{model}. If names were present,
  they would be used as the values of the arguments passed
  to \code{mcp}. Otherwise \code{opm_opt("contrast.type")}
  would be used. (See the \code{type} argument of
  \code{contrMat}.) The modified object would then be used
  as the argument list in a call to \code{mcp}.

  } After the conversions, if any, this argument is passed
  to \code{glht} as \code{linfct} argument.

  If the \code{output} argument is set to \kbd{linfct}, the
  function returns the converted \code{linfct} argument,
  which is useful in exploring the conducted conversions
  before running the proper tests.}

  \item{m.type}{Character scalar indicating which of the
  following model types to use in model fitting: \kbd{glm},
  \kbd{aov} or \kbd{lm}. See the eponymous functions in the
  \pkg{stats} package for details.}

  \item{rhs}{Numeric vector passed to \code{glht} in the
  \pkg{multcomp} package. Also considered when creating
  contrasts of the \sQuote{Pairs} type.}

  \item{alternative}{Character scalar also passed to that
  function (but only if \code{linfct} is or yields a
  matrix), and also relevant for \sQuote{Pairs}-type
  contrasts.}

  \item{glht.args}{List of additional arguments for the
  multiple comparison procedure passed to \code{glht}. See
  \code{glht} in the \pkg{multcomp} package for details.}

  \item{ops}{Character vector. \code{ops} is ignored if
  \code{model} is directly provided as a formula. Otherwise
  the provided list or character vector is converted to
  formula, and \code{ops} then specifies the operator(s) to
  insert between the variables in the right part of the
  \code{model} formula. Thus, \code{ops} should contain
  \sQuote{+}, \sQuote{*}, or \sQuote{:} as elements.
  \sQuote{+} is the default, and the elements are recycled
  as usual if necessary. See the description of
  \code{formula} for further details (in the \pkg{stats}
  package).}

  \item{output}{Character scalar determining the main
  output mode. See below under \sQuote{Value}.}

  \item{split.at}{Character vector. See
  \code{\link{extract}}. Cannot be set in the case of the
  \code{\link{OPMS}} method.}

  \item{sep}{Character scalar (comprising a single
  character) passed to \code{\link{extract}}.}

  \item{...}{Optional arguments passed to
  \code{\link{extract}}. Most of them would be passed to
  \code{\link{wells}} for creating substrate names. Some
  restrictions are necessary here if the resulting object
  shall latter on be analysed with \code{\link{annotated}};
  see there for details. This holds particularly if
  \code{object} is of class \code{\link{MOPMX}}. In that
  case, setting \code{full} to \code{FALSE} is likely to
  cause most combinations of wells and plates to be omitted
  because the well names get non-unique.}
}
\value{
  The kind of object returned by this function are
  determined by the \code{output} argument: \describe{
  \item{mcp}{The default. An object of class \code{glht} as
  the result of the multiple comparison of means. Methods
  for \code{print}, \code{summary}, \code{confint},
  \code{coef} and \code{vcov} are available for this class.
  See \code{glht} in the \pkg{multcomp} package for
  details.} \item{data}{Reshaped (\sQuote{flattened}) data
  frame of the class \code{\link{OPM_MCP_OUT}}. It contains
  one column for the measured values, one factorial
  variable determining the well, one factorial variable for
  the curve parameter (see \code{\link{param_names}}) and
  additional factorial variables selected by \code{model}
  as factors. The column names are converted to syntactical
  names. Such a data frame might be of use for
  model-building approaches not covered by this function.}
  \item{model}{The \code{model} argument \emph{after} the
  conversions conducted by \code{opm_mcp}, if any.}
  \item{linfct}{The \code{linfct} argument \emph{after} the
  conversions conducted by \code{opm_mcp}, if any.}
  \item{contrast}{A list of contrast matrices as specified
  by \code{model} and \code{linfct}. As these matrices
  would be guaranteed to fit to \code{object}, they could
  serve as template matrices to be further specified by the
  user. Note that this only works if \code{linfct} either
  is an object of class \code{mcp} or convertible to such
  an object, and if its values are character scalars that
  can be passed as \code{type} argument to \code{contrMat}
  from the \pkg{multcomp} package.} }
}
\description{
  This function provides linear-hypothesis testing and
  multiple comparisons for group means of curve parameters.
  Note that for most \code{output} modes (see below) it
  would be an error if this function was called and the
  package \pkg{multcomp} was unavailable (even though that
  package is not formally a dependency of \pkg{opm}).
}
\details{
  This function internally reshapes the data in
  \code{object} into a \sQuote{flat} data frame the
  structure of which is described under \sQuote{value}. In
  the default \code{output} mode, \code{glht} from the
  \pkg{multcomp} package is applied to this data frame.
  This causes (general linear) models and, by indicating a
  contrast type, user-defined simultaneous multiple testing
  procedures to be inferred.

  Since either the user or this function itself makes use
  of \code{mcp}, we refer to the \sQuote{Details} section
  of the \code{glht} function. The \code{mcp} function must
  be used with care when defining parameters of interest in
  two-way \acronym{ANOVA} or \acronym{ANCOVA} models. The
  definition of treatment differences might be
  problem-specific. An automated determination of the
  parameters of interest would be impossible and thus only
  comparisons for the main effects (ignoring covariates and
  interactions) would be generated and a warning issued.
}
\examples{
# helper function for plotting with better suitable margins
plot_with_margin <- function(x, mar, ...) {
  old.mar <- par(mar = mar)
  on.exit(par(old.mar)) # tidy up
  plot(x, ...)
}
do.plot <- FALSE # change this to see the plots

## OPMS method

# return the intermediary data frame, do not conduct statistical tests
head(x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
  output = "data"))
stopifnot(is.data.frame(x), dim(x) == c(384, 5))

# watch the converted 'model' argument
(x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
  output = "model"))
stopifnot(inherits(x, "formula")) # left side is set automatically

if (require("multcomp")) {

# watch the converted 'linfct' argument
(x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
  linfct = c(Dunnett = 1), output = "linfct"))
stopifnot(inherits(x, "mcp"))

# create a template contrast matrix
(x <- opm_mcp(vaas_4, model = list("Species", "Strain"),
  linfct = c(Dunnett = 1), output = "contrast"))
stopifnot(is.list(x), sapply(x, inherits, "contrMat"),
  names(x) == "Species") # the selection is as specified by 'linfct'

# comparison using specified model comparing 'Species' pooled over
# complete plates
(x <- opm_mcp(vaas_4, model = list("Species"), m.type = "lm",
  linfct = c(Dunnett = 1))) # refers to 'Species'
stopifnot(inherits(x, "glht"), length(coef(x)) == 1)
if (do.plot)
  plot_with_margin(x, c(3, 20, 3, 2), main = "Species")

# comparison of only A01 - A04 against each other, Tukey style
# note that the left side of the model is set automatically
(x <- opm_mcp(vaas_4[, , 1:4],
  model = ~ Well + Species, m.type = "lm",
  linfct = c(Tukey = 1))) # the number refers to 'Well'
stopifnot(inherits(x, "glht"), length(coef(x)) == 6)
if (do.plot)
  plot_with_margin(x, c(3, 18, 3, 2), main = "Tukey, A01 - A04")

# Dunnett-type comparison of selected wells
(x <- opm_mcp(vaas_4[, , 1:4], model = ~ Well,
  m.type = "lm", linfct = c(Dunnett = 1)))
stopifnot(inherits(x, "glht"), length(coef(x)) == 3)
if (do.plot)
  plot_with_margin(x, c(3, 20, 3, 2), main = "Dunnett, A01 vs. A02 - A04")
# by default 'Dunnett' uses first level as reference

# Dunnett-type comparison with selected control-group
(x <- opm_mcp(vaas_4[, , 1:5], output = "mcp", model = ~ Well,
  linfct = c(`Dunnett.A05 (D-Cellobiose)` = 1)))
if (do.plot)
  plot_with_margin(x, c(3, 20, 3, 2), main = "Dunnett, vs. A05")

# manually defined contrast matrix
(contr <- opm_mcp(vaas_4[, , 1:4], linfct = c(Tukey = 1),
  model = ~ Well, output = "contrast")) # create template, Tukey style
contr <- contr$Well[c(1:3, 6), ] # select comparisons of interest
(x <- opm_mcp(vaas_4[, , 1:4],
  model = ~ Well, m.type = "lm", linfct = contr)) # run tests
if (do.plot)
  plot_with_margin(x, c(3, 20, 3, 2), main = "My own contrasts")

# joining of selected metadata using pseudofunction J
(x <- opm_mcp(vaas_4[, , 1:4], model = ~ J(Well + Species),
  linfct = c(Dunnett = 1), full = FALSE)) # use short well names
if (do.plot)
  plot_with_margin(x, c(3, 22, 3, 2), main = "Dunnett, Well/Species joined")

# comparing wells pairwise regarding the tested species
(x <- opm_mcp(vaas_4[, , 1:4], model = ~ J(Well + Species),
  linfct = c(Pairs.Well = 1), full = FALSE)) # use short well names
if (do.plot)
  plot_with_margin(x, c(3, 22, 3, 2),
    main = "Wells compared between species")
# i.e. 'Pairs.Well' means 'Pairs' type of comparison for each 'Well'
# separately within a joined factor (the first one in 'model', hence
# 'c(Pairs.Well = 1)', with '1' referring to the elements of 'model').

# pairwise comparison of Species regarding the tested strains
xx <- c(vaas_4, vaas_4) # temporary test data
(x <- opm_mcp(xx[, , 1:4], model = ~ J(Strain + Species),
  linfct = c(Pairs.Species = 1), full = FALSE)) # use short well names
if (do.plot)
  plot_with_margin(x, c(3, 22, 3, 2),
    main = "Strains compared within species")
# i.e. 'Pairs.Species' means 'Pairs' type of comparison for each 'Species'
# separately within a joined factor (the first one in 'model', hence
# 'c(Pairs.Species = 1)', with '1' referring to the elements of 'model').

## one could check the number of calculated tests as follows:
#if (nrow(confint(result)$confint) > 20L)
#  warning("number of performed comparisons exceeds 20")

## data-frame method (usually unnecessary to directly apply it)
x <- extract(vaas_4, as.labels = list("Species", "Strain"), subset = "A",
  dataframe = TRUE)

# without the tests, returning the converted data frame
head(y <- opm_mcp(x, output = "data", model = list("Species", "Strain")))
stopifnot(is.data.frame(y), dim(y) == c(384, 5)) # same result as above

# now with conducting the tests
(y <- opm_mcp(x, model = "Species", m.type = "lm",
  linfct = c(Dunnett = 1)))
stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
if (do.plot)
  plot_with_margin(y, c(3, 20, 3, 2), main = "Species (from data frame)")

# testing for subsets of object
(y <- opm_mcp(subset(x, x$Species == "Escherichia coli"),
  linfct = c(Dunnett = 1), model = "Strain", m.type = "lm"))
stopifnot(inherits(y, "glht"), length(coef(y)) == 1)
if (do.plot)
  plot_with_margin(y, c(3, 15, 3, 2), main = "Dunnett (from data frame)")

}
}
\author{
  Lea A.I. Vaas, Markus Goeker
}
\seealso{
  multcomp::glht multcomp::contrMat stats::lm
  stats::formula

  Other multcomp-functions: \code{\link{annotated}}
}
\keyword{htest}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{opm_mcp-methods}
\alias{opm_mcp,MOPMX-method}
\alias{opm_mcp,OPMS-method}
\alias{opm_mcp,data.frame-method}
\usage{
  \S4method{opm_mcp}{MOPMX}(object, model, linfct = 1L,
    m.type = "glm", rhs = 0, alternative = "two.sided", glht.args = list(),
    ops = "+", output = "mcp", sep = opm_opt("comb.value.join"), ...) 
  \S4method{opm_mcp}{OPMS}(object, model, linfct = 1L,
    m.type = "glm", rhs = 0, alternative = "two.sided", glht.args = list(),
    ops = "+", output = "mcp", sep = opm_opt("comb.value.join"), ...) 
  \S4method{opm_mcp}{data.frame}(object, model, linfct = 1L,
    m.type = c("glm", "lm", "aov"), rhs = 0, alternative = "two.sided",
    glht.args = list(), ops = "+",
    output = c("mcp", "data", "model", "linfct", "contrast"),
    sep = opm_opt("comb.value.join"), split.at = param_names("split.at")) 
}
%% END INSERTION BY repair_S4_docu.rb
