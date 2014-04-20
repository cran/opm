\name{parallelplot}
\alias{parallelplot}
\title{Parallel plot}
\arguments{
  \item{x}{An \code{\link{OPMA}} or \code{\link{OPMS}}
  object with aggregated data. This and the following
  argument can swap their places.}

  \item{data}{Any kind of object that can be used for
  selecting \code{\link{metadata}}. Either \code{NULL}, a
  character vector, a list of character vectors or a
  formula indicating which metadata should be included and
  used to determine the shape of the plot. The next
  argument by default accesses the first metadata entry. If
  numeric, \code{panel.var} also accesses the results from
  applying \code{data}.

  Most flexibility is available if \code{data} is a
  formula. For instance, as usual the \code{J}
  pseudo-function can be used to join metadata entries.
  Further, if a left part is present, this can indicate the
  parameters that should be plotted on the Y-axes (in place
  of the \code{pnames} argument; see below for further
  details). As usual, the right part of the formula states
  the meta-information to be included.}

  \item{groups}{Character or numerical scalar determining
  which metadata entry or other information, such as the
  well indexes, (see the examples) is used for assigning
  colours to the curves. If a numeric scalar, it refers to
  the position of the (potentially merged) metadata entry
  within \code{data}. If that argument were empty, a
  numeric \code{groups} argument would be ignored. Empty
  \code{groups} arguments are always ignored; the
  (constant) plate type is then used for creating a
  header.}

  \item{panel.var}{Character or numeric vector indicating
  which metadata entry or other information, such as the
  well indexes, (see the examples) is used for creating
  sub-panels. If a numeric vector, it refers to the
  position of the (potentially merged) metadata entry
  within \code{data}. If that argument were empty, a
  numeric \code{panel.var} argument would be ignored.}

  \item{pnames}{Character vector or formula to select the
  curve parameters for plotting. It has to comprise at
  least two of the names given by
  \code{\link{param_names}()}. If explicitly provided, this
  argument overrules the left side, if any, of a formula
  given as \code{data} argument. (But the left side, if
  any, of such a formula would overrule the default.)}

  \item{col}{Character or numerical scalar or vector. This
  and the following arguments work like the eponymous
  arguments of \code{\link{xy_plot}}. See there for
  details.}

  \item{strip.fmt}{List.}

  \item{striptext.fmt}{List.}

  \item{legend.fmt}{List.}

  \item{legend.sep}{Character scalar.}

  \item{draw.legend}{Logical scalar.}

  \item{space}{Character scalar.}

  \item{...}{Optional arguments passed to
  \code{parallelplot} from the \pkg{lattice} package.}
}
\value{
  An object of class \sQuote{trellis}. See \code{xyplot}
  from the \pkg{lattice} package for details.
}
\description{
  Customised plotting of estimated curve parameter values
  from single or multiple \acronym{PM} plates using
  \code{parallelplot} from the \pkg{lattice} package with
  some adaptations likely to be useful for
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data.
}
\details{
  The main application of this function is to include all
  four estimated curve parameters into a single
  comprehensive overview. This assists in addressing
  questions such as \itemize{

  \item Are there any consistent patterns of individual
  curves that may be explained by specific class
  membership? For instance, which curve parameter best
  reflects the origin of the tested strains?

  \item Are there any patterns of individual curves with
  unexpected deviations? For instance, do differences
  between experimental repetitions occur?  }
}
\examples{
## OPM objects

parallelplot(vaas_1)
parallelplot(vaas_1, data = list("Species", "Strain"))
# ... no effect on selection but on header

# value of 'groups' not found in the data: per default no metadata are used
x <- try(parallelplot(vaas_1, groups = "Species"), silent = TRUE)
stopifnot(inherits(x, "try-error"))
# same problem: metadata selected but 'groups' is not contained
x <- try(parallelplot(vaas_1, data = list("Species", "Strain"),
  groups = "missing"), silent = TRUE)
stopifnot(inherits(x, "try-error"))
# ... thus it is safer to use a positional 'groups' argument

## OPMS objects

# per default metadata are ignored
parallelplot(vaas_4[, , 1:10])
# otherwise selecting metadata is as usual
parallelplot(vaas_4[, , 1:10], data = ~ J(Species, Strain))
parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"))

# value of 'groups' not found in the data: per default no metadata are used
x <- try(parallelplot(vaas_4[, , 1:10], groups = "Species"), silent = TRUE)
stopifnot(inherits(x, "try-error"))
# now 'groups' is all present but not a character scalar
x <- try(parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"),
  groups = c("Strain", "Species")), silent = TRUE)
stopifnot(inherits(x, "try-error"))
# here 'groups' is positional but beyond the last element
x <- try(parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"),
  groups = 3), silent = TRUE)
stopifnot(inherits(x, "try-error"))

# 'groups' and 'panel.var' arguments that work
parallelplot(vaas_4[, , 1:10], data = ~ J(Species, Strain),
  panel.var = "Species", groups = "Strain")
parallelplot(vaas_4[, , 1:10], data = "Species", panel.var = "Species",
  groups = NULL)
parallelplot(vaas_4[, , 1:10], data = list("Species", "Strain"),
  panel.var = "Species")

# use of non-metadata information: here the names of the wells
parallelplot(vaas_4[, , 1:10], data = "Species", panel.var = "Well",
  groups = "Species")

# selection of parameters via 'pnames'
parallelplot(vaas_4[, , 1:10], pnames = ~ A + AUC + mu,
  data = ~ Species + Strain, panel.var = "Species",
  col = c("black", "red"), groups = "Species")
x <- try(parallelplot(vaas_4[, , 1:10], pnames = "A",
  data = ~ Species + Strain, panel.var = "Species",
  col = c("black", "red"), groups = "Species"), silent = TRUE)
stopifnot(inherits(x, "try-error")) # => at least two 'pnames' needed

# selecting the parameters via the left side of a 'data' formula
parallelplot(vaas_4[, , 1:10], data = A + AUC ~ J(Species, Strain))
parallelplot(vaas_4[, , 1:10], data = A + AUC ~ J(Species, Strain),
  groups = "Species")

# 'pnames' explicitly given => left side of formula ignored
parallelplot(vaas_4[, , 1:10], data = A + AUC ~ J(Species, Strain),
  pnames = c("A", "mu", "AUC"), groups = "Species")

# again: at least two 'pnames' needed
x <- try(parallelplot(vaas_4[, , 1:10], data = AUC ~ J(Species, Strain),
  groups = "Species"), silent = TRUE)
stopifnot(inherits(x, "try-error"))
}
\author{
  Lea A.I. Vaas
}
\references{
  Sarkar, D. 2008 \emph{Lattice: Multivariate Data
  Visualization with R.} New York: Springer, 265 p.
}
\seealso{
  lattice::xyplot lattice::parallelplot

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{radial_plot}}, \code{\link{summary}},
  \code{\link{xy_plot}}
}
\keyword{hplot}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{parallelplot-methods}
\alias{parallelplot,NULL,OPMX-method}
\alias{parallelplot,OPMX,ANY-method}
\alias{parallelplot,OPMX-method}
\alias{parallelplot,OPMX,missing-method}
\alias{parallelplot,formula,OPMX-method}
\alias{parallelplot,missing,OPMX-method}
\alias{parallelplot,vector,OPMX-method}
\usage{
  \S4method{parallelplot}{NULL,OPMX}(x, data, ...) 
  \S4method{parallelplot}{OPMX,ANY}(x, data, groups = 1L,
  panel.var = NULL, pnames = param_names(), col = opm_opt("colors"),
  strip.fmt = list(), striptext.fmt = list(), legend.fmt = list(),
  legend.sep = " ", draw.legend = TRUE, space = "top", ...) 
  \S4method{parallelplot}{OPMX,missing}(x, data, ...) 
  \S4method{parallelplot}{formula,OPMX}(x, data, ...) 
  \S4method{parallelplot}{missing,OPMX}(x, data, ...) 
  \S4method{parallelplot}{vector,OPMX}(x, data, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
