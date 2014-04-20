\name{heat_map}
\alias{heat_map}
\title{Heat map}
\arguments{
  \item{object}{Matrix, data frame or \code{\link{OPMS}} or
  \code{\link{MOPMX}} object. The matrix method is mainly
  designed for curve-parameter matrices as created by
  \code{\link{extract}} but can be used with any numeric
  matrix. If a data frame, it must contain at least one
  column with numeric data. Not all \code{\link{MOPMX}}
  objects are suitable for this function; see the remarks
  under \code{\link{extract}}.}

  \item{as.labels}{Character, numeric or logical vector
  indicating the positions of the columns to be joined and
  used as row labels. If \code{NULL} or empty, the row
  names of \code{object} are used. See
  \code{\link{extract}} for details.}

  \item{as.groups}{Character, numeric or logical vector
  indicating the positions of the columns to be joined and
  used as group indicators. If \code{NULL} or empty, groups
  are ignored.}

  \item{sep}{Character scalar determining how to join row
  and group names. See \code{\link{extract}} for details.}

  \item{subset}{Character scalar passed to the
  \code{\link{OPMS}} method of \code{\link{extract}}.}

  \item{extract.args}{Optional list of arguments passed to
  that method.}

  \item{hclustfun}{Determines the clustering method used.
  If a function, used directly. If a character scalar, used
  as the \sQuote{method} argument of \code{hclust}. If a
  list, passed as argument list to \code{hclust}.}

  \item{distfun}{Determines the distance method used. If a
  function, used directly. If a character scalar, used as
  the \sQuote{method} argument of \code{dist}. If a list,
  passed as argument list to \code{dist}.}

  \item{scale}{Character scalar. See \code{heatmap} for
  details. The default was changed to no rescaling because
  the curve parameters estimated from
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data
  have the same scale. If the relative changes per
  substrate are of interest, \sQuote{column} should be
  used.}

  \item{r.groups}{Determines the plotting of a colour bar
  indicating row groups.

  If \code{NULL}, ignored. If a function, applied to the
  row names of \code{object}; should then yield one group
  name for each row name. If a character scalar, the name
  of an attribute of \code{object} that contains the row
  group affiliations (ignored if this is not found).
  Otherwise, coerced to \sQuote{character} mode.

  Finally the groups are converted to a factor and used for
  selecting from \code{r.col}.}

  \item{r.col}{Character vector of colour names used by
  \code{r.groups}.  Ignored if that is \code{NULL}.}

  \item{c.groups}{Determines the plotting of a colour bar
  indicating column groups.

  If \code{NULL}, ignored. If a function, applied to the
  column names of \code{object}; should then yield one
  group name for each column name. If a character scalar,
  the name of an attribute of \code{object} that contains
  the column group affiliations (ignored if this is not
  found). Otherwise, coerced to \sQuote{character} mode.

  Finally the groups are converted to a factor and used for
  selecting from \code{c.col}.}

  \item{c.col}{Character vector of colour names used by
  \code{c.groups}.  Ignored if that is \code{NULL}.}

  \item{magnif}{Numeric vector. Factor(s) used per default
  by \code{cexRow} and \code{cexCol}.}

  \item{cexRow}{Magnification of the row labels.}

  \item{cexCol}{Magnification of the column labels.}

  \item{borders}{Numeric vector. Factor(s) used per default
  by \code{margin} and \code{cexCol}.}

  \item{margins}{Two-element numeric vector determining the
  relative size of the margin (i) at the bottom and (ii) at
  the left.}

  \item{col}{Character vector containing the proper heat
  map colours.}

  \item{asqr}{Logical scalar indicating whether the data
  should be treated with the arcsine-square root
  transformation. This usually only makes sense for
  proportion data and cannot be used in conjunction with
  the \code{log1} argument set to \code{TRUE}. If
  \code{NA}, percentages are assumed.}

  \item{log1}{Logical scalar indicating whether
  \code{log1p} should be used for transforming the data
  prior to plotting.}

  \item{lmap}{Numeric scalar with at least three elements,
  or empty. If empty, ignored. Otherwise used for mapping
  logical values to numeric values. See
  \code{\link{map_values}} for details. Ignored if the data
  are not logical.}

  \item{abbrev}{Character scalar indicating whether row or
  column shall be abbreviated before plotting. Note that
  abbreviation is done by shortening words and ending them
  with a dot, so there is no guarantee that a certain
  maximum length will be obtained.}

  \item{...}{Optional arguments passed to \code{heatmap} or
  \code{heatmap.2}.  Note that some defaults of
  \code{heatmap.2} are overwritten even though this is not
  transparent from the argument list of \code{heat_map}. If
  set explicitly, the default \code{heatmap.2} behaviour is
  restored.  \code{\dots} also represents all arguments
  passed from the \code{\link{OPMS}} or data-frame methods
  to the matrix method.}

  \item{use.fun}{Character scalar. If \kbd{gplots}, it is
  attempted to load the \pkg{gplots} package and use its
  \code{heatmap.2} function (the default). If this fails, a
  warning is issued, and \code{heatmap} from the
  \pkg{stats} package (the default) is called instead.}
}
\value{
  A list as output by \code{heatmap} or \code{heatmap.2}
  with the additional entries \code{rowColMap} or
  \code{colColMap} giving the mapping(s) of group names to
  colours as named character vector(s), if this feature was
  used.
}
\description{
  A wrapper for \code{heatmap} from the \pkg{stats} package
  and \code{heatmap.2} from the \pkg{gplots} package with
  some adaptations likely to be useful for
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data.
  The data-frame and \code{\link{OPMS}} methods extract a
  numeric matrix from a given data frame or
  \code{\link{OPMS}} object and pass the result to the
  matrix method.
}
\examples{
# Matrix method (usually unnecessary, see below)
x <- extract(vaas_4, as.labels = list("Strain"),
  as.groups = list("Species"))
hm <- heat_map(x)
stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))

# 'OPMS' method (more convenient)
hm.2 <- heat_map(vaas_4, as.labels = "Strain", as.groups = "Species")
stopifnot(identical(hm[-3], hm.2[-3]))

# Data-frame method
x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
hm <- heat_map(x, as.labels = "Strain", as.groups = "Species")
stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))
}
\seealso{
  stats::heatmap gplots::heatmap.2

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{level_plot}}, \code{\link{parallelplot}},
  \code{\link{radial_plot}}, \code{\link{summary}},
  \code{\link{xy_plot}}
}
\keyword{hplot}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{heat_map-methods}
\alias{heat_map,MOPMX-method}
\alias{heat_map,OPMS-method}
\alias{heat_map,data.frame-method}
\alias{heat_map,matrix-method}
\usage{
  \S4method{heat_map}{MOPMX}(object, as.labels,
    subset = opm_opt("curve.param"), as.groups = NULL, sep = " ",
    extract.args = list(), ...) 
  \S4method{heat_map}{OPMS}(object, as.labels,
    subset = opm_opt("curve.param"), as.groups = NULL, sep = " ",
    extract.args = list(), ...) 
  \S4method{heat_map}{data.frame}(object, as.labels,
    as.groups = NULL, sep = " ", ...) 
  \S4method{heat_map}{matrix}(object,
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
    col = opm_opt("heatmap.colors"), asqr = FALSE, log1 = FALSE, lmap = 1L:3L,
    abbrev = c("none", "row", "column", "both"),
    ...,
    use.fun = c("gplots", "stats")) 
}
%% END INSERTION BY repair_S4_docu.rb
