\name{heat_map,matrix}
\alias{heat_map,matrix}
\title{Heatmap (matrix version)}
\arguments{
  \item{object}{Matrix. This function is mainly designed
  for curve parameter matrices as created by
  \code{\link{extract}} but can be used with any numeric
  matrix.}

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
  the curve parameters estimated from OmniLog(R) data have
  the same scale. If the relative changes per substrate are
  of interest, \sQuote{column} should be used.}

  \item{r.groups}{Determines the plotting of a color bar
  indicating row groups. If \code{NULL}, ignored. If a
  function, applied to the row names of \code{object};
  should then yield one group name for each row name. If a
  character scalar, the name of an attribute of
  \code{object} that contains the row group affiliations
  (ignored if this is not found). Otherwise, coerced to
  \sQuote{character} mode. Finally the groups are converted
  to a factor and used for selecting from \code{r.col}.}

  \item{r.col}{Character vector of color names used by
  \code{r.groups}.  Ignored if that is \code{NULL}.}

  \item{c.groups}{Determines the plotting of a color bar
  indicating column groups. If \code{NULL}, ignored. If a
  function, applied to the column names of \code{object};
  should then yield one group name for each column name. If
  a character scalar, the name of an attribute of
  \code{object} that contains the column group affiliations
  (ignored if this is not found). Otherwise, coerced to
  \sQuote{character} mode. Finally the groups are converted
  to a factor and used for selecting from \code{c.col}.}

  \item{c.col}{Character vector of color names used by
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

  \item{col}{Character vector containing the proper heatmap
  colors.}

  \item{...}{Optional arguments passed to \code{heatmap} or
  \code{heatmap.2}.  Note that some defaults of
  \code{heatmap.2} are overwritten even though this is not
  transparent from the argument list of \code{heat_map}. If
  set explicitely, the default \code{heatmap.2} behaviour
  is restored.}

  \item{use.fun}{Character scalar. If \sQuote{gplots}, it
  is attempted to load the \pkg{gplots} package and use its
  \code{heatmap.2} function. If this fails, a warning is
  issued, and \code{heatmap} from the the \pkg{gplots}
  package (the default) is called instead.}
}
\value{
  A list as output by \code{heatmap} or \code{heatmap.2}
  with the additional entries \sQuote{rowColMap} or
  \sQuote{colColMap} giving the mapping(s) of group names
  to colours as named character vector(s), if this feature
  was used.
}
\description{
  A wrapper for \code{heatmap} from the \pkg{stats} package
  and \code{heatmap.2} from the \pkg{gplots} package with
  some adaptations likely to be useful for OmniLog(R) data.
}
\examples{
data("vaas_4")
x <- extract(vaas_4, as.labels = list("Strain"),
  as.groups = list("Species"))
hm <- heat_map(x)
stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))
# Compare this with the dataframe example
}
\seealso{
  heatmap gplots::heatmap.2

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{ci_plot,OPMS}}, \code{\link{heat_map}},
  \code{\link{level_plot}}, \code{\link{level_plot,OPMS}},
  \code{\link{max_rgb_contrast}},
  \code{\link{select_colors}}, \code{\link{xy_plot}},
  \code{\link{xy_plot,OPMS}}
}
\keyword{hplot}

\docType{methods}
\alias{heat_map,matrix-method}
\usage{
  \S4method{heat_map}{matrix}(object, 
    hclustfun = "ward", distfun = "euclidean", scale = "none",
    r.groups = "row.groups", r.col = select_colors(),
    c.groups = "col.groups", c.col = select_colors(),                                     
    magnif = 4, cexRow = magnif[1L] / sqrt(nrow(object)), 
    cexCol = magnif[length(magnif)] / sqrt(ncol(object)),
    borders = c(0.55, 0.75),
    margins = c(borders[1L] * cexCol * max(nchar(colnames(object))), 
      borders[length(borders)] * cexRow * max(nchar(rownames(object)))),
    col = topo.colors(120L),
    ...,
    use.fun = c("stats", "gplots")) 
}
