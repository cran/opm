#' # Comparison of organisms: Does phenotypic similarity match phylogenetic
#' # similarity?
#'
#' Assume you have run Phenotype Microarray experiments for several organisms,
#' e.g. bacterial strains. Assume further that you have numerous metadata for
#' these strains, for example their genetic similarity and their geographical
#' and ecological origins.
#'
#' The data set `wittmann_et_al` contains bacterial strains from different
#' phylogenetic clusters (see the respective publication). For each of the
#' strains the geographic and ecological origin is known.
#'
#' ### In this example we compare phenotypic to phylogenetic similarity using
#' #### * graphical approaches such as heat maps
#' #### * bootstrapping to assess significance of phenotypic clusters
#' #### * multiple comparison of overall `AUC` values across phylogenetic clades
#'
#'
#' Author: *Johannes Sikorski*
#'
#'
#' ### Load R packages and data

library(opm)
library(opmdata)
library(pvclust)
data(wittmann_et_al)

#' For demonstration purposes, some plates are removed from the data set

wittmann_small <- subset(wittmann_et_al,
  query = list(MLSTcluster = c("Ax1", "Ax2", "Ax4", "Ax6")))

#' Check the dimensions of the data set:

dim(wittmann_small)

#' ### Display phenotypic similarity of strains using a heat map approach
#' * The row dendrogram will be coloured by the metadata information on
#'   phylogenetic clusters "Ax1", "Ax2", "Ax4" and "Ax6"
#' * The curve parameter "Area under the Curve" (`AUC`) will be used
#'
#+  fig.width = 15, fig.height = 8

heat_map(wittmann_small,
  as.labels = list("strain", "replicate", "MLSTcluster"),
  as.groups = "MLSTcluster",
  cexRow = 1.5,
  use.fun = "gplots",
  main = "Heatmap on AUC data",
  subset = "AUC",
  xlab = "Well substrates on Generation-III Biolog plate",
  ylab = "strains, replicates, and their MLST cluster affiliation")

#' ### Result:
#' * Obviously the four phylogenetic clades "Ax1", "Ax2", "Ax4" and "Ax6",
#'   indicated by the four different colours on the row dendrogram, fit to
#'   the phenotypic similarity clustering
#' * Only one strain from clade "Ax2" (strain `CCUG` 47074, second replicate)
#'   falls into the phenotypic similarity cluster of clade "Ax4"
#' * Similarly, a clade "Ax4" strain clusters with "Ax2" strains with respect
#'   to the phenotype
#'
#' ### Are these phenotypic similarity clusters statistically robust?
#' * We use the R package **pvclust** to test this (run
#' `demo("cluster-with-pvalues")` for details).

x <- t(extract(wittmann_small, list("strain", "replicate", "MLSTcluster")))
x.pvc <- pvclust(x, method.dist = "euclidean", method.hclust = "ward",
  nboot = 100)

#+  fig.width = 15, fig.height = 7

plot(x.pvc, hang = -1)
pvrect(x.pvc, max.only = FALSE)

#' ### Result:
#' According to the `AU` p-values there is significant support for some of the
#' observed phenotypic similarity clusters (highlighted with rectangles).
#'
#' ### Is there any significant difference in overall `AUC` values across
#' ### strains of the phylogenetic clades?
#' * we apply the `multcomp` algorithm for multiple comparisons of groups using
#'   a Tukey-type contrast matrix
#' * we compare `AUC` values across the phylogenetic clades which are identified
#'   by the metadata entry `MLSTcluster`

test <- opm_mcp(wittmann_small, model = ~ MLSTcluster, m.type = "aov",
  linfct = c(Tukey = 1))

#+  fig.width = 10, fig.height = 5
old.mar <- par(mar = c(3, 15, 3, 2)) # adapt margins in the plot
plot(test)
par(old.mar) # reset to default plotting settings

#' The numerical output of the statistical test is called as follows:
mcp.summary <- summary(test)
mcp.summary$model$call <- NULL # avoid some unnecessary output
mcp.summary

#' ### Result:
#' * There is no statistically significant difference across all `AUC` values
#'   between the strains of the different phylogenetic clades "Ax1", "Ax2",
#'   "Ax4" and "Ax6".
#' * Though there is no statistical support, the graphical analysis using the
#'   heat map approach suggest a correlation between phylogenetic similarity and
#'   phenotypic similarity.
#'
#' # Synopsis
#' * A graphical approach such as a heat map can be used to test for differences
#'   between a certain phenotypic similarity and s similarity regarding any
#'   other set of traits, as long as these are coded in the metadata.
#' * The statistical robustness can be tested using two different bootstrap
#'   procedures.
#' * The overall difference between multiple groups (as coded in the metadata)
#'   can be tested statistically.


