#' # Analysing phenotype microarray data: clustering with p-values
#'
#' Using **pvclust** and **opm** to calculate a clustering with branch support
#' values. Assessing the uncertainty of clustering results should be done for
#' all their serious interpretations. This is one approach to assessing
#' uncertainty; see the **pvclust** documentation for details.
#'
#' Author: *Markus Goeker*


library(pvclust)
library(opm)


#' ## Generating the data matrix to be clustered

#' The matrix must be transposed: `pvclust` expects the objects to be clustered
#' in the columns. See `pvclust::pvclust` and `base::t` for details. If you need
#' more space for the clustering, generate shorter labels from the metadata.

x <- t(extract(vaas_4, list("Species", "Strain")))


#' ## Clustering the data

#' The default distance method is based on correlations, which makes not much
#' sense for the usual application of PM data, at least not for *vaas_4*. (It
#' could be preferable to Euclidean distances if the overall reaction levels
#' were different between organisms because of experimental artifacts.) Several
#' other distance measures could be applicable, however, as well as clustering
#' algorithms other than the default average-linkage clustering.

x.pvc <- pvclust(x, method.dist = "euclidean")


#' ## Plotting the clustering:

#' The two kinds of support values are visible on the branches. We see that *E.
#' coli* is comparatively well differentiated from *P. aeruginosa*, but only if
#' standard bootstrapping is considered. Calling `pvclust::pvrect` highlights
#' clusters with high support.

plot(x.pvc, hang = -1)
pvrect(x.pvc, pv = "bp")
#' For real analyses, omit `pv = "bp"`.


detach("package:pvclust", unload = TRUE)


