
################################################################################
################################################################################
#
# Descriptions of all data sets included in the package
#


#' Example data set from Vaas et al. (2012), four selected plates
#'
#' This \code{\link{OPMS}} object contains measurements from four selected
#' plates from the study by Vaas et al. (2012). Metadata have been added to
#' fully describe the conducted PM experiments: these plates are the 6th
#' technical replicate from the first biological replicate for the four
#' bacterial strains considered in the study. The complete data set is available
#' as \code{vaas_et_al} in the \pkg{opmdata} package.
#'
#' @docType data
#' @keywords datasets
#' @name vaas_4
#' @format \code{\link{OPMS}} object with the dimensions 4 x 384 x 96, i.e. 4
#'   plates with 384 time points and 96 wells per plate.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#' @references Selezska, K., Kazmierczak, M., Muesken, M., Garbe, J., Schobert,
#'   M., Haeussler, S., Wiehlmann, L., Rohde, C., Sikorski, J. 2012
#'   \emph{Pseudomonas aeruginosa} population structure revisited under
#'   environmental focus: impact of water quality and phage pressure.
#'   \emph{Environmental Microbiology} \strong{14}, 1952--1967.
#' @references \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html}
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html}
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
#' @examples \dontrun{
#'
#' # Calling this yielded a variable vaas_4 containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(vaas_4)
#' }
#'
NULL


################################################################################


#' Example data set from Vaas et al. (2012), a single selected plate
#'
#' This \code{\link{OPMA}} object contains measurements from a single selected
#' plate from the study by Vaas et al. (2012). Metadata have been added to fully
#' describe the conducted PM experiments: this plate is the 6th technical
#' replicate from the first biological replicate for the strain
#' \emph{Escherichia coli} DSM30083T (yes, the type strain of \emph{E. coli}).
#' This is a a subset of \code{\link{vaas_4}}. The complete data set is
#' available as \code{vaas_et_al} in the \pkg{opmdata} package.
#'
#' @docType data
#' @keywords datasets
#' @name vaas_1
#' @format \code{\link{OPMD}} object with the dimensions 384 x 96, i.e. a single
#'   plate with 384 time points and 96 wells.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#' @references
#' \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
#' @examples \dontrun{
#'
#' # Calling this yielded a variable vaas_1 containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(vaas_1)
#' }
#'
NULL


################################################################################


