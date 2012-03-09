
################################################################################
################################################################################
#
# Descriptions of all datasets included in the package
#


#' Example dataset from Vaas et al. (2012)
#'
#' This \code{\link{OPMS}} object contains all measurements from the study by
#' Vaas et al. (2012). Metadata have been added to fully describe the 
#' conducted PM experiments. The plate type is \sQuote{Generation III}, but the
#' running mode was as for PM plates. Four bacterial strains from two species
#' were considered in the study. For the three publicly accessible ones, the 
#' URLs of their DSMZ catalog entries are given below.
#'
#' @docType data
#' @name vaas_et_al
#' @format \code{\link{OPMS}} object with the dimensions 114 x 384 x 96, i.e.
#'   114 plates with 384 time points and 96 wells per plate. (10 plates have
#'   364, 365, 368 or 371 time points, respectively; the remaining 74 plates 
#'   have 384 time points).
#' @source Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. Visualization 
#'   and curve parameter estimation strategies for efficient exploration of
#'   Phenotype Microarray kinetics. PLoS ONE 2012; in press.
#' @references 
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html} 
#' @references
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html} 
#' @references
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
#' @examples \dontrun{
#'
#' # Calling this yielded a variable vaas_et_al containing the data. The
#' # opm package must be loaded beforehand using library().
#' data(vaas_et_al)
#' }
#'
NULL


################################################################################


#' Example dataset from Vaas et al. (2012), four selected plates
#'
#' This \code{\link{OPMS}} object contains measurements from four selected 
#' plates from the study by Vaas et al. (2012). Metadata have been added to 
#' fully describe the conducted PM experiments: these plates are the 6th 
#' technical replicate from the first biological replicate for the four 
#' bacterial strains considered in the study (see \code{\link{vaas_et_al}}).
#'
#' @docType data
#' @name vaas_4
#' @format \code{\link{OPMS}} object with the dimensions 4 x 384 x 96, i.e.
#'   4 plates with 384 time points and 96 wells per plate.
#' @source Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. Visualization 
#'   and curve parameter estimation strategies for efficient exploration of
#'   Phenotype Microarray kinetics. PLoS ONE 2012; in press.
#' @references 
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html} 
#' @references
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html} 
#' @references
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
#' @examples \dontrun{
#'
#' # Calling this yielded a variable vaas_4 containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(vaas_4)
#' }
#'
NULL


################################################################################


#' Example dataset from Vaas et al. (2012), a single selected plate
#'
#' This \code{\link{OPMA}} object contains measurements from a single selected 
#' plate from the study by Vaas et al. (2012). Metadata have been added to 
#' fully describe the conducted PM experiments: this plate is the 6th technical
#' replicate from the first biological replicate for the strain 
#' \emph{Escherichia coli} DSM30083T (yes, the type strain of \emph{E. coli}).
#'
#' @docType data
#' @name vaas_1
#' @format \code{\link{OPMA}} object with the dimensions 384 x 96, i.e. a single
#'   plate with 384 time points and 96 wells.
#' @source Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. Visualization 
#'   and curve parameter estimation strategies for efficient exploration of
#'   Phenotype Microarray kinetics. PLoS ONE 2012; in press.
#' @references 
#'   \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
#' @examples \dontrun{
#'
#' # Calling this yielded a variable vaas_1 containing the data. The opm
#' # package must be loaded beforehand using library().
#' data(vaas_1)
#' }
#'
NULL


################################################################################


