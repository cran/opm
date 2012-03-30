

################################################################################
################################################################################
#
# Package description and package-wide constants
#


#' The opm package
#'
#' Package for analysing OmniLog(R) phenotype microarray data.
#'
#' @name opm.package
#' @docType package
#'
#' @note As a brief guideline for using this manual, including the definition 
#'   of frequently used terms, consider the following:
#' \describe{
#'   \item{families}{All functions and methods are assigned to one or several
#'     families of functions and methods with similar purposes. The respective 
#'     other family members are listed under each \sQuote{See Also} entry.}
#'   \item{classes}{To make sense of this package, one usually has to create
#'     at least one object of one of the classes \code{\link{OPM}},
#'     \code{\link{OPMA}} or \code{\link{OPMS}}. The documentation entry for
#'     each class contains a complete list of its user-level methods.
#'     Conceptually, all these classes store PM data; they differ in whether
#'     they also contain aggregated values (OPMA), and whether they contain 
#'     more than a single plate (OPMS). Note that objects of the class OPMS 
#'     may also contain aggregated values.}
#'   \item{input}{Most \pkg{opm} users will start with data input using either
#'     \code{\link{read_single_opm}} or \code{\link{read_opm}}, which create
#'     the appropriate objects. OmniLog(R) phenotype microarray data are 
#'     structured in \strong{plates}. Each plate has 12 x 8 \strong{well} 
#'     layout, and each well contains the respiration measurements on one
#'     substrate or inhibitor, or combination of substrates or inhibitors.}
#'   \item{undocumented methods}{Some \code{\link{OPMS}} methods with the same
#'     use as the eponymous \code{\link{OPM}} method are only briefly listed
#'     in the documentation of the \code{\link{OPMS}} class. \code{\link{OPMA}}
#'     inherits from \code{\link{OPM}} and has all its methods, even though
#'     they are not listed as \code{\link{OPMA}} methods. \code{\link{OPM}}
#'     itself inherits from \code{\link{WMD}} and has all its methods. Objects
#'     of the \code{\link{OPMS}} class act as containers for \code{\link{OPM}} 
#'     and/or \code{\link{OPMA}} objects, and its methods usually traverse the
#'     contained objects in some manner.}
#'   \item{coercion functions}{Coercion functions for the three classes are only
#'     briefly listed under each class documentation entry. For details, see
#'     the documentation of \code{as} from the \pkg{methods} package.}
#'   \item{scalars}{This documentation uses the term \sQuote{scalar} for
#'     single-element vectors, even though technically these are, well,
#'     vectors.}
#'   \item{YAML}{Input and output of YAML files needs the \pkg{yaml} package.
#'     Because this package is not required for the installation of
#'     \pkg{opm}, warnings or error messages will not occur before any of the
#'     YAML-dependent functions are called. We recommend installing one of
#'     the newer versions of \pkg{yaml} (>= v2.1.3) which are based on libyaml
#'     as parser instead of Syck, are faster and contain some bug fixes. The
#'     YAML-related functions of \pkg{opm} are \code{\link{to_yaml}} and 
#'     \code{\link{batch_opm_to_yaml}}.}
#'   \item{running time}{Computations on such high-dimensional data may take
#'     some time. The limiting steps are aggregating (curve-parameter 
#'     estimation) and plotting many curves together. The former step can be
#'     conducted in parallel if the \pkg{multicore} package is available. It is
#'     not required for the installation of \pkg{opm}. There is also fast
#'     estimation method for the parameters \sQuote{area under the curve} and
#'     \sQuote{maximum height}. See \code{\link{do_aggr}} and the methods it
#'     refers to for details.}
#'   \item{advanced plotting}{The \pkg{gplots} package is also not required for
#'     the installation of \pkg{opm} but can be used to draw more advanced 
#'     heatmaps. See \code{\link{heat_map}} and its accompanying methods for
#'     details.}
#' }
#' @references \url{http://www.biolog.com/}
#' @references Bochner, B. R., Gadzinski, P., Panomitros, E. 2001 Phenotype
#'   MicroArrays for high throughput phenotypic testing and assay of gene 
#'   function. \emph{Genome Research} \strong{11}, 1246--1255.
#' @references Bochner, B. R. 2009 Global phenotypic characterization of
#'   bacteria. \emph{FEMS Microbiological Reviews} \strong{33}, 191--205.
#' @references \url{http://www.dsmz.de/}
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk 
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for  
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE},
#'   in press.
#' @references \url{http://www.yaml.org/}
#~ @export
#' @keywords package
#'
NULL


################################################################################
################################################################################
#
# Programming constants
#


# Error messages
#
NOT_YET <- "not yet implemented"
BUG_MSG <- "a bug -- this should not happen"


# Class names and settings
#
WMD <- "WMD"
OPM <- "OPM"
OPMA <- "OPMA"
OPMS <- "OPMS"
OPMX <- "OPMX"
YAML_VIA_LIST <- "YAML_VIA_LIST"
MOA <- "MOA"
SEALED <- FALSE


################################################################################
################################################################################
#
# Input constants
#


# Names used in CSV data
#
FILE <- "File"
PLATE_TYPE <- "Plate Type"
HOUR <- "Hour"
POS <- "Position"
SETUP <- "Setup Time"
GEN_III <- "Gen III"


# Range of the OmniLog(R) measurements
#
THEOR_MIN <- 0
THEOR_MAX <- 400


################################################################################
################################################################################
#
# Output constants
#


# Curve parameters
# N.B.: The order must be kept in sync with map_grofit_names()
#
LAMBDA <- "lambda"
MU <- "mu"
CURVE_PARAMS <- c(MU, LAMBDA, "A", "AUC")


# Names used in aggregation settings
#
PROGRAM <- "program"
OPTIONS <- "options"
KNOWN_PROGRAMS <- c("grofit", "opm-fast")


################################################################################
################################################################################
#
# Colors
#


# Basic color keywords from http://www.w3.org/TR/css3-color/ (accessed on
# 29-8-2011), sorted darkest-first.
#
W3C_COLORS <- structure(
  .Data = c(
    "#000000",
    "#000080",
    "#008000",
    "#800000",
    "#0000FF",
    "#00FF00",
    "#FF0000",
    "#008080",
    "#800080",
    "#808000",
    "#808080",
    "#00FFFF",
    "#FF00FF",
    "#FFFF00",
    "#C0C0C0",
    "#FFFFFF"
  ), names = c(
    "black",
    "navy",
    "green",
    "maroon",
    "blue",
    "lime",
    "red",
    "teal",
    "purple",
    "olive",
    "gray",
    "aqua",
    "fuchsia",
    "yellow",
    "silver",
    "white"
  )
)


# Names of W3c colors (except white) sorted so as to maximize contrast between
# adjacent colors.
#
W3C_NAMES_MAX_CONTRAST <- c("teal", "purple", "olive", "black", "silver",
  "blue", "lime", "red", "aqua", "fuchsia", "yellow", "navy", "green",
  "maroon", "gray")


# Colors manually selected and sorted by Nora Buddruhs for maximum contrast.
#
NORAS_COLORS <- c("midnightblue", "darkred", "darkgreen", "orange",
  "lightslateblue", "seashell4", "saddlebrown", "firebrick2",
  "palevioletred3", "purple4")


# Shades of pink...
#
ROSEOBACTER_COLORS <- c("maroon1", "palevioletred3", "hotpink1",
  "mediumvioletred", "violetred3", "deeppink3", "lightcoral", "pink1",
  "indianred3", "magenta1")


# Colors from two ColorBrewer palettes, sorted so as to maximize contrast 
# between adjacent colors.
#
BREWER_COLORS <- c(
  "#CAB2D6", "#A6CEE3", "#80B1D3", "#CCEBC5", "#FDB462", "#8DD3C7", 
  "#33A02C", "#B3DE69", "#B15928", "#FF7F00", "#1F78B4", "#B2DF8A", 
  "#6A3D9A", "#E31A1C", "#FFED6F", "#FFFF99", "#FB8072", "#FFFFB3", 
  "#FDBF6F", "#D9D9D9", "#FB9A99", "#FCCDE5", "#BC80BD", "#BEBADA"
)


################################################################################
################################################################################
#
# Plates and substrates
#


PLATE_MAP <- structure(
  .Data = c(
    "Identification",
    "Carbon Sources",
    "Carbon Sources",
    "Nitrogen Sources",
    "Phosphorus and Sulfur Sources",
    "Nutrient Supplements",
    "Peptide Nitrogen Sources",
    "Peptide Nitrogen Sources",
    "Peptide Nitrogen Sources",
    "Osmolytes",
    "pH",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Carbon and Energy Sources",
    "Carbon and Energy Sources / Nitrogen Sources",
    "Carbon and Energy Sources / Nitrogen Sources",
    "Carbon and Energy Sources / Nitrogen Sources",
    "Ions",
    "Hormones & Metabolic Effectors",
    "Hormones & Metabolic Effectors",
    "Hormones & Metabolic Effectors",
    "Anti-Cancer Agents",
    "Anti-Cancer Agents",
    "Anti-Cancer Agents",
    "Anti-Cancer Agents"
  ), names = c(
    "Gen III",
    "PM01",
    "PM02",
    "PM03",
    "PM04",
    "PM05",
    "PM06",
    "PM07",
    "PM08",
    "PM09",
    "PM10",
    "PM11",
    "PM12",
    "PM13",
    "PM14",
    "PM15",
    "PM16",
    "PM17",
    "PM18",
    "PM19",
    "PM20",
    "PM-M01",
    "PM-M02",
    "PM-M03",
    "PM-M04",
    "PM-M05",
    "PM-M06",
    "PM-M07",
    "PM-M08",
    "PM-M11",
    "PM-M12",
    "PM-M13",
    "PM-M14"
  )
)


if (FALSE)
PLATE_MAP <- structure(
  .Data = c(
    "Identification",
    "Carbon Sources",
    "Carbon Sources",
    "Nitrogen Sources",
    "Phosphorus and Sulfur Sources",
    "Nutrient Supplements",
    "Peptide Nitrogen Sources",
    "Peptide Nitrogen Sources",
    "Peptide Nitrogen Sources",
    "Osmolytes",
    "pH",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Chemicals",
    "Carbon and Energy Sources",
    "Carbon and Energy Sources / Nitrogen Sources",
    "Carbon and Energy Sources / Nitrogen Sources",
    "Carbon and Energy Sources / Nitrogen Sources",
    "Ions",
    "Hormones & Metabolic Effectors",
    "Hormones & Metabolic Effectors",
    "Hormones & Metabolic Effectors",
    "Anti-Cancer Agents",
    "Anti-Cancer Agents",
    "Anti-Cancer Agents",
    "Anti-Cancer Agents"
  ), names = c(
    "Gen III",
    "PM01",
    "PM02-A",
    "PM03-B",
    "PM04-A",
    "PM05",
    "PM06",
    "PM07",
    "PM08",
    "PM09",
    "PM10",
    "PM11-C",
    "PM12-B",
    "PM13-B",
    "PM14-A",
    "PM15-B",
    "PM16-A",
    "PM17-A",
    "PM18-C",
    "PM19",
    "PM20-B",
    "PM-M01-A",
    "PM-M02-A",
    "PM-M03-A",
    "PM-M04-A",
    "PM-M05",
    "PM-M06",
    "PM-M07",
    "PM-M08",
    "PM-M11",
    "PM-M12",
    "PM-M13",
    "PM-M14"
  )
)


################################################################################



