

################################################################################
################################################################################
#
# Programming constants
#


# Error messages
#
NOT_YET <- "not yet implemented"
BUG_MSG <- "a bug -- this should not happen"


# Class names
#
WMD <- "WMD"
OPM <- "OPM"
OPMA <- "OPMA"
OPMD <- "OPMD"
OPMS <- "OPMS"
OPMX <- "OPMX"
YAML_VIA_LIST <- "YAML_VIA_LIST"
MOA <- "MOA"
CMAT <- "CMAT"


# Used in all S4 method definitions. The idea is to keep it FALSE during
# programming but set it to TRUE when building the package. The line contains a
# special  comment used by pkgutils and should not be changed.
#
SEALED <- TRUE #|| SEALED <- FALSE


################################################################################
################################################################################
#
# Input constants
#


# Names used in CSV data
#
CSV_NAMES <- c(FILE = "File", PLATE_TYPE = "Plate Type", POS = "Position",
  SETUP = "Setup Time")


# Expected name of time points in input data
#
HOUR <- "Hour"


# The known special plates
#
SPECIAL_PLATES <- c("Gen III", "ECO", "SF-N2", "SF-P2")
names(SPECIAL_PLATES) <- c("gen.iii", "eco", "sf.n2", "sf.p2")


# Theoretically expected range of the OmniLog measurements (Bochner, pers.
# comm.)
#
THEOR_RANGE <- c(0, 400)


################################################################################
################################################################################
#
# Output constants
#


# Curve parameters
# N.B.: The order must be kept in sync with map_grofit_names() and with the
# functions that fetch the name of lambda using CURVE_PARAMS[2L]. So this should
# not be changed unless great care is taken.
#
CURVE_PARAMS <- c("mu", "lambda", "A", "AUC")


# Names used in aggregation/discretization settings
#
SOFTWARE <- "software"
VERSION <- "version"
UNKNOWN_VERSION <- "0.0.0"
PROGRAM <- "program" # from the old style, synonym of METHOD in new style
METHOD <- "method"
OPTIONS <- "options"
KNOWN_METHODS <- list(
  aggregation = c("grofit", "opm-fast", "shortcut"),
  discretization = c("direct", "kmeans", "best-cutoff")
)


################################################################################
################################################################################
#
# Colours
#


# Basic colour keywords from http://www.w3.org/TR/css3-color/ (accessed on
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


################################################################################
################################################################################
#
# Storage of precomputed values
#

MEMOIZED <- new.env(parent = emptyenv())


################################################################################
################################################################################
#
# Default opm options
#

OPM_OPTIONS <- new.env(parent = emptyenv())
OPM_OPTIONS$color.borders <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E",
  "#993404")
OPM_OPTIONS$css.file <- ""
OPM_OPTIONS$csv.keys <- unname(CSV_NAMES[c("SETUP", "POS")])
OPM_OPTIONS$csv.selection <- unname(CSV_NAMES[c("SETUP", "POS", "FILE")])
OPM_OPTIONS$gen.iii <- ""
OPM_OPTIONS$html.class <- "section-level-%i"
OPM_OPTIONS$phylo.fmt <- "epf"
OPM_OPTIONS$time.fmt <- c("%m/%d/%Y %I:%M:%S %p", "%b %d %Y %I:%M %p",
  "%d.%m.%Y %H:%M:%S")
OPM_OPTIONS$time.zone <- ""
OPM_OPTIONS$colors <- "w3c"
OPM_OPTIONS$split <- "/.-_"
OPM_OPTIONS$digits <- 4L
OPM_OPTIONS$file.encoding <- ""
OPM_OPTIONS$curve.param <- "A"


################################################################################
################################################################################
#
# Constants related to the phylogeny functions
#


CHARACTER_STATES <- c(0L:9L, LETTERS)[1L:32L]

MISSING_CHAR <- "?"

PHYLO_FORMATS <- c("epf", "nexus", "phylip", "hennig", "html")

# only those Greek letters that are likely to occur in substrate names, and
# deliberately not the uppercase versions
GREEK_LETTERS <- c("alpha", "beta", "gamma", "delta", "epsilon")
names(GREEK_LETTERS) <- substring(GREEK_LETTERS, 1L, 1L)
GREEK_LETTERS <- cbind(plain = GREEK_LETTERS,
  html = sprintf("&%s;", GREEK_LETTERS))


