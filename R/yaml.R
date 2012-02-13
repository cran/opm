


################################################################################
#
# YAML functions
#


#' YAML_VIA_LIST class
#'
#' This is a virtual class facilitating the conversion to YAML format. It can
#' currently be used by any class that can be coerced to a list, but it is not 
#' directly applied by an \pkg{opm} user.
#'
#' @name YAML_VIA_LIST
#'
#' @docType class
#' @export
#' @family classes
#' @keywords methods
#'
NULL


setClassUnion(YAML_VIA_LIST, c(OPM, OPMS, "list"))


setGeneric("to_yaml", function(object, ...) standardGeneric("to_yaml"))
#' Convert to YAML
#'
#' Convert some R object to YAML. If the package \pkg{yaml} is not installed, a
#' call of this function will result in an error.
#'
#' @param object Object of one of the classes belonging to
#'   \code{\link{YAML_VIA_LIST-class}}.
#' @param sep Logical scalar. Prepend YAML document separator \verb{---}?
#' @param line.sep Character scalar used as output line separator.
#' @param ... Optional other arguments passed to \code{as.yaml} from the
#'   \pkg{yaml} package.
#' @export
#' @return Character scalar (YAML string).
#' @family conversion-functions
#' @keywords character IO
#' @references \url{http://www.yaml.org/}
#' @note Many PM datasets can be batch-converted into YAML format using
#'   \code{\link{batch_opm_to_yaml}}. The output format for the child classes
#'   is described in detail there.
#' @seealso yaml::as.yaml yaml::yaml.load_file
#'
#' @examples \dontrun{
#'
#' # Let 'x' be a any convertible object
#' # Store the data in file 'out.yml' in YAML format.
#' write(to_yaml(x), "out.yml")
#' }
#'
setMethod("to_yaml", YAML_VIA_LIST, function(object, sep = TRUE,
    line.sep = "\n", ...) {
  if (!require("yaml", quietly = TRUE, warn.conflicts = FALSE))
    stop("package 'yaml' is not available")
  result <- yaml::as.yaml(as(object, "list"), line.sep = line.sep, ...)
  if (sep)
    result <- sprintf(sprintf("---%s%%s%s", line.sep, line.sep), result)
  result
}, sealed = SEALED)


