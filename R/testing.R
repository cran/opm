
################################################################################
################################################################################
#
# Testing utilities
#


#' Source file name
#'
#' Name of this source file. Does not work if \code{library} instead of
#' \code{source} is used. Auxiliary function.
#'
#' @return Name of file in which this function is defined or \code{NULL}.
#' @family testing-functions
#' @keywords internal
#'
source_location <- function() {
  result <- attr(body(source_location), "srcfile")$filename
  if (is.null(result))
    result
  else
    path.expand(normalizePath(result))
}


################################################################################


#' Test file directory
#'
#' Name of directory with test files. Auxiliary function.
#'
#' @param loc Location of source file.
#' @return Name of the directory in which the input files for testing reside.
#' @family testing-functions
#' @keywords internal
#'
test_file_dir <- function(loc) {
  dn <- sub("/R$", "", dirname(loc), perl = TRUE)
  if (file.exists(result <- file.path(dn, "inst", "testdata")))
    result
  else if (file.exists(result <- file.path(dn, "opm", "testdata")))
    result
  else if (file.exists(result <- file.path(dn, "opm", "inst", "testdata")))
    result
  else
    file.path(dn, "testdata")
}


################################################################################
