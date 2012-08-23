
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
#' @keywords internal
#'
source_location <- function() {
  result <- attr(body(match.fun(source_location)), "srcfile")$filename
  if (length(result) == 0L)
    return(result)
  path.expand(normalizePath(result))
}


################################################################################


#' Test file directory
#'
#' Name of directory with test files. Auxiliary function.
#'
#' @param x Location of source file.
#' @param files Character vector. Optional list of of filenames to append
#'   to the directory name.
#' @return Name of the directory in which the input files for testing reside.
#' @keywords internal
#'
test_file_dir <- function(x = source_location(), files = NULL) {
  if (length(x)) {
    x <- sub("[\\/][Rr]$", "", dirname(x), perl = TRUE)
    x <- file.path(x, c("inst", "opm", file.path("inst", "opm"), ""),
      "testdata")
  } else
    x <- dirname(opm_files("testdata"))
  x <- x[file.access(x, 1L) >= 0L]
  if (length(x) == 0L)
    return(x)
  x <- x[1L]
  if (length(files) == 0L)
    return(normalizePath(x))
  if (!all(file.access(x <- file.path(x, files), 4L) >= 0L))
    x <- character()
  normalizePath(x)
}


################################################################################


