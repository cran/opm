

################################################################################
################################################################################
#
# Testing utilities
#


#' Source file name
#'
#' Name of this source file. Does not work if \code{library} instead of
#' \code{source} is used.
#'
#' @return Name of the file in which this function is defined or an empty
#'   character vector.
#' @keywords internal
#'
source_location <- function() {
  result <- attr(body(match.fun(source_location)), "srcfile")$filename
  if (length(result))
    normalizePath(result)
  else
    character()
}


################################################################################


#' Test file directory
#'
#' Name of directory with test files. The purpose of this function is to
#' guarantee to always find these files irrespective of whether the package was
#' loaded using \code{library()} (e.g., during \acronym{CRAN} checking) or using
#' \code{source()} (optionally used during development).
#'
#' @param files Character vector. Optional list of of filenames to append to the
#'   directory name. They would be removed again unless all of them did in fact
#'   exist.
#' @return Name of the directory in which the input files for testing reside.
#' @keywords internal
#'
testfile_dir <- function(files = NULL) {
  append_subdirs <- function(x) {
    # original location as well as after installation
    file.path(x, c(file.path("inst", "testdata"), "testdata"))
  }
  from_source_location <- function() {
    # if source_location() works, the result ends in '/opm/R/testing.R'
    append_subdirs(dirname(dirname(source_location())))
  }
  from_working_dir <- function(x) {
    # getwd() called from the tests should return path ending in 'opm/tests'
    append_subdirs(dirname(getwd()))
  }
  from_opmfiles_function <- function() {
    # should return the files installed with the package IF it is installed
    unique(dirname(opm_files("testdata")))
  }
  x <- c(from_source_location(), from_working_dir(), from_opmfiles_function())
  x <- x[file.access(x, 1L) >= 0L]
  if (!length(x))
    return(x)
  x <- x[1L]
  if (length(files))
    if (!all(file.access(x <- file.path(x, files), 4L) >= 0L))
      x <- character()
  normalizePath(x)
}


################################################################################


#' Test objects
#'
#' Create a standardized collection of objects used in the unit tests.
#'
#' @return Named list.
#' @keywords internal
#'
objects_for_testing <- function() {
  x <- list()
  x$TEST.DIR <- testfile_dir()
  x$INFILES <- testfile_dir(sprintf("Example_%i.csv.xz", 1L:3L))
  x$FILE.OLD.STYLE <- testfile_dir("Example_Old_Style_1.csv.xz")
  x$FILE.ID.RUN <- testfile_dir("Example_ID_run.csv.xz")
  x$OPM.1 <- read_single_opm(x$INFILES[1L])
  x$OPM.2 <- read_single_opm(x$INFILES[2L])
  x$OPM.3 <- read_single_opm(x$FILE.OLD.STYLE)
  x$ORGN <- "Bacillus simplex"
  x$MD <- data.frame(File = filename(x$OPM.1), Position = position(x$OPM.1),
    `Setup Time` = setup_time(x$OPM.1), Organism = x$ORGN,
    check.names = FALSE, stringsAsFactors = FALSE)
  x$OPM.WITH.MD <- include_metadata(x$OPM.1, x$MD, remove.csv.data = FALSE)
  x$OPMS.INPUT <- opms(x$OPM.1, x$OPM.2)
  x$SMALL.WITH.MD <- x$OPM.WITH.MD[, 1L:10L]
  metadata(x$OPMS.INPUT) <- data.frame(run = 4L:3L, organism = x$ORGN,
    stringsAsFactors = FALSE)
  x$THIN.AGG <- do_aggr(thin_out(x$OPMS.INPUT, 10), boot = 2L, verbose = FALSE)
  x$SMALL <- x$OPM.1[, 1L:10L]
  x$SMALL.AGG <- do_aggr(x$SMALL, boot = 0L, cores = 1L)
  x
}


################################################################################

