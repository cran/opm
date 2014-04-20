source_location <- function() {
  result <- attr(body(match.fun(source_location)), "srcfile")$filename
  if (length(result))
    normalizePath(result)
  else
    character()
}

testfile_dir <- function(files = NULL) {
  append_subdirs <- function(x) {
    # original location as well as after installation
    file.path(x, c(file.path("inst", "testdata"), "testdata"))
  }
  from_source_location <- function() {
    # if source_location() works, the result ends in '/opm/R/testing.R'
    append_subdirs(dirname(dirname(source_location())))
  }
  from_working_dir <- function() {
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
  x$MD <- data.frame(File = csv_data(x$OPM.1, what = "filename"),
    Position = csv_data(x$OPM.1, what = "position"),
    `Setup Time` = csv_data(x$OPM.1, what = "setup_time"), Organism = x$ORGN,
    check.names = FALSE, stringsAsFactors = FALSE)
  x$OPM.WITH.MD <- include_metadata(x$OPM.1, x$MD, remove.csv.data = FALSE)
  x$OPMS.INPUT <- opms(x$OPM.1, x$OPM.2)
  x$SMALL.WITH.MD <- x$OPM.WITH.MD[, 1L:10L]
  metadata(x$OPMS.INPUT) <- data.frame(run = 4L:3L, organism = x$ORGN,
    stringsAsFactors = FALSE)
  x$THIN.AGG <- do_aggr(thin_out(x$OPMS.INPUT, 10), boot = 2L, verbose = FALSE)
  x$SMALL <- x$OPM.1[, 1L:10L]
  x$SMALL.AGG <- do_aggr(x$SMALL, boot = 0L, cores = 1L)
  x$MOPMX.1 <- as(list(A = x$OPM.3, B = x$OPMS.INPUT), MOPMX)
  x$MOPMX.2 <- as(list(U = c(x$SMALL.AGG, x$SMALL.AGG), V = x$THIN.AGG), MOPMX)
  metadata(x$MOPMX.2[[1L]][1L]) <- list(run = 5, organism = "Unknown")
  metadata(x$MOPMX.2[[1L]][2L]) <- list(organism = "Unknown", run = 8)
  x
}

