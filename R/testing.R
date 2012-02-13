
################################################################################
#
# Testing utilities
#


#~ * Source file name
#~
#~ Name of this source file. Does not work if \code{library} instead of
#~ \code{source} is used. Auxiliary function.
#~
#~ @return Name of file in which this function is defined or \code{NULL}.
#~ @family testing-functions
#~ @keywords IO
#~
source_location <- function() {
  result <- attr(body(source_location), "srcfile")$filename
  if (is.null(result))
    result
  else
    path.expand(result)
}


#~ * Test file directory
#~
#~ Name of directory with test files. Auxiliary function.
#~
#~ @param loc Location of source file.
#~ @return Name of the directory in which the input files for testing reside.
#~ @family testing-functions
#~ @keywords IO
#~
test_file_dir <- function(loc) {
  result <- file.path(sub("/R/.+\\.R$", "", loc), "inst/testdata")
  if (file.exists(result))
    result
  else
    sub("/inst/", "/", result, fixed = TRUE)
}

