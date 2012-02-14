
library(testthat)


context("Testing the IO functions.")


loc <- source_location()
if (is.null(loc))
  loc <- .path.package("opm")
  
TEST.DIR <- test_file_dir(loc)
INFILES <- file.path(TEST.DIR, sprintf("Example_%i.csv.xz", 1L:4L))
OUTDIR <- tempdir()
OUTFILES <- paste(file.path(OUTDIR, sub("\\.csv\\.xz$", "", 
  basename(INFILES))), "txt", sep = ".")


# A silly function for testing batch_process()
#
copy_head <- function(infile, outfile) {
  data <- readLines(infile, n = 5L)
  write(data, outfile)
}


################################################################################
#
# IO helpers
#


test_that("file patterns can be constructed", {
  
  default.pat <- "\\.(csv|ya?ml)(\\.(bz2|gz|lzma|xz))?$"
  expect_equal(default.pat, file_pattern())
  expect_equal("\\.csv$", file_pattern(type = "csv", compressed = FALSE))
  
  expect_equal(NULL, extended_file_pattern(NULL))
  expect_equal(default.pat, extended_file_pattern(list()))
  expect_error(extended_file_pattern("*.csv"))
  expect_equal("*.csv", extended_file_pattern("*.csv", wildcard = FALSE))
  expect_equal("^.*\\.csv$", extended_file_pattern("*.csv", wildcard = TRUE))
  expect_equal("5", extended_file_pattern(5, wildcard = FALSE))
  
})


################################################################################
#
# Input of single OPM files
#


test_that("example file 1 in old style can be read", {
  example.file.old.style.1 <- file.path(TEST.DIR,
    "Example_Old_Style_1.csv.xz")
  opm.2 <- read_single_opm(example.file.old.style.1)
  expect_is(opm.2, "OPM")
  expect_equal(filename(opm.2), example.file.old.style.1)
  expect_equal(plate_type(opm.2), "PM20")
  expect_equal(setup_time(opm.2), "Apr 11 2011 5:08 PM")
  expect_equal(position(opm.2), "12-A")
  expect_equal(hours(opm.2), 91.25)
  expect_equal(dim(opm.2), c(366, 96))
  expect_equal(metadata(opm.2), list())
})


################################################################################
#
# Input of multiple OPM files
#

    
test_that("explode_dir finds the files it should find", {
  files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    wildcard = FALSE)  
  expect_true(all(grepl(TEST.DIR, files)))
  expect_equal(length(files), 8L)
  expect_equal(names(files), NULL)
  files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    exclude = "old", wildcard = FALSE)  
  expect_true(all(grepl(TEST.DIR, files)))
  expect_equal(length(files), 3L)
  expect_equal(names(files), NULL)
})
  

test_that("explode_dir uses list pattern input", {
  csv.files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    wildcard = FALSE)
  old.csv.files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    exclude = "old", wildcard = FALSE)
  files <- explode_dir(TEST.DIR, include = list(type = "csv"))
  expect_equal(files, csv.files)
  files <- explode_dir(TEST.DIR, include = list(type = "csv"),
    exclude = "old", wildcard = FALSE)
  expect_equal(files, old.csv.files)
})
  

test_that("explode_dir uses globbing patterns", {
  csv.files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    wildcard = FALSE)
  old.csv.files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    exclude = "old", wildcard = FALSE)
  files <- explode_dir(TEST.DIR, include = "*.csv.xz", wildcard = TRUE)
  expect_equal(files, csv.files)
  files <- explode_dir(TEST.DIR, include = "*.csv.xz", wildcard = TRUE,
    exclude = "*old*")
  expect_equal(files, old.csv.files)
})
  

test_that("explode_dir uses regex patterns", {
  csv.files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    wildcard = FALSE)
  old.csv.files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    exclude = "old", wildcard = FALSE)
  files <- explode_dir(TEST.DIR, include = ".*\\.csv\\.xz$",
    wildcard = FALSE)
  expect_equal(files, csv.files)
  files <- explode_dir(TEST.DIR, include = ".*\\.csv\\.xz$", 
    wildcard = FALSE, exclude = ".*old.*")
  expect_equal(files, old.csv.files)
})
  

test_that("explode_dir deals with non-existing files", {
  x <- c("0123456789", TEST.DIR)
  expect_error(explode_dir(x))
  expect_warning(explode_dir(x, missing.error = FALSE))
})

    
## UNTESTED: opm_files() read_opm() [see test_opm.R and test_opms.R]
    

################################################################################
#
# Metadata IO
#


test_that("to_metadata converts objects in the right way", {
  x <- data.frame(a = 1:10, b = letters[1:10])
  expect_equivalent(c("integer", "factor"), sapply(x, class))
  x <- as.data.frame(x)
  expect_equivalent(c("integer", "factor"), sapply(x, class))
  x <- to_metadata(x)
  expect_equivalent(c("integer", "factor"), sapply(x, class))
  x <- to_metadata(as.matrix(x))
  expect_equivalent(c("character", "character"), sapply(x, class))
  x <- as.data.frame(as.matrix(x))
  expect_equivalent(c("factor", "factor"), sapply(x, class))
})


################################################################################
#
# Batch-collection functions
#


test_that("batch collection works as expected", {
  files <- INFILES[1L:2L]
  got <- batch_collect(files, readLines, fun.arg = list(n = 5L))
  expect_is(got, "list")
  expect_equal(files, names(got))
  expect_true(all(sapply(got, is.character)))
  expect_true(all(sapply(got, length) == 5L))
  expect_that(got <- batch_collect(files, readLines, fun.arg = list(n = 5L),
    demo = TRUE), shows_message())
  expect_equal(got, files)
  expect_that(got <- batch_collect(TEST.DIR, include = '*.csv.xz',
     readLines, fun.arg = list(n = 5L), demo = TRUE), shows_message())
  expect_equal(got[1L:2L], files)
  expect_is(got <- batch_collect(files, readLines, fun.arg = list(n = 5L),
    exclude = "*.csv.xz", wildcard = TRUE), "list")
  expect_equal(length(got), 0L)
  expect_that(got <- batch_collect(files, readLines, fun.arg = list(n = 5L),
    exclude = "*.csv.xz", wildcard = TRUE, demo = TRUE), shows_message())
  expect_equal(character(), got)
})


test_that("templates can be collected", {
  
  files <- INFILES[1L:3L]
  
  template <- collect_template(files, exclude = "*3*")
  expect_is(template, "data.frame")
  expect_equal(colnames(template), c("Setup Time", "Position", "File"))
  expect_equal(nrow(template), 2L)
  expect_true(all(! is.na(template)))
  expect_true(all("character" == sapply(template, class)))
  
  expect_that(template <- collect_template(files, exclude = "*3*", demo = TRUE),
    shows_message())
  expect_equal(template, files[1L:2L])

})


test_that("templates can be collected and written to files", {
  
  files <- INFILES[1L:3L]
  outfile <- tempfile()
  infile <- tempfile()
  
  expect_false(any(file.exists(c(outfile, infile))))
  
  template <- collect_template(files, exclude = "*3*", outfile = outfile)
  expect_true(file.exists(outfile))
  expect_false(file.exists(infile))
  expect_is(template, "data.frame")
  expect_equal(colnames(template), c("Setup Time", "Position", "File"))
  expect_equal(nrow(template), 2L)
  expect_true(all(! is.na(template)))
  expect_true(all("character" == sapply(template, class)))
  unlink(outfile)
  
  expect_error(template <- collect_template(files, exclude = "*3*", 
    outfile = outfile, previous = infile))
  expect_false(file.exists(outfile))
  expect_false(file.exists(infile))

  unlink(c(infile, outfile))
  
})


test_that("templates can be collected with added columns", {
  
  files <- INFILES[1L:3L]
  to.add <- c("A", "B")
  
  template <- collect_template(files, exclude = "*3*", add.cols = to.add)
  expect_is(template, "data.frame")
  expect_equal(colnames(template), c("Setup Time", "Position", "File", to.add))
  expect_equal(nrow(template), 2L)
  expect_true(all(!is.na(template[, 1L:3L])))
  expect_true(all(is.na(template[, to.add])))
  expect_true(all("character" == sapply(template, class)))

  expect_that(template <- collect_template(files, exclude = "*3*", 
    add.cols = to.add, demo = TRUE), shows_message())
  expect_equal(template, files[1L:2L])

})


################################################################################
#
# Batch conversion functions
#


test_that("batch conversion works in demo mode", {
  
  infiles <- INFILES
  expect_false(any(file.exists(OUTFILES)))
  
  # Demo run not allowing missing input files
  expect_error(got <- batch_process(infiles, out.ext = 'txt',            
    io.fun = copy_head, outdir = OUTDIR, verbose = FALSE, demo = TRUE))
  
  # Demo run
  expect_that(got <- batch_process(infiles, out.ext = 'txt', io.fun = copy_head,
    outdir = OUTDIR, missing.error = FALSE, verbose = TRUE, demo = TRUE),
    shows_message())
  expect_is(got, "matrix")
  expect_equal(got[, 1L], infiles[-4L])
  expect_equal(got[, 2L], OUTFILES[-4L])
  
  expect_false(any(file.exists(OUTFILES)))
  
})  
  
  
test_that("batch conversion works", {
  
  infiles <- INFILES
  expect_false(any(file.exists(OUTFILES)))
  
  # Real run with forced overwriting
  for (i in 1L:2L) {
    expect_warning(got <- batch_process(infiles, out.ext = 'txt',
      io.fun = copy_head, overwrite = "yes", outdir = OUTDIR,
      missing.error = FALSE, verbose = TRUE, demo = FALSE))
    expect_true(all(file.exists(OUTFILES[-4L])))
    expect_false(file.exists(OUTFILES[4L]))
    expect_true(all(file.info(OUTFILES[-4L])$size > 0))
    expect_is(got, "matrix")
    expect_equal(infiles[-4L], got[, "infile"])
    expect_equal(OUTFILES[-4L], got[, "outfile"])
    expect_true(all(got[, "before"] == "attempt to create outfile"))
    expect_true(all(got[, "after"] == "ok"))
  }

  # Real run without forced overwriting
  expect_warning(got <- batch_process(infiles, out.ext = 'txt',
    io.fun = copy_head, overwrite = "no", outdir = OUTDIR,
    missing.error = FALSE, verbose = TRUE, demo = FALSE))
  expect_true(all(file.exists(OUTFILES[-4L])))
  expect_false(file.exists(OUTFILES[4L]))
  expect_true(all(file.info(OUTFILES[-4L])$size > 0))
  expect_is(got, "matrix")
  expect_equal(infiles[-4L], got[, "infile"])
  expect_equal(OUTFILES[-4L], got[, "outfile"])
  expect_true(all(got[, "before"] == "outfile not empty"))
  expect_true(all(got[, "after"] == ""))

  # Real run without forced overwriting
  expect_warning(got <- batch_process(infiles, out.ext = 'txt',
    io.fun = copy_head, overwrite = "older", outdir = OUTDIR,
    missing.error = FALSE, verbose = TRUE, demo = FALSE))
  expect_true(all(file.exists(OUTFILES[-4L])))
  expect_false(file.exists(OUTFILES[4L]))
  expect_true(all(file.info(OUTFILES[-4L])$size > 0))
  expect_is(got, "matrix")
  expect_equal(infiles[-4L], got[, "infile"])
  expect_equal(OUTFILES[-4L], got[, "outfile"])
  expect_true(all(got[, "before"] == "outfile not empty and newer"))
  expect_true(all(got[, "after"] == ""))
  
  # Clean up
  unlink(OUTFILES)
  
})


################################################################################
#
# Batch IO with OPM objects
#


test_that("batch conversion to yaml works", {
  infiles <- INFILES
  expect_warning(got <- batch_opm_to_yaml(infiles, missing.error = FALSE,
    demo = TRUE))
  infiles <- infiles[-4L]
  expect_is(got, "matrix")
  expect_equal(got[, 1L], infiles)
  expect_equal(got[, 2L], sub("\\.csv\\.xz$", ".yml", infiles))
  infiles <- infiles[1L]
  outdir <- tempdir()
  exp.outfile <- file.path(outdir, "Example_1.yml")
  expect_false(file.exists(exp.outfile))
  got <- batch_opm_to_yaml(infiles, outdir = tempdir(), verbose = TRUE)
  expect_true(file.exists(exp.outfile))
  unlink(exp.outfile)
})


################################################################################
#
# Splitting files
#


test_that("files can be split", { # see also the example
  
  tmp <- c(tempfile(), tempfile())
  # Dummy FASTA files
  x <- c(">Ahoernchen", "acataggacaggataggacaattagatacagat", "acggat",
    ">Behoernchen", "agatacaggataggaacca--acaggattattg", "--ccca")
  y <- c(">Taxon_1", "---taggacaggataggacaattagatacagat", "acggat",
    ">Taxon_2", "agatacaggatannnacca--acaggattattg", "--ccca",
    ">Taxon_3", "agatacaggatannnacca--acaggattattg", "--ccca")
  write(x, tmp[1L])
  write(y, tmp[2L])

  expect_that(got <- split_files(tmp, ">*", wildcard = TRUE, demo = TRUE), 
    shows_message())
  expect_is(got, "list")
  expect_equal(names(got), tmp)
  
  got <- split_files(tmp, ">*", wildcard = TRUE)
  expect_is(got, "list")
  expect_equal(names(got), tmp)
  
  got.1 <- got[[1L]]
  expect_equal(length(got.1), 2L)
  expect_true(all(file.exists(got.1)))  
  unlink(got.1)

  got.2 <- got[[2L]]
  expect_equal(length(got.2), 3L)
  expect_true(all(file.exists(got.2)))  
  unlink(got.2)

  unlink(tmp)
  
})


################################################################################
#
# File renaming
#


test_that("file names can be cleaned", {
  x <- c("a b/c/x-y-z.txt", "d--z/z?-z. .csv", "/xxx/y y/?_?-abcd*+.txt", 
    "", "sapif89asdh_&#-*_asdfhu.---", "!$%&+()=?`")
  expect_that(got <- clean_filenames(x, demo = TRUE), shows_message())
  expect_equal(names(got), x[-1L])
  expect_equivalent(got, c("d--z/z-z.csv", "/xxx/y y/abcd.txt",
    "./__EMPTY__00001__", "./sapif89asdh-asdfhu", "./__EMPTY__00002__"))             
})



