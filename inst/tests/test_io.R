

library(testthat)
context("Testing the IO functions of the opm package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())

# This file does not exist, hence testfile_dir() would not return it
INFILES.2 <- c(INFILES, sub("Example_3.csv.xz", "Example_4.csv.xz", INFILES[3L],
  fixed = TRUE))

OUTDIR <- tempdir()
OUTFILES <- paste(file.path(OUTDIR, sub("\\.csv\\.xz$", "",
  basename(INFILES.2))), "txt", sep = ".")


################################################################################


# A silly function for testing batch_process() etc.
#
copy_head <- function(infile, outfile) {
  data <- readLines(infile, n = 5L)
  write(data, outfile)
}


# Tests whether two character vectors refer to the same file path (are
# identical once normalized); standardize the path separators (to UNIX style).
#
expect_path_equivalent <- function(actual, expected) {
  norm_path <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  expect_equal(norm_path(actual), norm_path(expected))
}


# Tests whether two character vectors are identical except for differences
# in the path separators (Windows vs. UNIX style).
#
expect_path_equal <- function(actual, expected) {
  clean <- function(x) chartr("\\", '/', x)
  expect_equal(clean(actual), clean(expected))
}


################################################################################
#
# IO helpers
#


## glob_to_regex
test_that("wildcards can be converted to regular expressions", {
  # from http://docstore.mik.ua/orelly/perl/cookbook/ch06_10.htm
  # with some adaptations and
  x <- c("list.?", "project.*", "*old", "type*.[ch]", "*.*", "*")
  wanted <- c("^list\\..$", "^project\\.", "^.*old$", "^type.*\\.\\[ch]$",
    "^.*\\.", "^")
  got <- glob_to_regex(x)
  expect_equal(wanted, got)
  x <- c("^anc-+k", "+us$hs+")
  got <- glob_to_regex(x)
  expect_equal(c("^\\^anc-\\+k$", "^\\+us\\$hs\\+$"), got)
})


## file_pattern
test_that("file patterns can be constructed", {
  default.pat <- "\\.(csv|ya?ml|json)(\\.(bz2|gz|lzma|xz))?$"
  expect_equal(default.pat, file_pattern())
  expect_equal("\\.csv$", file_pattern(type = "csv", compressed = FALSE))
  expect_equal("\\.(ya?ml|json)$", file_pattern(type = "yorj",
    compressed = FALSE))
})


################################################################################
#
# Input of single OPM files
#


## repair_oth
## UNTESTED

## read_old_opm
## UNTESTED

## read_new_opm
## UNTESTED

## read_microstation_opm
## UNTESTED

## read_opm_yaml
## UNTESTED

## FILE_NOT_CSV
## UNTESTED

## read_single_opm
test_that("the example file in old style can be read", {
  x <- read_single_opm(FILE.OLD.STYLE)
  expect_is(x, "OPM")
  expect_equal(csv_data(x, what = "filename"), FILE.OLD.STYLE)
  expect_equal(plate_type(x), "PM20")
  expect_equal(csv_data(x, what = "setup_time"), "Apr 11 2011 5:08 PM")
  expect_equal(csv_data(x, what = "position"), "12-A")
  expect_equal(hours(x), 91.25)
  expect_equal(dim(x), c(366, 96))
  expect_equal(metadata(x), list())
})

## read_single_opm
test_that("the ID-run example file can be read", {
  x <- read_single_opm(FILE.ID.RUN)
  expect_is(x, "OPM")
  expect_equal(csv_data(x, what = "filename"), FILE.ID.RUN)
  expect_equal(plate_type(x), SPECIAL_PLATES[["gen.iii"]])
  expect_equal(csv_data(x, what = "setup_time"), "2/28/2012 9:59:53 AM")
  expect_equal(csv_data(x, what = "position"), " 9-B")
  expect_equal(dim(x), c(1, 96))
  expect_equal(metadata(x), list())
})


################################################################################
#
# Input of multiple OPM files
#


## read_opm
test_that("the ECO-run example file can be read", {
  example.file.eco <- file.path(TEST.DIR, "Example_Ecoplate.csv.xz")
  x <- read_opm(example.file.eco)
  expect_is(x, "OPMS")
  expect_equal(unique(csv_data(x, what = "filename")), example.file.eco)
  expect_equal(plate_type(x), SPECIAL_PLATES[["eco"]])
  expect_equal(dim(x), c(11, 1, 96))
  md.len <- unique(vapply(metadata(x), length, integer(1L)))
  expect_equal(md.len, 106L)
})


## read_opm
test_that("read_opm can read a single file", {
  files <- INFILES.2[1L]
  opm.1 <- read_opm(files)
  expect_is(opm.1, "OPM")
  opm.1 <- read_opm(files, convert = "try")
  expect_is(opm.1, "OPM")
  opm.1 <- read_opm(files, convert = "yes")
  expect_is(opm.1, "OPM")
  opm.1 <- read_opm(files, convert = "no")
  expect_is(opm.1, "list")
  expect_equal(1L, length(opm.1))
})

## read_opm
test_that("read_opm can read two compatible files", {
  files <- INFILES.2[1L:2L]
  opm.1 <- read_opm(files)
  expect_is(opm.1, "OPMS")
  opm.1 <- read_opm(files, convert = "try")
  expect_is(opm.1, "OPMS")
  expect_equal(NULL, names(plates(opm.1)))
  opm.1 <- read_opm(files, convert = "yes")
  expect_is(opm.1, "OPMS")
  expect_equal(NULL, names(plates(opm.1)))
  opm.1 <- read_opm(files, convert = "no")
  expect_is(opm.1, "MOPMX")
  expect_equal(2L, length(opm.1))
})

## read_opm
test_that("read_opm can read three partially incompatible files", {

  files <- testfile_dir(files = c("Example_1.csv.xz", "Example_2.csv.xz",
    "Example_Old_Style_1.csv.xz"))

  expect_warning(opm.1 <- read_opm(files))
  expect_is(opm.1, "MOPMX")
  expect_equal(3L, length(opm.1))

  expect_warning(opm.1 <- read_opm(files, convert = "try"))
  expect_is(opm.1, "MOPMX")
  expect_equal(3L, length(opm.1))

  expect_error(opm.1 <- read_opm(files, convert = "yes"))

  opm.1 <- read_opm(files, convert = "no")
  expect_is(opm.1, "MOPMX")
  expect_equal(3L, length(opm.1))

  opm.1 <- read_opm(files, convert = "sep")
  expect_is(opm.1, "list")
  expect_equal(2L, length(opm.1))
  expect_true(all(vapply(opm.1, is, NA, "MOPMX")))
  expect_true(all(vapply(opm.1[[1]], is, NA, OPM)))
  expect_true(all(vapply(opm.1[[2]], is, NA, OPM)))

  opm.1 <- read_opm(files, convert = "grp")
  expect_is(opm.1, "MOPMX")
  expect_equal(2L, length(opm.1))
  expect_is(opm.1[[1L]], OPMS)
  expect_is(opm.1[[2L]], OPM)

})


## explode_dir
test_that("explode_dir finds the files it should find", {
  files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    wildcard = FALSE)
  expect_true(all(grepl(TEST.DIR, files, fixed = TRUE)))
  expect_equal(length(files), 9L)
  expect_equal(names(files), NULL)
  files <- explode_dir(TEST.DIR, include = file_pattern(type = "csv"),
    exclude = "old", wildcard = FALSE)
  expect_true(all(grepl(TEST.DIR, files, fixed = TRUE)))
  expect_equal(length(files), 5L)
  expect_equal(names(files), NULL)
})

## explode_dir
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

## explode_dir
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

## explode_dir
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

## explode_dir
test_that("explode_dir deals with non-existing files", {
  x <- c("0123456789", TEST.DIR)
  expect_error(explode_dir(x))
  expect_warning(explode_dir(x, missing.error = FALSE))
})


################################################################################
#
# Metadata IO
#


## finish_template
## UNTESTED


## to_metadata
test_that("to_metadata converts objects in the right way", {
  x <- data.frame(a = 1:10, b = letters[1:10])
  expect_equivalent(c("integer", "factor"), vapply(x, class, ""))
  x <- as.data.frame(x)
  expect_equivalent(c("integer", "factor"), vapply(x, class, ""))
  x <- to_metadata(x)
  expect_equivalent(c("integer", "factor"), vapply(x, class, ""))
  x <- to_metadata(as.matrix(x))
  expect_equivalent(c("character", "character"), vapply(x, class, ""))
  x <- as.data.frame(as.matrix(x))
  expect_equivalent(c("factor", "factor"), vapply(x, class, ""))
})

## to_metadata
test_that("to_metadata converts OPMS objects in the right way", {
  # 1
  got <- to_metadata(OPMS.INPUT)
  expect_is(got, "data.frame")
  expect_equal(nrow(got), length(OPMS.INPUT))
  expect_true(setequal(vapply(got, class, ""), c("character", "integer")))
  got <- to_metadata(OPMS.INPUT, stringsAsFactors = TRUE)
  expect_true(setequal(vapply(got, class, ""), c("factor", "integer")))
  # 2 (nested metadata)
  x <- OPM.1
  metadata(x) <- list(A = 1:3, B = 7L, C = list('c1', 1:3))
  y <- OPM.1
  metadata(y) <- list(A = 1:3, 11, B = -1L, D = "?")
  x <- c(x, y)
  rm(y)
  expect_warning(got <- to_metadata(x))
  expect_equal(nrow(got), length(x))
  expect_true(setequal(names(got), LETTERS[1:4]))
  expect_true(setequal(vapply(got, class, ""),
    c("list", "integer", "character")))
})

## to_metadata
test_that("to_metadata converts MOPMX objects in the right way", {
  expect_warning(got <- to_metadata(MOPMX.1))
  expect_is(got, "data.frame")
  expect_equal(nrow(got), sum(vapply(MOPMX.1, length, 0L)))
  expect_false(all(complete.cases(got)))
  expect_equal(ncol(got), 2L)
  expect_true(all(got[-1L, ] == to_metadata(MOPMX.1[2L])))

  metadata(MOPMX.1[[1]]) <- list(run = 17)
  got <- to_metadata(MOPMX.1)
  expect_is(got, "data.frame")
  expect_equal(nrow(got), sum(vapply(MOPMX.1, length, 0L)))
  expect_false(all(complete.cases(got)))
  expect_equal(ncol(got), 2L)
  expect_true(all(got[-1L, ] == to_metadata(MOPMX.1[2L])))

  metadata(MOPMX.1[[1]]) <- list(organism = 'Unknown', run = 17)
  got <- to_metadata(MOPMX.1)
  expect_true(all(complete.cases(got)))

  expect_equivalent(to_metadata(MOPMX.1[2L]), to_metadata(MOPMX.1[[2L]]))
  # i.e. differences in the row names are possible
})


################################################################################
#
# Batch-collection functions
#

## batch_collect
test_that("batch collection works as expected", {
  files <- INFILES.2[1L:2L]
  got <- batch_collect(files, readLines, fun.arg = list(n = 5L))
  expect_is(got, "list")
  expect_equal(files, names(got))
  expect_true(all(vapply(got, is.character, NA)))
  expect_true(all(vapply(got, length, 0L) == 5L))
  expect_that(got <- batch_collect(files, readLines, fun.arg = list(n = 5L),
    demo = TRUE), shows_message())
  expect_path_equal(got, files)
  expect_that(got <- batch_collect(TEST.DIR, include = '*.csv.xz',
    readLines, fun.arg = list(n = 5L), demo = TRUE), shows_message())
  expect_path_equal(got[1L:2L], files)
  expect_is(got <- batch_collect(files, readLines, fun.arg = list(n = 5L),
    exclude = "*.csv.xz", wildcard = TRUE), "list")
  expect_equal(length(got), 0L)
  expect_that(got <- batch_collect(files, readLines, fun.arg = list(n = 5L),
    exclude = "*.csv.xz", wildcard = TRUE, demo = TRUE), shows_message())
  expect_equal(character(), got)
})

## collect_template
test_that("templates can be collected", {

  files <- INFILES.2[1L:3L]

  expect_equal(length(files), 3L)
  expect_true(all(file.exists(files)))

  # if 'exclude' was not specific enough, 'template' would be NULL
  template <- collect_template(files, exclude = "*Example*_3.*")
  expect_is(template, "data.frame")
  expect_equal(colnames(template), c("Setup Time", "Position", "File"))
  expect_equal(nrow(template), 2L)
  expect_true(all(!is.na(template)))
  expect_true(all(vapply(template, inherits, logical(1L), "character")))

  expect_that(template <- collect_template(files, exclude = "*Example*_3.*",
    demo = TRUE), shows_message())
  # if 'exclude' was not specific enough, 'template' would be empty
  expect_equal(template, files[1L:2L])

})

## collect_template
test_that("templates can be collected and written to files", {

  files <- INFILES.2[1L:3L]
  outfile <- tempfile()
  infile <- tempfile()

  expect_equal(length(files), 3L)
  expect_true(all(file.exists(files)))
  expect_false(any(file.exists(c(outfile, infile))))

  # if 'exclude' was not specific enough, 'template' would be NULL
  template <- collect_template(files, exclude = "*Example*_3.*",
    outfile = outfile)
  expect_true(file.exists(outfile))
  expect_false(file.exists(infile))
  expect_is(template, "data.frame")
  expect_equal(colnames(template), c("Setup Time", "Position", "File"))
  expect_equal(nrow(template), 2L)
  expect_true(all(! is.na(template)))
  expect_true(all("character" == vapply(template, class, "")))
  unlink(outfile)

  expect_error(template <- collect_template(files, exclude = "*Example*_3.*",
    outfile = outfile, previous = infile))
  expect_false(file.exists(outfile))
  expect_false(file.exists(infile))

  unlink(c(infile, outfile))

})

## collect_template
test_that("templates can be collected with added columns", {

  files <- INFILES.2[1L:3L]
  to.add <- c("A", "B")

  expect_equal(length(files), 3L)
  expect_true(all(file.exists(files)))

  # if 'exclude' was not specific enough, 'template' would be NULL
  template <- collect_template(files, exclude = "*Example*_3.*",
    add.cols = to.add)
  expect_is(template, "data.frame")
  expect_equal(colnames(template),
    c("Setup Time", "Position", "File", to.add))
  expect_equal(nrow(template), 2L)
  expect_true(all(!is.na(template[, 1L:3L])))
  expect_true(all(is.na(template[, to.add])))
  expect_true(all("character" == vapply(template, class, "")))

  # if 'exclude' was not specific enough, 'template' would be empty
  expect_that(template <- collect_template(files, exclude = "*Example*_3.*",
    add.cols = to.add, demo = TRUE), shows_message())
  expect_equal(template, files[1L:2L])

})


## collect_template
test_that("templates can be collected from MOPMX objects", {
  got <- collect_template(MOPMX.1)
  expect_equal(nrow(got), length(plates(MOPMX.1)))
  got.2 <- collect_template(MOPMX.1, add.cols = letters[1:2])
  expect_equal(ncol(got.2), ncol(got) + 2L)
})


################################################################################
#
# Batch conversion functions
#


## process_io
## UNTESTED

## batch_process
test_that("batch conversion works in demo mode", {

  infiles <- INFILES.2
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


## batch_process
test_that("batch conversion works", {

  infiles <- INFILES.2
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

## batch_opm
test_that("batch conversion to yaml works", {
  infiles <- INFILES.2
  expect_warning(got <- batch_opm(infiles, missing.error = FALSE, demo = TRUE))
  infiles <- infiles[-4L]
  expect_is(got, "matrix")
  expect_equal(got[, 1L], infiles)
  expect_path_equal(got[, 2L], sub("\\.csv\\.xz$", ".yml", infiles))
  infiles <- infiles[1L]
  outdir <- tempdir()
  exp.outfile <- file.path(outdir, "Example_1.yml")
  expect_false(file.exists(exp.outfile))
  got <- batch_opm(infiles, outdir = tempdir(), verbose = TRUE)
  expect_true(file.exists(exp.outfile))
  unlink(exp.outfile)
})


################################################################################
#
# Splitting files
#

## split_files
test_that("files can be split", {

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

## clean_filenames
test_that("file names can be cleaned", {
  x <- c("a b/c/x-y-z.txt", "d--z/z?-z. .csv", "/xxx/y y/?_?-abcd*+.txt",
    "sapif89asdh_&#-*_asdfhu.---", "!$%&+()=?`")
  expect_that(got <- clean_filenames(x, demo = TRUE), shows_message())
  expect_equal(names(got), x[-1L])
  expect_equivalent(got, c("d--z/z-z.csv", "/xxx/y y/abcd.txt",
    "sapif89asdh-asdfhu", "__EMPTY__00001__"))
  expect_warning(got <- clean_filenames(c("", "a", "?"), demo = TRUE))
  expect_equal(names(got), "?")
})


################################################################################

