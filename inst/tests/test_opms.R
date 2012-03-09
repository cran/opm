

library(testthat)


# IO testing is done in 'test_io.R' , with the exception of read_opm() 
# applied to the example files in new CSV format, which is done here because 
# some input data are needed.
#
context("Testing the methods of the OPMS class and its IO functions.")


loc <- source_location()
if (is.null(loc))
  loc <- .path.package("opm")
  
example.file <- file.path(test_file_dir(loc),
  c("Example_1.csv.xz", "Example_2.csv.xz", "Example_Old_Style_1.csv.xz"))

OPM.1 <- read_single_opm(example.file[1L])
OPM.2 <- read_single_opm(example.file[2L])
OPM.3 <- read_single_opm(example.file[3L])
ORGN <- "Bacillus subtilis"
metadata(OPM.1) <- list(run = 4L, organism = ORGN)
metadata(OPM.2) <- list(run = 3L, organism = ORGN)
OPMS.INPUT <- opms(OPM.1, OPM.2)
THIN.AGG <- do_aggr(thin_out(OPMS.INPUT, 10), boot = 2L, verbose = FALSE)


################################################################################
#
# Input from files
#

test_that("read_opm can read a single file", {
  files <- example.file[1L]
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


test_that("read_opm can read two compatible files", {
  files <- example.file[1L:2L]
  opm.1 <- read_opm(files)
  expect_is(opm.1, "OPMS")
  opm.1 <- read_opm(files, convert = "try")
  expect_is(opm.1, "OPMS")
  expect_equal(NULL, names(plates(opm.1)))
  opm.1 <- read_opm(files, convert = "yes")
  expect_is(opm.1, "OPMS")
  expect_equal(NULL, names(plates(opm.1)))
  opm.1 <- read_opm(files, convert = "no")
  expect_is(opm.1, "list")
  expect_equal(2L, length(opm.1))
  expect_equal(NULL, names(opm.1))
})


test_that("read_opm can read three partially incompatible files", {
  files <- example.file
  expect_warning(opm.1 <- read_opm(files))
  expect_is(opm.1, "list")
  expect_equal(3L, length(opm.1))
  expect_equal(NULL, names(opm.1))
  expect_warning(opm.1 <- read_opm(files, convert = "try"))
  expect_is(opm.1, "list")
  expect_equal(3L, length(opm.1))
  expect_equal(NULL, names(opm.1))
  expect_error(opm.1 <- read_opm(files, convert = "yes"))
  opm.1 <- read_opm(files, convert = "no")
  expect_is(opm.1, "list")
  expect_equal(3L, length(opm.1))
  expect_equal(NULL, names(opm.1))
})


################################################################################
#
# Method presence
#


test_that("OPMS has all method of OPMA", {
  m <- tryCatch(as.character(getGenerics("package:opm")),
    error = function(e) character())
  if (length(m)) {
    opm.methods <- m[sapply(m, existsMethod, OPMA) | 
      sapply(m, existsMethod, OPM) | sapply(m, existsMethod, WMD)]
    opms.methods <- m[sapply(m, existsMethod, OPMS)]
    expect_equal(character(), setdiff(opm.methods, opms.methods))
    expect_true(length(setdiff(opms.methods, opm.methods)) > 0)
  }
})


################################################################################
#
# Combination functions
#


test_that("we can use + to put plates together", {
  x <- OPM.1 + OPM.2
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 2L)
  x <- x + OPM.2
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 3L)
  expect_error(x <- x + OPM.3)
  expect_error(x <- x + 5)
  expect_error(x <- x + "abc")
  x <- OPM.1 + OPM.1
  expect_equal(2L, dim(x)[1L])
  y <- x + OPM.1
  expect_equal(3L, dim(y)[1L])
  y <- x + list(OPM.1)
  expect_equal(3L, dim(y)[1L])
  y <- x + list(OPM.1, OPM.2)
  expect_equal(4L, dim(y)[1L])
  y <- x + x
  expect_equal(4L, dim(y)[1L])
})
 
          
test_that("we can use c to put plates together", {
  x <- c(OPM.1)
  expect_is(x, "OPM")
  expect_equal(x, OPM.1)
  x <- c(OPM.1, 55L)
  expect_is(x, "list")
  expect_equal(length(x), 2L)
  x <- c(OPM.1, 55L, "abc")
  expect_is(x, "list")
  expect_equal(length(x), 3L)
  x <- c(OPM.1, OPM.2)
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 2L)
  x <- c(x, OPM.2)
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 3L)
  x <- c(x, 55L)
  expect_is(x, "list")
  expect_equal(length(x), 2L)
  x <- c(x, 55L, "abc")
  expect_is(x, "list")
  expect_equal(length(x), 4L)  
})
  

test_that("we can use opms and new to put plates together", {
  
  # we need more than one plate
  expect_error(opms(OPM.1), "less than two plates submitted")
  
  # we need a uniform plate type
  expect_error(opms(OPM.1, OPM.3),
    "plate types are not uniform: PM01 <=> PM20")
  
  x <- list(a = OPM.1, b = OPM.2)
  x.opms <- opms(x)
  expect_is(x.opms, "OPMS")
  expect_equal(2L, length(x.opms))
  expect_equal(NULL, names(plates(x.opms)))
  
  x.opms <- new("OPMS", plates = x)
  expect_equal(NULL, names(plates(x.opms)))
  
})


################################################################################
#
# Getter functions
#


test_that("we can query information over all plates", {  
  
  expect_equal(2L, length(OPMS.INPUT))
  
  expect_equal(c(2L, dim(OPM.1)), dim(OPMS.INPUT))
  
  pl <- plates(OPMS.INPUT)
  expect_is(pl, "list")
  expect_equal(length(pl), 2L)
  expect_true(all(sapply(pl, class) == "OPM"))
  
  m <- max(OPMS.INPUT)
  expect_true(m > max(OPMS.INPUT, "A01"))
  mm <- minmax(OPMS.INPUT)
  expect_true(m > mm)
  expect_true(mm < minmax(OPMS.INPUT, "A01"))
  
  expect_that(s <- summary(OPMS.INPUT), shows_message())
  expect_is(s, "list")
  expect_equal(length(s), 2L)
  expect_true(all(sapply(s, class) == "list"))
  
  pt.got <- plate_type(OPMS.INPUT)
  expect_is(pt.got, "character")
  expect_equal(length(pt.got), 1L)

  w.got <- wells(OPMS.INPUT)
  expect_is(w.got, "character")
  expect_equal(length(w.got), dim(OPMS.INPUT)[3L])
  
})
  
  
test_that("we can get the measurements and the hours from the plates", {
  
  m.got <- measurements(OPMS.INPUT)
  expect_is(m.got, "list")
  expect_true(all(sapply(m.got, is.matrix)))

  m.got <- measurements(OPMS.INPUT, 3L)
  expect_is(m.got, "list")
  expect_true(all(sapply(m.got, is.matrix)))

  w.got <- well(OPMS.INPUT)
  expect_is(w.got, "list")
  expect_true(all(sapply(w.got, is.matrix)))

  w.got <- well(OPMS.INPUT, 3L)
  expect_is(w.got, "matrix")
  expect_equal(nrow(w.got), length(OPMS.INPUT))

  w.got <- well(OPMS.INPUT, 3L:4L)
  expect_is(w.got, "list")
  expect_true(all(sapply(w.got, is.matrix)))

  h.got <- hours(OPMS.INPUT)
  expect_is(h.got, "numeric")
  expect_equal(length(h.got), length(OPMS.INPUT))
  
  h.got <- hours(OPMS.INPUT, what = "all")
  expect_is(h.got, "matrix")
  expect_equal(nrow(h.got), length(OPMS.INPUT))
  
})


test_that("we can query information from the plates", {
  
  sum.got <- summary(OPMS.INPUT)
  expect_is(sum.got, "list")
  expect_equal(length(sum.got), 2L)
  
  cd.got <- csv_data(OPMS.INPUT)
  expect_is(cd.got, "matrix")
  expect_equal(dim(cd.got), c(2L, 10L))

  ha.got <- has_aggr(OPMS.INPUT)
  expect_is(ha.got, "logical")
  expect_equal(length(ha.got), length(OPMS.INPUT))

  p.got <- position(OPMS.INPUT)
  expect_is(p.got, "character")
  expect_equal(length(p.got), length(OPMS.INPUT))
  
  st.got <- setup_time(OPMS.INPUT)
  expect_is(st.got, "character")
  expect_equal(length(st.got), length(OPMS.INPUT))
  
})


## UNTESTED: oapply (see the example)


################################################################################
#
# Getter/setter functions for aggregated data
#


test_that("we can aggregate plates", {
  expect_is(OPMS.INPUT, "OPMS")
  expect_false(any(has_aggr(OPMS.INPUT)))
  expect_is(THIN.AGG, "OPMS")
  expect_true(all(has_aggr(THIN.AGG)))
  expect_equal(metadata(OPMS.INPUT), metadata(THIN.AGG))
  ag.got <- aggregated(THIN.AGG)
  expect_is(ag.got, "list")
  expect_equal(length(ag.got), length(THIN.AGG))
  expect_true(all(sapply(ag.got, is.matrix)))  
  set.got <- aggr_settings(THIN.AGG)
  expect_is(set.got, "list")
  expect_true(all(sapply(set.got, is.list)))
  expect_true(all(sapply(set.got, function(x) names(x) == c(PROGRAM, OPTIONS))))
})


test_that("we can aggregate plates using the fast method", {
  fast.agg <- do_aggr(thin_out(OPMS.INPUT, 10), program = "opm-fast")
  expect_is(fast.agg, "OPMS")
  expect_true(all(has_aggr(fast.agg)))
  expect_equal(metadata(OPMS.INPUT), metadata(fast.agg))
  ag.got <- aggregated(fast.agg)
  expect_is(ag.got, "list")
  expect_equal(length(ag.got), length(fast.agg))
  expect_true(all(sapply(ag.got, is.matrix)))  
  ag.got.2 <- aggregated(THIN.AGG)
  expect_equal(lapply(ag.got, colnames), lapply(ag.got.2, colnames))
  expect_equal(lapply(ag.got, rownames), lapply(ag.got.2, rownames))
})


################################################################################
#
# Other setter functions
#

  
test_that("we can change the plate type", {
  
  x <- gen_iii(OPMS.INPUT)
  expect_equal(class(x), class(OPMS.INPUT))
  expect_equal(dim(x), dim(OPMS.INPUT))
  expect_false(plate_type(x) == plate_type(OPMS.INPUT))
  
})


################################################################################
#
# Metadata functions (including the infix operators)
#


test_that("we can query the metadata", {
   
  md.got <- metadata(OPMS.INPUT)
  expect_is(md.got, "list")
  expect_equal(length(md.got), length(OPMS.INPUT))
  expect_true(all(sapply(md.got, is.list)))
  
  md.got <- metadata(OPMS.INPUT, "organism")
  expect_is(md.got, "character")
  expect_equal(length(md.got), length(OPMS.INPUT))
  
  md.got <- metadata(OPMS.INPUT, list("not.there"))
  expect_is(md.got, "list")
  expect_true(all(sapply(md.got, is.list)))
  expect_true(all(sapply(md.got, names) == "not.there"))
  expect_true(all(sapply(md.got, function(x) is.null(x$not.there))))

  expect_error(md.got <- metadata(OPMS.INPUT, list("not.there"), strict = TRUE))
  
})


test_that("we can query the metadata keys with little-k", {
  expect_equal(c(TRUE, TRUE), "organism" %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), c("organism", "run") %k% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), "not there" %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), list(organism = "dummy") %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE),
    list(organism = "dummy", run = "dummy") %k% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(`not there` = missing) %k% OPMS.INPUT)
})


test_that("we can query the metadata keys with large-k", {
  expect_equal(c(TRUE, TRUE), "organism" %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), c("organism", "run") %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), "not there" %K% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), list(organism = "dummy") %K% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE),
    list(organism = "dummy", run = "dummy") %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(`not there` = missing) %K% OPMS.INPUT)
})


test_that("we can query the metadata values with little-q", {
  expect_equal(c(TRUE, TRUE), c(organism = ORGN) %q% OPMS.INPUT)
  expect_equal(c(FALSE, TRUE), c(organism = ORGN, run = 3L) %q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), c(missing = "not there") %q% OPMS.INPUT)  
  expect_equal(c(FALSE, FALSE), c("not there") %q% OPMS.INPUT)
})


test_that("we can query the metadata values with little-q", {
  expect_equal(c(TRUE, TRUE), list(organism = ORGN) %q% OPMS.INPUT)
  expect_equal(c(FALSE, TRUE), list(organism = ORGN, run = 3L) %q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(missing = "not there") %q% OPMS.INPUT)  
  expect_equal(c(FALSE, FALSE), list("not there") %q% OPMS.INPUT)
})


test_that("we can query the metadata characters", {
  chars <- metadata_chars(OPMS.INPUT)
  expect_equal(chars, structure(ORGN, names = ORGN))
  chars <- metadata_chars(OPMS.INPUT, classes = "integer")
  expect_equal(chars, structure(c(3L, 4L, ORGN), names = c(3L, 4L, ORGN)))
  chars <- metadata_chars(OPMS.INPUT, values = FALSE)
  expect_equal(chars, structure(c("organism", "run"), 
    names = c("organism", "run")))
})


test_that("we can modify the metadata with a mapping function", {
  got <- map_metadata(OPMS.INPUT, mapping = identity)
  expect_equal(got, OPMS.INPUT)
  got <- metadata(map_metadata(OPMS.INPUT, mapping = function(x) "x"))
  expect_equal(got, list(list(run = "x", organism = "x"),
    list(run = "x", organism = "x")))
  got <- metadata(map_metadata(OPMS.INPUT, mapping = function(x) "x",
    classes = "character"))
  expect_equal(got, list(list(run = 4L, organism = "x"),
    list(run = 3L, organism = "x")))
})


test_that("we can modify the metadata by setting them", {
  x <- OPMS.INPUT
  wanted <- list(a = 3, b = 7)
  md <- metadata(x) <- wanted
  expect_equal(md, wanted)
  got <- metadata(x)
  expect_equal(got[[1L]], wanted)
  expect_equal(got[[2L]], wanted)
  e.coli <- "E. coli"
  e.coli.list <- list(organism = e.coli)
  md <- metadata(x, "organism") <- e.coli
  expect_equal(md, e.coli)
  got <- metadata(x)
  expect_equal(got[[1L]], c(wanted, e.coli.list))
  expect_equal(got[[2L]], c(wanted, e.coli.list))
  md <- metadata(x, "organism") <- NULL
  expect_equal(md, NULL)
  got <- metadata(x)
  expect_equal(got[[1L]], wanted)
  expect_equal(got[[2L]], wanted)
})


################################################################################
#
# Conversion functions: OPMS => lists.
#


## TODO


################################################################################
#
# Thinning out and subsetting
#


test_that("we can thin out the plates", {
  dims <- dim(OPMS.INPUT)
  dims[2L] <- floor(dims[2L] / 10)
  thin <- thin_out(OPMS.INPUT, 10)
  expect_equal(dim(thin), dims)
  expect_equal(metadata(thin), metadata(OPMS.INPUT))
})


test_that("we can subset all plates", {
  small <- OPMS.INPUT[, , 1:10]
  expect_is(small, "OPMS")
  dims <- dim(OPMS.INPUT)
  dims[3L] <- 10L
  expect_equal(dims, dim(small))
  expect_equal(metadata(small), metadata(OPMS.INPUT))
  tiny <- small[, 1L:10L]
  expect_is(tiny, "OPMS")
  dims[2L] <- 10L
  expect_equal(dims, dim(tiny))
  expect_equal(metadata(tiny), metadata(small))
  tiny.2 <- OPMS.INPUT[, 1L:10L, 1L:10L]
  expect_equal(tiny.2, tiny)
})


test_that("we can subset the entire object", {
  few <- OPMS.INPUT[]
  expect_equal(few, OPMS.INPUT)
  few <- OPMS.INPUT[1L:2L]
  expect_equal(few, OPMS.INPUT)
  few <- OPMS.INPUT[1L]
  expect_is(few, "OPM")
  dims <- dim(OPMS.INPUT)[-1L]
  expect_equal(dim(few), dims)
  few <- OPMS.INPUT[2L, , 1:10]
  expect_is(few, "OPM")
  dims[2L] <- 10L
  expect_equal(dim(few), dims)
})


test_that("we can subset the plates based on the metadata", {
  query <- list(organism = ORGN, run = 3L) 
  other.query <- list(organism = ORGN, run = 5L) # wrong value
  third.query <- list(organism = ORGN, runs = 3L) # wrong key
  subset <- select(OPMS.INPUT, query = query, values = TRUE)
  expect_is(subset, OPM)
  subset <- select(OPMS.INPUT, query = query, use = "q")
  expect_is(subset, OPM)
  subset <- select(OPMS.INPUT, query = query, values = FALSE)
  expect_is(subset, OPMS)
  subset <- select(OPMS.INPUT, query = query, use = "k")
  expect_is(subset, OPMS)
  subset <- select(OPMS.INPUT, query = other.query, values = TRUE)
  expect_is(subset, "NULL")
  subset <- select(OPMS.INPUT, query = other.query, use = "q")
  expect_is(subset, "NULL")
  subset <- select(OPMS.INPUT, query = other.query, values = FALSE)
  expect_is(subset, OPMS)
  subset <- select(OPMS.INPUT, query = other.query, use = "k")
  expect_is(subset, OPMS)
  subset <- select(OPMS.INPUT, query = third.query, values = FALSE)
  expect_is(subset, "NULL")
  subset <- select(OPMS.INPUT, query = third.query, use = "k")
  expect_is(subset, "NULL")
})
  

test_that("we can subset based on common time points", {
  expect_warning(x <- c(OPM.1[1:50, ], OPM.2))
  expect_equal(as.vector(oapply(x, dim)), c(50L, 96L, 384L, 96L))
  got <- select(x, time = TRUE)
  expect_equal(as.vector(oapply(got, dim)), c(50L, 96L, 50L, 96L))
  got <- select(x, use = "t")
  expect_equal(as.vector(oapply(got, dim)), c(50L, 96L, 50L, 96L))
})


################################################################################
#
# Extraction of character matrices
#


test_that("we can extract aggregated parameters as matrix", {
  
  rn <- paste(metadata(THIN.AGG, "organism"), metadata(THIN.AGG, "run"),
    sep = "||")
  gn <- paste(metadata(THIN.AGG, "run"), metadata(THIN.AGG, "organism"),
    sep = "||")
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), sep = "||")
  expect_is(mat, "matrix")
  expect_equal(dim(mat), c(2L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(NULL, attr(mat, "row.groups"))
  expect_equal(rn, rownames(mat))
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "lambda", as.groups = list("run", "organism"), sep = "||")
  expect_is(mat, "matrix")
  expect_equal(dim(mat), c(2L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(as.factor(gn), attr(mat, "row.groups"))
  expect_equal(rn, rownames(mat))
  
})
  
 
test_that("we can extract aggregated parameters as matrix with CIs", {
          
  rn <- paste(metadata(THIN.AGG, "organism"), metadata(THIN.AGG, "run"),
    sep = "+++")
  gn <- paste(metadata(THIN.AGG, "run"), metadata(THIN.AGG, "organism"),
    sep = "+++")
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), ci = TRUE, 
    sep = "+++")
  expect_is(mat, "matrix")
  expect_is(mat[1L], "numeric")
  expect_equal(dim(mat), c(6L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(grepl(rn[1L], rownames(mat), fixed = TRUE), c(T, T, T, F, F, F))
  expect_equal(grepl(rn[2L], rownames(mat), fixed = TRUE), c(F, F, F, T, T, T))
  expect_equal(NULL, attr(mat, "row.groups"))
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "mu", ci = TRUE, as.groups = list("organism"), sep = "+++")
  expect_is(mat, "matrix")
  expect_is(mat[1L], "numeric")
  expect_equal(dim(mat), c(6L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(grepl(rn[1L], rownames(mat), fixed = TRUE), c(T, T, T, F, F, F))
  expect_equal(grepl(rn[2L], rownames(mat), fixed = TRUE), c(F, F, F, T, T, T))
  expect_equal(rep(as.factor(metadata(THIN.AGG, "organism")), each = 3L), 
    attr(mat, "row.groups"))
  
})


test_that("we can extract aggregated parameters as dataframe", {
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "lambda", dataframe = TRUE, sep = "***")
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(2L, 99L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter", 
    wells(THIN.AGG, full = TRUE)))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 2L))
  expect_equal(as.character(mat[, 2L]), c("4", "3"))
  expect_equal(as.character(mat[, 3L]), rep("lambda", 2L))
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "mu", dataframe = TRUE, sep = "&", 
    as.groups = list("run", "organism"))
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(2L, 101L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter", 
    wells(THIN.AGG, full = TRUE), "run", "organism"))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_true(all(sapply(mat[, 100L:101L], is.factor)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 2L))
  expect_equal(as.character(mat[, 2L]), c("4", "3"))
  expect_equal(as.character(mat[, 3L]), rep("mu", 2L))
  expect_equal(as.character(mat[, 100L]), c("4", "3"))
  expect_equal(as.character(mat[, 101L]), rep(ORGN, 2L))
  
})


test_that("we can extract aggregated parameters as dataframe with CIs", {
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE)
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(6L, 99L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter", 
    wells(THIN.AGG, full = TRUE)))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 6L))
  expect_equal(as.character(mat[, 2L]), rep(c("4", "3"), each = 3L))
  expect_equal(as.character(mat[, 3L]) == rep("lambda", 6L), 
    c(T, F, F, T, F, F))
  expect_true(all(grepl("lambda", as.character(mat[, 3L]), fixed = TRUE)))
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE,
    as.groups = list("run", "organism"))
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(6L, 101L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter", 
    wells(THIN.AGG, full = TRUE), "run", "organism"))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_true(all(sapply(mat[, 100L:101L], is.factor)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 6L))
  expect_equal(as.character(mat[, 2L]), rep(c("4", "3"), each = 3L))
  expect_equal(as.character(mat[, 3L]) == rep("lambda", 6L), 
    c(T, F, F, T, F, F))
  expect_true(all(grepl("lambda", as.character(mat[, 3L]), fixed = TRUE)))
  expect_equal(mat[, 1L], mat[, 101L])
  expect_equal(mat[, 2L], mat[, 100L])
  
})


################################################################################
#
# Heatmaps
#


test_that("we can draw a heatmap", {
  
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "A", as.groups = list("organism"))
  mat.2 <- extract(THIN.AGG, as.labels = list("organism", "run"), 
    subset = "A", as.groups = list("organism"), dataframe = TRUE)
  
  hm <- heat_map(mat, margins = c(5, 5), use.fun = "stats")
  expect_is(hm, "list")
  expect_equal(NULL, hm$colColMap)
  expect_equal(names(hm$rowColMap), metadata(THIN.AGG, "organism"))
  
  # Data frame version
  hm.2 <- heat_map(mat.2, as.labels = c("organism", "run"), 
    as.groups = "organism", margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)
  
  # Distance given as function or list
  hm.2 <- heat_map(mat, distfun = dist, margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)
  hm.2 <- heat_map(mat, distfun = list(method = "euclidean"), 
    margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)
  
  # Clustering function given in distinct ways
  hm <- heat_map(mat, hclustfun = hclust, margins = c(5, 5), use.fun = "stats")
  expect_false(identical(hm.2, hm))
  hm.2 <- heat_map(mat, hclustfun = "complete", margins = c(5, 5), 
    use.fun = "stats")
  expect_equal(hm, hm.2)
  hm.2 <- heat_map(mat, hclustfun = list(method = "complete"), 
    margins = c(5, 5), use.fun = "stats")
  expect_equal(hm, hm.2)

  # Column groups
  group_fun <- function(x) substr(x, 1, 1)
  hm <- heat_map(mat, margins = c(5, 5), c.groups = group_fun)
  groups <- group_fun(colnames(mat))
  hm.2 <- heat_map(mat, margins = c(5, 5), c.groups = groups)
  expect_equal(hm, hm.2)
  expect_equivalent(groups, names(hm$colColMap))
  
})


################################################################################
#
# CI plot methods
#


test_that("we can draw a tie-fighter (CI) plot", {
  
  legend <- ci_plot(THIN.AGG[, , 1:12], as.labels = list("organism", "run"), 
    subset = "A", na.action = "ignore")
  expect_equal(c("1: Bacillus subtilis 3", "2: Bacillus subtilis 4"), legend)
  
  legend <- ci_plot(THIN.AGG[, , 1:6], as.labels = list("organism"), 
    subset = "A", na.action = "ignore", legend.field = NULL, bg = "lightgrey",
    x = "bottom")
  expect_equal(c("1: Bacillus subtilis", "2: Bacillus subtilis"), legend)
  
})
  

################################################################################
#
# XY plot and level plot, and their helper functions
#


test_that("we can apply the OPMX functions to OPMS objects", {
  
  mt.got <- main_title(OPMS.INPUT, list())
  expect_is(mt.got, "character")
  expect_equal(length(mt.got), 1L)
  
  nc.got <- negative_control(OPMS.INPUT, neg.ctrl = TRUE)
  expect_is(nc.got, "numeric")
  expect_equal(length(nc.got), 1L)
  
})

  
test_that("we can flatten plates", {
  opms.input <- OPMS.INPUT[, 1L:10L]
  base.colnames <- c("Plate", "Time", "Well", "Value")
  flat <- flatten(opms.input)
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), base.colnames)
  exp.len <- Reduce(`*`, dim(opms.input))
  expect_equal(exp.len, nrow(flat)) # warning: depends on length of runs 
  plate.nums <- unique(flat[, "Plate"])
  expect_equal(paste("Plate", 1:2), as.character(plate.nums))
})
  
  
test_that("we can flatten plates including the metadata", {
  
  opms.input <- OPMS.INPUT[, 1L:10L]
  base.colnames <- c("Plate", "Time", "Well", "Value")
  
  # Flatten with metadata no. 1
  flat <- flatten(opms.input, include = "organism")
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("organism", base.colnames))
  orgns <- unique(as.character(flat[, "organism"]))
  expect_equal(orgns, ORGN)
  
  # Flatten with metadata no. 2
  flat <- flatten(opms.input, include = c("organism", "run"))
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("organism", "run", base.colnames))
  runs <- unique(flat[, "run"])
  expect_equal(4:3, runs)
  
})  


test_that("we can plot plates", {
  
  opms.input <- OPMS.INPUT[, 1L:10L]
  expect_is(opms.input, "OPMS")

  # Draw levelplot
  expect_error(got <- level_plot(opms.input, colors = "black"), 
    "need at least two non-NA values to interpolate")
  got <- level_plot(opms.input)
  expect_is(got, "trellis")
  
  # Draw xyplot
  got <- xy_plot(opms.input)
  expect_is(got, "trellis")
  got <- xy_plot(opms.input, include = "organism")
  expect_is(got, "trellis")
  expect_error(got <- xy_plot(opms.input, include = "doesnotexist"))
  
})


