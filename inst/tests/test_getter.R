

library(testthat)
context("Testing the getter functions of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## measurements
test_that("all measurements are accessible", {
  m.got <- measurements(OPMS.INPUT)
  expect_is(m.got, "list")
  expect_true(all(sapply(m.got, is.matrix)))
  m.got <- measurements(OPMS.INPUT, 3L)
  expect_is(m.got, "list")
  expect_true(all(sapply(m.got, is.matrix)))
})


################################################################################


## [
test_that("data from example file 1 can be subset", {
  expect_is(OPM.1, "OPM")
  small <- OPM.1[31:40, 11:20]
  expect_is(small, "OPM")
  expect_equal(dim(small), c(10, 10))
  small <- OPM.1[31:35, ]
  expect_is(small, "OPM")
  expect_equal(dim(small), c(5, 96))
  small <- OPM.1[, 11:20]
  expect_is(small, "OPM")
  expect_equal(dim(small), c(384, 10))
})

## [
test_that("OPMS objects can be subset", {
  tiny <- SMALL.AGG[, 1L:5L]
  expect_is(tiny, "OPMA")
  expect_equal(csv_data(tiny), csv_data(SMALL.AGG))
  expect_equal(metadata(tiny), metadata(SMALL.AGG))
  expect_equal(wells(tiny), colnames(aggregated(tiny)))
  # drop the aggregated data
  tiny <- SMALL.AGG[, 1L:5L, drop = TRUE]
  expect_is(tiny, "OPM")
  expect_equal(csv_data(tiny), csv_data(SMALL.AGG))
  expect_equal(metadata(tiny), metadata(SMALL.AGG))
})

## [
test_that("all plates can be subset", {
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

## [
test_that("the entire OMSS object can be subset", {
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


################################################################################


## well
test_that("the content of the wells can be subset", {
  w.got <- well(OPMS.INPUT)
  expect_is(w.got, "list")
  expect_true(all(sapply(w.got, is.matrix)))
  w.got <- well(OPMS.INPUT, 3L)
  expect_is(w.got, "matrix")
  expect_equal(nrow(w.got), length(OPMS.INPUT))
  w.got <- well(OPMS.INPUT, 3L:4L)
  expect_is(w.got, "list")
  expect_true(all(sapply(w.got, is.matrix)))
})


## hours
test_that("hours can be explicitely queried", {
  expect_equal(hours(OPM.1), 95.75) # see also the examples
  h.got <- hours(OPMS.INPUT)
  expect_is(h.got, "numeric")
  expect_equal(length(h.got), length(OPMS.INPUT))
  h.got <- hours(OPMS.INPUT, what = "all")
  expect_is(h.got, "matrix")
  expect_equal(nrow(h.got), length(OPMS.INPUT))
})


## max
test_that("maxima can be explicitely queried", {
  expect_equal(351, max(OPM.1))
  expect_equal(56, max(OPM.1, 'A01'))
  expect_equal(56, max(OPM.1, 1L))
  m <- max(OPMS.INPUT)
  expect_true(m > max(OPMS.INPUT, "A01"))
})


## minmax
test_that("lowest maxima can be explicitely queried", {
  expect_equal(15, minmax(OPM.1))
  expect_equal(56, minmax(OPM.1, 'A01'))
  expect_equal(56, minmax(OPM.1, 1L))
  mm <- minmax(OPMS.INPUT)
  expect_true(max(OPMS.INPUT) > mm)
  expect_true(mm < minmax(OPMS.INPUT, "A01"))
})


## dim
test_that("dimensions can be explicitely queried", {
  expect_equal(dim(OPM.1), c(384, 96))
  expect_equal(c(2L, dim(OPM.1)), dim(OPMS.INPUT))
})


## length
test_that("the length can be queried", {
  expect_equal(2L, length(OPMS.INPUT))
})


## seq
## UNTESTED


## wells
test_that("information on the contained wells can be received", {
  w.got <- wells(OPMS.INPUT)
  expect_is(w.got, "character")
  expect_equal(length(w.got), dim(OPMS.INPUT)[3L])
  w.got <- wells(OPMS.INPUT)
  expect_is(w.got, "character")
  expect_equal(length(w.got), dim(OPMS.INPUT)[3L])
})


## csv_data
test_that("CSV data can be accessed", {
  expect_equal(length(csv_data(OPM.1)), 10L)
  expect_equal(length(csv_data(OPM.WITH.MD)), 10L)
  expect_equal(length(csv_data(SMALL)), 10L)
  expect_equal(length(csv_data(OPM.WITH.MD[, 1L:10L])), 10L)
  expect_equal(length(csv_data(SMALL.AGG)), 10L)
  # Picking CSV data
  picked <- csv_data(OPM.1, c("File", "Setup Time"))
  expect_is(picked, "character")
  expect_equivalent(picked, c(filename(OPM.1), setup_time(OPM.1)))
  missing.key <- "19825761285616"
  error.msg <- paste("could not find key", missing.key)
  expect_error(csv_data(OPM.1, c("File", missing.key)), error.msg)
  # OPMS method
  cd.got <- csv_data(OPMS.INPUT)
  expect_is(cd.got, "matrix")
  expect_equal(dim(cd.got), c(2L, 10L))
})


## filename
test_that("filename of example object can be explicitely queried", {
  expect_equal(filename(OPM.1), INFILES[1L])
})


## plate_type
test_that("plate types can be explicitely queried", {
  expect_equal(plate_type(OPM.1), "PM01")
  pt.got <- plate_type(OPMS.INPUT)
  expect_is(pt.got, "character")
  expect_equal(length(pt.got), 1L)
})


## plate_type
test_that("plate names can be normalized", {
  # Normal input arguments
  x <- c("<strange>", "PM-M3 A", "PM09", "pm10b", "pmM10D", "PM1")
  exp <- c("<strange>", "PM-M03-A", "PM09", "PM10-B", "PM-M10-D", "PM01")
  got <- plate_type(x, subtype = TRUE)
  expect_equal(got, exp)
  # Microstation plates
  x <- c("<strange>", "ECO", "SFN2", "GP2", "SF-N2", "G-N2")
  exp <- c("<strange>", "ECO", "SF-N2", "SF-P2", "SF-N2", "SF-N2")
  got <- plate_type(x, subtype = TRUE)
  expect_equal(got, exp)
  # The internally used names must already be normalized
  standard.names <- names(PLATE_MAP)
  expect_equal(plate_type(standard.names), standard.names)
  appended <- paste(standard.names, letters)
  cat(appended[plate_type(appended) != standard.names])
  expect_equal(plate_type(appended), standard.names)
  expect_equal(names(PLATE_MAP), colnames(WELL_MAP))
})


## setup_time
test_that("setup times can be explicitely queried", {
  expect_equal(setup_time(OPM.1), "8/30/2010 11:28:54 AM")
  st.got <- setup_time(OPMS.INPUT)
  expect_is(st.got, "character")
  expect_equal(length(st.got), length(OPMS.INPUT))
})


## position
test_that("plate positions can be explicitely queried", {
  expect_equal(position(OPM.1), "21-B")
  p.got <- position(OPMS.INPUT)
  expect_is(p.got, "character")
  expect_equal(length(p.got), length(OPMS.INPUT))
})


## has_aggr
test_that("aggregated values can be obtained", {
  expect_false(has_aggr(OPM.1))
  ha.got <- has_aggr(OPMS.INPUT)
  expect_is(ha.got, "logical")
  expect_equal(length(ha.got), length(OPMS.INPUT))
  expect_false(any(ha.got))
  expect_true(all(has_aggr(THIN.AGG)))
  expect_false(has_aggr(SMALL))
  expect_true(has_aggr(SMALL.AGG))
})


## has_disc
## UNTESTED


## summary
test_that("a summary can be printed", {
  # OPM method
  x <- summary(OPM.1)
  expect_is(x, "OPM_Summary")
  expect_true(length(x) > 7L)
  capture.output(expect_equal(print(x), x))
  # OPMS method
  s <- summary(OPMS.INPUT)
  capture.output(expect_equal(print(s), s))
  expect_is(s, "OPMS_Summary")
  expect_equal(length(s), length(OPMS.INPUT))
  expect_true(all(vapply(s, inherits, logical(1L), "OPM_Summary")))
})


################################################################################


## aggregated
test_that("aggregated data in OPM objects can be queried", {
  aggr <- aggregated(SMALL.AGG)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(12L, 10L))
  aggr <- aggregated(SMALL.AGG, subset = "mu")
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(3L, 10L))
  aggr <- aggregated(SMALL.AGG, subset = c("mu", "AUC"), ci = FALSE)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(2L, 10L))
  aggr <- aggregated(SMALL.AGG, subset = c("mu", "lambda", "AUC"), ci = TRUE)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(9L, 10L))
})


## aggregated
test_that("aggregated data in OPMS objects can be queried", {
  ag.got <- aggregated(THIN.AGG)
  expect_is(ag.got, "list")
  expect_equal(length(ag.got), length(THIN.AGG))
  expect_true(all(sapply(ag.got, is.matrix)))
})


## aggr_settings
test_that("aggregation settings can be queried", {
  settings <- aggr_settings(SMALL.AGG)
  expect_is(settings, "list")
  expect_equal(length(settings), 4L)
  expect_false(is.null(names(settings)))
  settings <- aggr_settings(THIN.AGG)
  expect_is(settings, "list")
  expect_true(all(vapply(settings, is.list, logical(1L))))
  expect_true(all(vapply(settings, length, integer(1L)) == 4L))
})


################################################################################


## discretized
## UNTESTED


## disc_settings
## UNTESTED


################################################################################


## metadata
test_that("missing metadata result in an error if requested", {
  expect_is(OPM.1, "OPM")
  expect_equal(metadata(OPM.1), list())
  expect_equal(metadata(OPM.1, "Organism"), NULL)
  expect_error(metadata(OPM.1, "Organism", strict = TRUE))
})

## metadata
test_that("metadata have be included in example OPM object", {
  expect_is(OPM.WITH.MD, "OPM")
  exp.list <- list(File = filename(OPM.1), Organism = ORGN)
  expect_equal(metadata(OPM.WITH.MD), exp.list)
  expect_equal(metadata(OPM.WITH.MD, "Organism"), ORGN)
  expect_equal(metadata(OPM.WITH.MD, list("File", "Organism")), exp.list)
  exp.list$Organism <- NULL
  exp.list$Org <- ORGN
  expect_equal(metadata(OPM.WITH.MD, list("File", "Org"), exact = FALSE),
    exp.list)
  exp.list$Org <- NULL
  exp.list <- c(exp.list, list(Org = NULL))
  expect_equal(metadata(OPM.WITH.MD, list("File", "Org"), exact = TRUE),
    exp.list)
})

## metadata
test_that("the OPMS metadata can be queried", {
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
  expect_equal(metadata(OPMS.INPUT), metadata(THIN.AGG))
})


################################################################################


## select
## UNTESTED


## subset
test_that("the plates can be subset based on the metadata", {
  query <- list(organism = ORGN, run = 3L)
  other.query <- list(organism = ORGN, run = 5L) # wrong value
  third.query <- list(organism = ORGN, runs = 3L) # wrong key
  got <- subset(OPMS.INPUT, query = query, values = TRUE)
  expect_is(got, OPM)
  got <- subset(OPMS.INPUT, query = query, use = "q")
  expect_is(got, OPM)
  got <- subset(OPMS.INPUT, query = query, values = FALSE)
  expect_is(got, OPMS)
  got <- subset(OPMS.INPUT, query = query, use = "k")
  expect_is(got, OPMS)
  got <- subset(OPMS.INPUT, query = other.query, values = TRUE)
  expect_is(got, "NULL")
  got <- subset(OPMS.INPUT, query = other.query, use = "q")
  expect_is(got, "NULL")
  got <- subset(OPMS.INPUT, query = other.query, values = FALSE)
  expect_is(got, OPMS)
  got <- subset(OPMS.INPUT, query = other.query, use = "k")
  expect_is(got, OPMS)
  got <- subset(OPMS.INPUT, query = third.query, values = FALSE)
  expect_is(got, "NULL")
  got <- subset(OPMS.INPUT, query = third.query, use = "k")
  expect_is(got, "NULL")
})

## subset
test_that("the plates can be subset based on common time points", {
  expect_warning(x <- c(OPM.1[1:50, ], OPM.2))
  expect_equal(as.vector(oapply(x, dim)), c(50L, 96L, 384L, 96L))
  got <- subset(x, time = TRUE)
  expect_equal(as.vector(oapply(got, dim)), c(50L, 96L, 50L, 96L))
  got <- subset(x, use = "t")
  expect_equal(as.vector(oapply(got, dim)), c(50L, 96L, 50L, 96L))
})


################################################################################


## duplicated
## UNTESTED

## anyDuplicated
## UNTESTED


################################################################################


## %k%
test_that("OPM metadata keys can be queried with %k%", {
  expect_false("Organism" %k% OPM.1)
  expect_true("Organism" %k% OPM.WITH.MD)
  expect_true(c("Organism", "File") %k% OPM.WITH.MD)
  expect_false("not there" %k% OPM.WITH.MD)
  expect_true(list(Organism = "dummy", File = "dummy") %k% OPM.WITH.MD)
  expect_false(list(`not there` = "dummy") %k% OPM.WITH.MD)
})

## %k%
test_that("OPM metadata keys can be queried with %k% and a formula", {
  f <- ~ Organism == "Bacillus simplex"
  got <- f %k% OPM.WITH.MD # must do this outside of expect()
  expect_true(got)
  got <- OPM.WITH.MD %k% f
  expect_true(got)
  got <- f %k% OPM.1
  expect_false(got)
  got <- OPM.1 %k% f
  expect_false(got)
  # this should work but does not because of test_that():
  #Organism <- 'Elephas maximus'
  #got <- f %k% OPM.1
  #expect_true(got)
  #got <- OPM.1 %k% f
  #expect_true(got)
  got <- f %k% OPM.WITH.MD
  expect_true(got)
  got <- OPM.WITH.MD %k% f
  expect_true(got)
})

## %k%
test_that("OPMS metadata keys can be queried with %k%", {
  expect_equal(c(TRUE, TRUE), "organism" %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), c("organism", "run") %k% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), "not there" %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), list(organism = "dummy") %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE),
    list(organism = "dummy", run = "dummy") %k% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(`not there` = "missing") %k% OPMS.INPUT)
})

## %k%
test_that("OPMS metadata keys can be queried with %k% and a formula", {
  f <- ~ organism
  got <- f %k% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  got <- OPMS.INPUT %k% f
  expect_equal(c(TRUE, TRUE), got)
  f <- ~ organism + run # symbols are there, operations fails
  got <- f %k% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %k% f
  expect_equal(c(FALSE, FALSE), got)
  f <- ~ c(organism, run) # symbols are there, operations succeeds
  got <- f %k% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  got <- OPMS.INPUT %k% f
  expect_equal(c(TRUE, TRUE), got)
})

## %k%
test_that("OPMS metadata keys can be queried with %k% and a formula #2", {
  f <- ~ not.there
  got <- f %k% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %k% f
  expect_equal(c(FALSE, FALSE), got)
  # this should work but does not because of test_that():
  #not.there <- 42L
  #got <- f %k% OPMS.INPUT
  #expect_equal(c(TRUE, TRUE), got)
  #got <- OPMS.INPUT %k% f
  #expect_equal(c(TRUE, TRUE), got)
  #rm(not.there)
  got <- f %k% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %k% f
  expect_equal(c(FALSE, FALSE), got)
})


## %k%
test_that("OPMS metadata keys can be queried with %k% and a formula #3", {
  f <- ~ organism == "dummy"
  got <- f %k% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  OPMS.INPUT %k% f
  expect_equal(c(TRUE, TRUE), got)
  f <- ~ organism == "dummy" & run == "dummy"
  got <- f %k% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  got <- OPMS.INPUT %k% f
  expect_equal(c(TRUE, TRUE), got)
  f <- ~ organism == "dummy" & run == "dummy" & `not there` == "missing"
  got <- f %k% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %k% f
  expect_equal(c(FALSE, FALSE), got)
})


#-------------------------------------------------------------------------------


## %K%
test_that("OPM metadata keys can be queried with %K%", {
  expect_false("Organism" %K% OPM.1)
  expect_false("Organism" %K% OPM.1)
  expect_true("Organism" %K% OPM.WITH.MD)
  expect_false(c("Organism", "File") %K% OPM.WITH.MD)
  expect_false("not there" %K% OPM.WITH.MD)
  expect_true(list(Organism = "dummy", File = "dummy") %K% OPM.WITH.MD)
  expect_false(list(`not there` = "dummy") %K% OPM.WITH.MD)
})

## %K%
test_that("OPM metadata keys can be queried with %K% and a formula", {
  f <- ~ Organism == "Bacillus simplex"
  got <- f %K% OPM.WITH.MD # must do this outside of expect()
  expect_true(got)
  got <- OPM.WITH.MD %K% f
  expect_true(got)
  got <- f %K% OPM.1
  expect_false(got)
  got <- OPM.1 %K% f
  expect_false(got)
  Organism <- 'Elephas maximus'
  got <- f %K% OPM.1
  expect_false(got) # difference to %k%
  got <- f %K% OPM.1
  expect_false(got)
  got <- f %K% OPM.WITH.MD
  expect_true(got)
  got <- OPM.WITH.MD %K% f
  expect_true(got)
})

## %K%
test_that("OPMS metadata keys can be queried with %K%", {
  expect_equal(c(TRUE, TRUE), "organism" %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), c("organism", "run") %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), "not there" %K% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), list(organism = "dummy") %K% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE),
    list(organism = "dummy", run = "dummy") %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(`not there` = missing) %K% OPMS.INPUT)
})

## %K%
test_that("OPMS metadata keys can be queried with %K% and a formula", {
  f <- ~ organism
  got <- f %K% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(TRUE, TRUE), got)
  f <- ~ organism + run # symbols are there, operations fails
  got <- f %K% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(FALSE, FALSE), got)
  f <- ~ c(organism, run) # symbols are there, operations succeeds
  got <- f %K% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(TRUE, TRUE), got)
})

## %K%
test_that("OPMS metadata keys can be queried with %K% and a formula #2", {
  f <- ~ not.there
  got <- f %K% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(FALSE, FALSE), got)
  not.there <- 42L
  got <- f %K% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(FALSE, FALSE), got)
  rm(not.there)
  got <- f %K% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(FALSE, FALSE), got)
})


## %K%
test_that("OPMS metadata keys can be queried with %K% and a formula #3", {
  f <- ~ organism == "dummy"
  got <- f %K% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  OPMS.INPUT %K% f
  expect_equal(c(TRUE, TRUE), got)
  f <- ~ organism == "dummy" & run == "dummy"
  got <- f %K% OPMS.INPUT
  expect_equal(c(TRUE, TRUE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(TRUE, TRUE), got)
  f <- ~ organism == "dummy" & run == "dummy" & `not there` == "missing"
  got <- f %K% OPMS.INPUT
  expect_equal(c(FALSE, FALSE), got)
  got <- OPMS.INPUT %K% f
  expect_equal(c(FALSE, FALSE), got)
})


#-------------------------------------------------------------------------------


## %q%
test_that("metadata can be queried with %q%", {
  expect_false(c(Organism = "Bacillus simplex") %q% OPM.1)
  expect_true(c(Organism = "Bacillus simplex") %q% OPM.WITH.MD)
  # factors should be recognized
  x <- OPM.WITH.MD
  metadata(x, "Organism") <- as.factor(metadata(x, "Organism"))
  expect_true(c(Organism = "Bacillus simplex") %q% x)
  expect_true(list(Organism = "Bacillus simplex") %q% x)
  # character vectors
  expect_true(character() %q% OPM.WITH.MD)
  expect_false(c(Organism = "Bacillus subtilis") %q% OPM.WITH.MD)
  expect_false(list(Organism = "Bacillus subtilis") %q% OPM.WITH.MD)
  expect_false(c(`not there` = "missing") %q% OPM.WITH.MD)
  expect_false("missing" %q% OPM.WITH.MD)
  expect_false(list(`not there` = "missing") %q% OPM.WITH.MD)
})

## %q%
test_that("OPM metadata keys can be queried with %q% and a formula", {
  f <- ~ Organism == "Bacillus simplex"
  expect_error(f %q% OPM.1)
  expect_error(OPM.1 %q% f)
  expect_true(f %q% OPM.WITH.MD)
  expect_true(OPM.WITH.MD %q% f)
  f <- ~ Organism == "Bacillus subtilis"
  expect_error(f %q% OPM.1)
  expect_error(OPM.1 %q% f)
  expect_false(f %q% OPM.WITH.MD)
  expect_false(OPM.WITH.MD %q% f)
  # this should work but does not because of test_that():
  #Organism <- "Bacillus subtilis"
  #expect_true(f %q% OPM.1)
  #expect_true(OPM.1 %q% f)
  #expect_false(f %q% OPM.WITH.MD)
  #expect_false(OPM.WITH.MD %q% f)
})

## %q%
test_that("OPMS metadata values can be queried with %q%", {
  expect_equal(c(TRUE, TRUE), c(organism = ORGN) %q% OPMS.INPUT)
  expect_equal(c(FALSE, TRUE), c(organism = ORGN, run = 3L) %q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), c(missing = "not there") %q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), c("not there") %q% OPMS.INPUT)
})


#-------------------------------------------------------------------------------


## %Q%
test_that("OPM metadata can be queried with %Q%", {
  expect_false(c(Organism = "Bacillus simplex") %Q% OPM.1)
  expect_true(c(Organism = "Bacillus simplex") %Q% OPM.WITH.MD)
  # Factors should not be recognized in strict mode
  x <- OPM.WITH.MD
  metadata(x, "Organism") <- as.factor(metadata(x, "Organism"))
  expect_false(c(Organism = "Bacillus simplex") %Q% x)
  expect_false(list(Organism = "Bacillus simplex") %Q% x)
})

## %Q%
test_that("OPM metadata keys can be queried with %Q% and a formula", {
  f <- ~ Organism == "Bacillus simplex"
  expect_error(f %Q% OPM.1)
  expect_error(OPM.1 %Q% f)
  expect_true(f %Q% OPM.WITH.MD)
  expect_true(OPM.WITH.MD %Q% f)
  f <- ~ Organism == "Bacillus subtilis"
  expect_error(f %Q% OPM.1)
  expect_error(OPM.1 %Q% f)
  expect_false(f %Q% OPM.WITH.MD)
  expect_false(OPM.WITH.MD %Q% f)
  Organism <- "Bacillus subtilis"
  expect_error(f %Q% OPM.1)
  expect_error(OPM.1 %Q% f)
  expect_false(f %Q% OPM.WITH.MD)
  expect_false(OPM.WITH.MD %Q% f)
})

## %Q%
test_that("OPMS metadata can be queried with %Q%", {
  expect_equal(c(TRUE, TRUE), list(organism = ORGN) %Q% OPMS.INPUT)
  expect_equal(c(FALSE, TRUE), list(organism = ORGN, run = 3L) %Q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(missing = "not there") %Q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list("not there") %Q% OPMS.INPUT)
})


################################################################################

