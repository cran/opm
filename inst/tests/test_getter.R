

library(testthat)
context("Testing the getter functions of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## measurements
test_that("all measurements are accessible", {
  m.got <- measurements(OPMS.INPUT)
  expect_is(m.got, "list")
  expect_true(all(vapply(m.got, is.matrix, NA)))
  m.got <- measurements(OPMS.INPUT, 3L)
  expect_is(m.got, "list")
  expect_true(all(vapply(m.got, is.matrix, NA)))
  m.got <- measurements(MOPMX.1, 3L)
  expect_is(m.got, "list")
  expect_equal(length(m.got), length(MOPMX.1))
})


## well
test_that("the content of the wells can be subset", {
  w.got <- well(OPMS.INPUT)
  expect_is(w.got, "list")
  expect_true(all(vapply(w.got, is.matrix, NA)))
  expect_true(all(!vapply(w.got, function(x) is.null(rownames(x)), NA)))
  w.got <- well(OPMS.INPUT, 3L)
  expect_is(w.got, "matrix")
  expect_equal(nrow(w.got), length(OPMS.INPUT))
  w.got <- well(OPMS.INPUT, 3L:4L)
  expect_is(w.got, "list")
  expect_true(all(vapply(w.got, is.matrix, NA)))
})

## well
test_that("the content of the wells can be obtained from MOPMX objects", {
  w.got <- well(MOPMX.1)
  expect_is(w.got, "list")
  expect_false(all(vapply(w.got, is.matrix, NA)))
  expect_true(any(vapply(w.got, is.matrix, NA)))
  expect_false(all(vapply(w.got, is.list, NA)))
  expect_true(any(vapply(w.got, is.list, NA)))
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
  h.got <- hours(MOPMX.1, what = "all")
  expect_is(h.got, "list")
  expect_is(h.got[[1L]], "numeric")
  expect_is(h.got[[2L]], "matrix")
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
test_that("the entire OPMS object can be subset", {
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

## [
test_that("the entire MOPMX object can be subset", {
  x <- MOPMX.1[]
  expect_equal(x, MOPMX.1)
  x <- MOPMX.1[1L:2L]
  expect_equal(x, MOPMX.1)
  x <- MOPMX.1[1L]
  expect_is(x, "MOPMX")
  x <- MOPMX.1[2L]
  expect_is(x, "MOPMX")
  expect_warning(x <- MOPMX.1[10L])
  expect_is(x, "MOPMX")
})


################################################################################


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


################################################################################


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
  expect_equivalent(picked,
    c(csv_data(OPM.1, what = "filename"), csv_data(OPM.1, what = "setup_time")))
  missing.key <- "19825761285616"
  error.msg <- paste("could not find key", missing.key)
  expect_error(csv_data(OPM.1, c("File", missing.key)), error.msg)
  # OPMS method
  cd.got <- csv_data(OPMS.INPUT)
  expect_is(cd.got, "matrix")
  expect_equal(dim(cd.got), c(2L, 10L))
})


## csv_data
test_that("filename of example object can be explicitely queried", {
  expect_equal(csv_data(OPM.1, what = "filename"), INFILES[1L])
})


## csv_data
test_that("setup times can be explicitely queried", {
  expect_equal(csv_data(OPM.1, what = "setup_time"), "8/30/2010 11:28:54 AM")
  st.got <- csv_data(OPMS.INPUT, what = "setup_time")
  expect_is(st.got, "character")
  expect_equal(length(st.got), length(OPMS.INPUT))
})


## csv_data
test_that("plate positions can be explicitely queried", {
  expect_equal(csv_data(OPM.1, what = "position"), "21-B")
  p.got <- csv_data(OPMS.INPUT, what = "position")
  expect_is(p.got, "character")
  expect_equal(length(p.got), length(OPMS.INPUT))
})


## csv_data
test_that("CSV data of MOPMX objects can be accessed", {
  got <- csv_data(MOPMX.1)
  expect_is(got, "matrix")
  expect_equal(nrow(got), length(plates(MOPMX.1)))
  expect_true(all(got[1L, ] == csv_data(MOPMX.1[[1L]])))
  expect_true(all(got[-1L, ] == csv_data(MOPMX.1[[2L]])))

  got.2 <- csv_data(MOPMX.1, normalize = TRUE)
  expect_equal(dim(got.2), dim(got))
  expect_false(all(got == got.2))

  got <- csv_data(MOPMX.1, what = "position")
  expect_is(got, "character")
  expect_equal(length(got), length(plates(MOPMX.1)))

  got.2 <- csv_data(MOPMX.1, what = "position", normalize = TRUE)
  expect_equal(got, got.2) # positions were already normalised
})



################################################################################


## has_aggr
test_that("information on presense of aggregated values can be obtained", {
  expect_false(has_aggr(OPM.1))
  ha.got <- has_aggr(OPMS.INPUT)
  expect_is(ha.got, "logical")
  expect_equal(length(ha.got), length(OPMS.INPUT))
  expect_false(any(ha.got))
  expect_true(all(has_aggr(THIN.AGG)))
  expect_false(has_aggr(SMALL))
  expect_true(has_aggr(SMALL.AGG))
  got <- has_aggr(MOPMX.1)
  expect_is(got, "list")
  expect_equal(length(got), length(MOPMX.1))
  expect_false(any(vapply(got, any, NA)))
})


## has_disc
test_that("information on presense of discretised values can be obtained", {
  got <- has_disc(MOPMX.1)
  expect_is(got, "list")
  expect_equal(length(got), length(MOPMX.1))
  expect_false(any(vapply(got, any, NA)))
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
  expect_true(all(vapply(ag.got, is.matrix, NA)))
})

## aggregated
test_that("aggregated data in MOPMX objects can be queried", {
  expect_error(aggregated(MOPMX.1))
  got <- aggregated(new(MOPMX))
  expect_equal(length(got), 0L)
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

## aggr_settings
test_that("aggregation settings in MOPMX objects can be queried", {
  expect_error(aggr_settings(MOPMX.1))
  got <- aggr_settings(new(MOPMX))
  expect_equal(length(got), 0L)
})


################################################################################


## discretized
test_that("discretised data in MOPMX objects can be queried", {
  expect_error(discretized(MOPMX.1))
  got <- discretized(new(MOPMX))
  expect_equal(length(got), 0L)
})


## disc_settings
test_that("discretisation settings in MOPMX objects can be queried", {
  expect_error(disc_settings(MOPMX.1))
  got <- disc_settings(new(MOPMX))
  expect_equal(length(got), 0L)
})


################################################################################


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
test_that("an OPM object can be subset based on the metadata", {
  query <- list(Organism = ORGN)
  other.query <- list(Organism = "Elephas maximums") # wrong value
  third.query <- list(organism = ORGN) # wrong key
  got <- subset(OPM.WITH.MD, query = query, values = TRUE)
  expect_is(got, OPM)
  got <- subset(OPM.WITH.MD, query = query, use = "q")
  expect_is(got, OPM)
  got <- subset(OPM.WITH.MD, query = query, values = FALSE)
  expect_is(got, OPM)
  got <- subset(OPM.WITH.MD, query = query, use = "k")
  expect_is(got, OPM)
  got <- subset(OPM.WITH.MD, query = other.query, values = TRUE)
  expect_is(got, "NULL")
  got <- subset(OPM.WITH.MD, query = other.query, use = "q")
  expect_is(got, "NULL")
  got <- subset(OPM.WITH.MD, query = other.query, values = FALSE)
  expect_is(got, OPM)
  got <- subset(OPM.WITH.MD, query = other.query, use = "k")
  expect_is(got, OPM)
  got <- subset(OPM.WITH.MD, query = third.query, values = FALSE)
  expect_is(got, "NULL")
  got <- subset(OPM.WITH.MD, query = third.query, use = "k")
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
  got <- subset(OPM.1, time = TRUE)
  expect_equal(got, OPM.1)
})

## subset
test_that("MOPMX objects can be subset", {
  got <- subset(MOPMX.1, time = TRUE)
  expect_equal(got, MOPMX.1)
  expect_warning(got <- subset(MOPMX.1, ~ organism, use = "k"))
  expect_equal(got, MOPMX.1[2L])
})


## thin_out
test_that("OPM example data can be thinned out", {
  expect_error(thin_out(OPM.1, 0.5), "'factor' must be >= 1")
  thin <- thin_out(OPM.1, 1)
  expect_equal(OPM.1, thin)
  thin <- thin_out(OPM.1, 2)
  dims <- dim(thin)
  dims[1L] <- dims[1] * 2
  expect_equal(dims, dim(OPM.1))
})

## thin_out
test_that("OPMS example data can be thinned out", {
  dims <- dim(OPMS.INPUT)
  dims[2L] <- floor(dims[2L] / 10)
  thin <- thin_out(OPMS.INPUT, 10)
  expect_equal(dim(thin), dims)
  expect_equal(metadata(thin), metadata(OPMS.INPUT))
})

## thin_out
test_that("MOPMX example data can be thinned out", {
  thin <- thin_out(MOPMX.1, 10)
  expect_is(thin, MOPMX)
  expect_equal(length(thin), length(MOPMX.1))
  expect_false(identical(thin, MOPMX.1))
})


################################################################################


## duplicated
test_that("MOPMX objects can be checked for duplicates #2", {
  expect_equal(duplicated(MOPMX.1), c(FALSE, FALSE))
  x <- MOPMX.1 + OPM.3
  expect_equal(duplicated(x), c(FALSE, FALSE, TRUE))
})


## anyDuplicated
test_that("MOPMX objects can be checked for duplicates #2", {
  expect_equal(anyDuplicated(MOPMX.1), 0L)
  x <- MOPMX.1 + OPM.3
  expect_equal(anyDuplicated(x), 3L)
})

## contains
test_that("MOPMX objects can be queried with contains()", {
  expect_true(contains(MOPMX.1, OPM.3))
  expect_false(contains(MOPMX.1, OPM.1))
  expect_equal(contains(MOPMX.1, MOPMX.1), c(TRUE, TRUE))
})


################################################################################


## %k%
test_that("OPM metadata can be queried with %k%", {
  expect_false("Organism" %k% OPM.1)
  expect_true("Organism" %k% OPM.WITH.MD)
  expect_true(c("Organism", "File") %k% OPM.WITH.MD)
  expect_false("not there" %k% OPM.WITH.MD)
  expect_true(list(Organism = "dummy", File = "dummy") %k% OPM.WITH.MD)
  expect_false(list(`not there` = "dummy") %k% OPM.WITH.MD)
})

## %k%
test_that("OPM metadata can be queried with %k% and a formula", {
  f <- ignored ~ Organism == "Bacillus simplex"
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
test_that("OPMS metadata can be queried with %k%", {
  expect_equal(c(TRUE, TRUE), "organism" %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), c("organism", "run") %k% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), "not there" %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), list(organism = "dummy") %k% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE),
    list(organism = "dummy", run = "dummy") %k% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(`not there` = "missing") %k% OPMS.INPUT)
})

## %k%
test_that("OPMS metadata can be queried with %k% and a formula/expression", {
  for (f in list(junk ~ organism, expression(organism))) {
    got <- f %k% OPMS.INPUT
    expect_equal(c(TRUE, TRUE), got)
    got <- OPMS.INPUT %k% f
    expect_equal(c(TRUE, TRUE), got)
  }
  # symbols are there, operations fails
  for (f in list(junk ~ organism + run, expression(organism + run))) {
    got <- f %k% OPMS.INPUT
    expect_equal(c(FALSE, FALSE), got)
    got <- OPMS.INPUT %k% f
    expect_equal(c(FALSE, FALSE), got)
  }
  # symbols are there, operations succeeds
  for (f in list(junk ~ c(organism, run), expression(c(organism, run)))) {
    f <- ~ c(organism, run)
    got <- f %k% OPMS.INPUT
    expect_equal(c(TRUE, TRUE), got)
    got <- OPMS.INPUT %k% f
    expect_equal(c(TRUE, TRUE), got)
  }
})

## %k%
test_that("OPMS metadata can be queried with %k% and formula/expression #2", {
  for (f in list(ignored ~ not.there, expression(not.there))) {
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
  }
})


## %k%
test_that("OPMS metadata can be queried with %k% and formula #3", {
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


## %k%
test_that("MOPMX metadata can be queried with %k%", {
  got <- MOPMX.1 %k% ~ organism
  expect_equal(got, list(FALSE, c(TRUE, TRUE)))
})


#-------------------------------------------------------------------------------


## %K%
test_that("OPM metadata can be queried with %K%", {
  expect_false("Organism" %K% OPM.1)
  expect_false("Organism" %K% OPM.1)
  expect_true("Organism" %K% OPM.WITH.MD)
  expect_false(c("Organism", "File") %K% OPM.WITH.MD)
  expect_false("not there" %K% OPM.WITH.MD)
  expect_true(list(Organism = "dummy", File = "dummy") %K% OPM.WITH.MD)
  expect_false(list(`not there` = "dummy") %K% OPM.WITH.MD)
})

## %K%
test_that("OPM metadata can be queried with %K% and formula/expression", {
  for (f in list(ignored ~ Organism == "Bacillus simplex",
      expression(Organism == "Bacillus simplex"))) {
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
  }
})

## %K%
test_that("OPMS metadata can be queried with %K%", {
  expect_equal(c(TRUE, TRUE), "organism" %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), c("organism", "run") %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), "not there" %K% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE), list(organism = "dummy") %K% OPMS.INPUT)
  expect_equal(c(TRUE, TRUE),
    list(organism = "dummy", run = "dummy") %K% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(`not there` = missing) %K% OPMS.INPUT)
})

## %K%
test_that("OPMS metadata can be queried with %K% and a formula", {
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
test_that("OPMS metadata can be queried with %K% and formula/expression #2", {
  for (f in list(junk ~ not.there, expression(not.there))) {
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
  }
})


## %K%
test_that("OPMS metadata can be queried with %K% and formula #3", {
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


## %K%
test_that("MOPMX metadata can be queried with %K%", {
  got <- MOPMX.1 %K% ~ organism
  expect_equal(got, list(FALSE, c(TRUE, TRUE)))
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
test_that("OPM metadata can be queried with %q% and formula/expression", {
  for (f in list(junk ~ Organism == "Bacillus simplex",
      expression(Organism == "Bacillus simplex"))) {
    expect_error(f %q% OPM.1)
    expect_error(OPM.1 %q% f)
    expect_true(f %q% OPM.WITH.MD)
    expect_true(OPM.WITH.MD %q% f)
  }
  for (f in list(junk ~ Organism == "Bacillus subtilis",
      expression(Organism == "Bacillus subtilis"))) {
    expect_error(f %q% OPM.1)
    expect_error(OPM.1 %q% f)
    expect_false(f %q% OPM.WITH.MD)
    expect_false(OPM.WITH.MD %q% f)
  }
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


## %q%
test_that("MOPMX metadata can be queried with %q%", {
  got <- MOPMX.1[2] %q% ~ organism == "Bacillus simplex"
  expect_equal(got, list(c(TRUE, TRUE)))
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
test_that("OPM metadata can be queried with %Q% and formula/expression", {
  for (f in list(ignored ~ Organism == "Bacillus simplex",
      expression(Organism == "Bacillus simplex"))) {
    expect_error(f %Q% OPM.1)
    expect_error(OPM.1 %Q% f)
    expect_true(f %Q% OPM.WITH.MD)
    expect_true(OPM.WITH.MD %Q% f)
  }
  for (f in list(ignored ~ Organism == "Bacillus subtilis",
      expression(Organism == "Bacillus subtilis"))) {
    expect_error(f %Q% OPM.1)
    expect_error(OPM.1 %Q% f)
    expect_false(f %Q% OPM.WITH.MD)
    expect_false(OPM.WITH.MD %Q% f)
  }
})

## %Q%
test_that("OPMS metadata can be queried with %Q%", {
  expect_equal(c(TRUE, TRUE), list(organism = ORGN) %Q% OPMS.INPUT)
  expect_equal(c(FALSE, TRUE), list(organism = ORGN, run = 3L) %Q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list(missing = "not there") %Q% OPMS.INPUT)
  expect_equal(c(FALSE, FALSE), list("not there") %Q% OPMS.INPUT)
})

## %Q%
test_that("MOPMX metadata can be queried with %Q%", {
  got <- MOPMX.1[2] %Q% ~ organism == "Bacillus simplex"
  expect_equal(got, list(c(TRUE, TRUE)))
})



################################################################################

