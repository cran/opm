

library(testthat)
context("Testing the classes of the opm package and their conversion functions")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## update_settings_list
## UNTESTED


## opm_problems (this actually only tests inheritance relationships)
test_that("OPMS has all method of OPM/OPMA/OPMD", {
  m <- tryCatch(as.character(getGenerics("package:opm")),
    error = function(e) character())
  if (length(m)) {
    opm.methods <- m[
      vapply(m, existsMethod, NA, "OPMD") |
      vapply(m, existsMethod, NA, "OPMA") |
      vapply(m, existsMethod, NA, "OPM") |
      vapply(m, existsMethod, NA, "WMD")
    ]
    opms.methods <- m[vapply(m, existsMethod, NA, "OPMS")]
    expect_equal(character(), setdiff(opm.methods, opms.methods))
    expect_true(length(setdiff(opms.methods, opm.methods)) > 0)
  }
})


## attach_attr
## UNTESTED


## rename_wells
## UNTESTED


## opma_problems
## UNTESTED


## opmd_problems
## UNTESTED


## as
test_that("the OPM/OPMA example data can be converted to a list and back", {

  # Converting to list and back
  opm.list <- as(OPM.1, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPM")
  expect_equal(OPM.1, opm.back)

  # Converting with metadata to list and back
  opm.list <- as(OPM.WITH.MD, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPM")
  expect_equal(OPM.WITH.MD, opm.back)

  # Converting aggregated stuff to list and back
  opm.list <- as(SMALL.AGG, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPMA")
  expect_equal(SMALL.AGG, opm.back)

  # With distortion of ordering and addition of nonsense
  change <- 1L:5L
  expect_true("aggregated" %in% names(opm.list))
  expect_true(all(vapply(opm.list$aggregated, is.list, NA)))
  opm.list$aggregated <- c(opm.list$aggregated[-change],
    opm.list$aggregated[change])
  opm.list$aggregated[change] <- lapply(opm.list$aggregated[change], rev)
  opm.list$aggregated[-change] <- lapply(opm.list$aggregated[-change], c, Z = 7)
  opm.back <- as(opm.list, "OPMA")
  expect_equal(SMALL.AGG, opm.back)

})


################################################################################


## opms_problems
test_that("new() can be used to put plates together", {
  x <- list(a = OPM.1, b = OPM.2)
  x.opms <- new("OPMS", plates = x)
  expect_is(x.opms, "OPMS")
  expect_equal(length(x.opms), 2L)
  expect_equal(NULL, names(plates(x.opms)))
})


## as
test_that("the OPMS example data can be converted to a list and back", {

  # Converting to list and back
  opms.list <- as(OPMS.INPUT, "list")
  expect_is(opms.list, "list")
  opms.back <- as(opms.list, "OPMS")
  expect_equal(OPMS.INPUT, opms.back)

  # Converting with aggregated data to list and back
  opms.list <- as(THIN.AGG, "list")
  expect_is(opms.list, "list")
  opms.back <- as(opms.list, "OPMS")
  expect_equal(THIN.AGG, opms.back)

})

## as
test_that("the OPM example data can be converted to a list and back", {
  # conversion of a list to an OPM object is tolerant against re-orderings
  # (but not against additions and omissions)
  x <- as(SMALL, "list")
  x$measurements <- c(rev(x$measurements[7:8]), rev(x$measurements[-7:-8]))
  x <- as(x, "OPM")
  stopifnot(identical(measurements(x), measurements(SMALL)))
})

## as
test_that("the OPMA example data can be converted to a list and back", {
  # conversion of a list to an OPMA object is tolerant against re-orderings
  # and additions (but not against omissions)
  x <- as(SMALL.AGG, "list")
  x$aggregated <- c(Answer = 42L, rev(x$aggregated), Text = LETTERS)
  x <- as(x, "OPMA")
  stopifnot(identical(aggregated(x), aggregated(SMALL.AGG)))
})

## as
test_that("OPMD objects can be converted to a list and back", {
  # conversion of a list to an OPMA object is tolerant against re-orderings
  # and additions (but not against omissions)
  d <- do_disc(SMALL.AGG, TRUE)
  x <- as(d, "list")
  x$discretized <- c(Answer = 42L, rev(x$discretized), Text = LETTERS)
  x <- as(x, "OPMD")
  stopifnot(identical(discretized(x), discretized(d)))
})



################################################################################


## initialize
test_that("the example objects have the correct classes", {
  expect_is(OPMS.INPUT, "OPMS")
  expect_is(THIN.AGG, "OPMS")
  expect_is(SMALL, "OPM")
  expect_false(is(SMALL, "OPMA"))
  expect_is(SMALL.AGG, "OPM")
  expect_is(SMALL.AGG, "OPMA")
})


## initialize
test_that("MOPMX objects are correctly created", {
  expect_error(x <- new("MOPMX", list(A = NULL, B = SMALL)))
  x <- new("MOPMX", list(B = SMALL, THIN.AGG))
  expect_equal(names(x), c("B", ""))
})



################################################################################

