

library(testthat)
context("Testing the constants of the OPM package for consistency")


expect_length <- function(actual, expected) {
  expect_equal(length(actual), expected)
}


################################################################################


## NOT_YET
## UNTESTED

## BUG_MSG
## UNTESTED


################################################################################


## WMD
## UNTESTED

## OPM
## UNTESTED

## OPMA
## UNTESTED

## OPMD
## UNTESTED

## OPMS
## UNTESTED

## OPMX
## UNTESTED

## YAML_VIA_LIST
## UNTESTED

## FOE
## UNTESTED

## MOA
## UNTESTED

## CMAT
## UNTESTED


################################################################################


## SEALED
## UNTESTED


################################################################################


## CSV_NAMES
test_that("the internally used CSV names are OK", {
  expect_is(CSV_NAMES, "character")
  expect_false(is.null(names(CSV_NAMES)))
})


## SPECIAL_PLATES
test_that("the so-called special plates are known plates", {
  expect_true(all(SPECIAL_PLATES %in% names(PLATE_MAP)))
  expect_false(is.null(names(SPECIAL_PLATES)))
})


## SP_PATTERN
## UNTESTED (see plate_type())


## THEOR_RANGE
test_that("the theoretical range is OK", {
  expect_length(THEOR_RANGE, 2L)
  expect_false(is.unsorted(THEOR_RANGE))
})


## HOUR
## UNTESTED


## CURVE_PARAMS
test_that("the internally used parameter names are OK", {
  # this strange test is intended to ensure that one thinks twice before
  # changing the constant
  expect_equal(CURVE_PARAMS, c("mu", "lambda", "A", "AUC"))
})


## RESERVED_NAMES
test_that("the internally used reserved metadata names are OK", {
  # this strange test is intended to ensure that one thinks twice before
  # changing the constant
  wanted <- c("plate", "well", "time", "value", "parameter")
  expect_equal(names(RESERVED_NAMES), wanted)
})


## SOFTWARE
## UNTESTED


## VERSION
## UNTESTED


## UNKNOWN_VERSION
## UNTESTED


## PROGRAM
## UNTESTED


## METHOD
## UNTESTED


## OPTIONS
## UNTESTED


## KNOWN_METHODS
test_that("the known method names are OK", {
  expect_is(KNOWN_METHODS, "list")
  expect_true(all(vapply(KNOWN_METHODS, is.character, logical(1L))))
  expect_true("aggregation" %in% names(KNOWN_METHODS))
  expect_true("discretization" %in% names(KNOWN_METHODS))
})


## GRAPHICS_FORMAT_MAP
## UNTESTED


## HTML_DOCTYPE
## UNTESTED


## URL_BASE
test_that("bases of URLs contain no special characters", {
  got <- vapply(URL_BASE, URLencode, "")
  expect_equivalent(got, URL_BASE)
})


################################################################################


## W3C_COLORS
## UNTESTED


## MEMOIZED
## UNTESTED


## OPM_OPTIONS
## UNTESTED


################################################################################


## CHARACTER_STATES
test_that("character-state symbols are correctly defined", {
  expect_is(CHARACTER_STATES, "character")
  expect_length(CHARACTER_STATES, 32L)
  expect_true(all(nchar(CHARACTER_STATES) == 1L))
})


## MISSING_CHAR
test_that("the missing character symbol is correctly defined", {
  expect_is(MISSING_CHAR, "character")
  expect_length(MISSING_CHAR, 1L)
  expect_equal(nchar(MISSING_CHAR), 1L)
  expect_false(MISSING_CHAR %in% CHARACTER_STATES)
})


## PHYLO_FORMATS
test_that("phylogeny formats are defined", {
  expect_is(PHYLO_FORMATS, "character")
  expect_true(length(PHYLO_FORMATS) > 0L)
})


## GREEK_LETTERS
## UNTESTED


################################################################################

