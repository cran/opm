

library(testthat)
context("Testing the well mapping of the OPM package for consistency")


## WELL_MAP
test_that("substrate names are ok", {
  expect_false(any(grepl("\\sacid$", WELL_MAP, perl = TRUE)))
  expect_false(any(grepl(" - ", WELL_MAP, perl = TRUE)))
  expect_false(any(grepl("[^',()A-Za-z0-9 %./+-]", WELL_MAP, perl = TRUE)))
  expect_false(is.unsorted(rownames(WELL_MAP))) # wells should be sorted
})


## WELL_MAP
test_that("substrate names fit together in ID map and well map", {
  expect_true(all(WELL_MAP %in% rownames(SUBSTRATE_INFO)))
})

