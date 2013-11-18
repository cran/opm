

library(testthat)
context("Testing the well mapping of the OPM package for consistency")


## WELL_MAP
test_that("substrate names are ok", {
  expect_false(any(grepl("\\sacid$", WELL_MAP, FALSE, TRUE)))
  expect_false(any(grepl(" - ", WELL_MAP, FALSE, TRUE)))
  expect_false(any(grepl("[^',()A-Za-z0-9 #%./+-]", WELL_MAP, FALSE, TRUE)))
  expect_false(is.unsorted(rownames(WELL_MAP))) # wells should be sorted
  expect_false(is.unsorted(colnames(WELL_MAP))) # plates should be sorted
})


## WELL_MAP
test_that("substrate IDs in well map can query substrate information", {
  all.ids <- seq.int(nrow(SUBSTRATE_INFO))
  expect_true(setequal(WELL_MAP[, , "substrate_id"], all.ids))
})

