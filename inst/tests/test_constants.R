library(testthat)


context("Test the constants of the OPM package.")


mixed_names_and_values <- function(x) {
  diff <- setdiff(x, names(x))
  length(diff) >= length(x)
}


################################################################################


test_that("plate names agree", {
  expect_false(mixed_names_and_values(PLATE_MAP))
  expect_equal(names(PLATE_MAP), colnames(WELL_MAP))
  expect_true(GEN_III %in% names(PLATE_MAP))
})


test_that("substrate names are ok", {
  expect_false(any(grepl("\\sacid$", WELL_MAP, perl = TRUE)))
  expect_false(any(grepl(" - ", WELL_MAP, perl = TRUE)))
  expect_false(any(grepl("[^',()A-Za-z0-9 %./+-]", WELL_MAP, perl = TRUE)))
})




