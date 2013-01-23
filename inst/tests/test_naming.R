

library(testthat)
context("Testing the opm naming functions")


################################################################################


## map_grofit_names
test_that("curve parameter names can be mapped", {
  x <- map_grofit_names()
  expect_true(all(CURVE_PARAMS %in% unlist(x)))
  y <- map_grofit_names(opm.fast = TRUE)
  expect_true(!any(names(y) %in% names(x)))
  expect_equivalent(x, y)
  x <- map_grofit_names(plain = TRUE)
  expect_true(setequal(unlist(x), param_names()))
})


## map_well_names
## UNTESTED


## to_sentence
## UNTESTED


## listing
## UNTESTED


################################################################################


## well_to_substrate
test_that("substrate names can be translated", {

  plate.1 <- "PM01"
  exp.1 <- c(A01 = "Negative Control", A02 = "L-Arabinose")
  got <- well_to_substrate(plate.1, c("A01", "A02"))

  plates.2 <- c(plate.1, "PM02")
  exp.2 <- c(A01 = "Negative Control", A02 = "Chondroitin Sulfate C")
  exp.2 <- cbind(exp.1, exp.2)
  colnames(exp.2) <- plates.2
  got <- well_to_substrate(plates.2, c("A01", "A02"))
  expect_equal(got, exp.2)

  # Partial matching is allowed
  plates.2 <- c(plate.1, "PM02")
  exp.2 <- c(A01 = "Negative Control", A02 = "Chondroitin Sulfate C")
  exp.2 <- cbind(exp.1, exp.2)
  colnames(exp.2) <- c(plates.2[1L], "PM02")
  got <- well_to_substrate(plates.2, c("A01", "A02"))
  expect_equal(got, exp.2)

})


################################################################################


## find_substrate
test_that("substrate names can be searched", {

  found <- find_substrate("Fructose", search = "exact")
  expect_is(found, "list")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate"), found[[1L]])

  found <- find_substrate("Fructose", search = "approx")
  expect_is(found, "list")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate", "D-Fucose",
    "L-Fucose"), found[[1L]])

})


## find_substrate
test_that("substrate names can be searched with patterns", {

  glob.pat <- c("ampic*", "penic*", "random*")
  found <- find_substrate(glob.pat, search = "glob")
  expect_is(found, "list")
  expect_equal(3L, length(found))
  expect_equal(glob.pat, names(found))
  expect_equal("Ampicillin", found[[1L]])
  expect_equal("Penicillin G", found[[2L]])
  expect_equal(character(), found[[3L]])

  reg.pat <- c("^ampic.*", "^penic.*", "^random.*")
  found.2 <- find_substrate(reg.pat, search = "regex")
  expect_equal(reg.pat, names(found.2))
  names(found.2) <- glob.pat
  expect_equal(found, found.2)

})


################################################################################


## find_positions
test_that("positions within PM plates can be found", {

  query <- c("D-Fructose", "Penicillin G", "This is not present")
  got <- find_positions(query)
  expect_is(got, "list")
  expect_equal(query, names(got))
  expect_true(all(sapply(got, is.matrix)))
  expect_true(all(dim(got[[1L]] > 0L)))
  expect_true(all(dim(got[[2L]] > 0L)))
  expect_true(all(dim(got[[3L]] == 0L)))

  query <- find_substrate("Fructose", search = "exact")
  got <- find_positions(query)
  expect_is(got, "list")
  expect_equal(1L, length(got))
  expect_equal("Fructose", names(got))
  got <- got[[1L]]
  expect_is(got, "list")
  expect_equal(query[[1L]], names(got))
  expect_true(all(sapply(got, is.matrix)))

})


################################################################################


## substrate_info
## UNTESTED


################################################################################

