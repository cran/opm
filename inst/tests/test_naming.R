

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


## clean_coords
test_that("well names can be cleaned", {
  x <- c("  Z07\t", "D\n11\r", " A06  ", " B7")
  got <- clean_coords(x)
  expect_equal(got, c("Z07", "D11", "A06", "B07"))
})


## well_index
test_that("well indices given as formula can be mapped", {
  expect_equal(well_index(1:10), 1:10)
  expect_equal(well_index(), TRUE)
  got <- well_index(letters)
  expect_is(got, "character")
  expect_equal(length(got), 26L)
  expect_equal(toupper(got), got)
  expect_error(well_index(NULL ~ a:c))
  expect_equal(well_index(NULL ~ a:c, letters), 1:3)
  expect_equal(well_index(NULL ~ c(b, e:f), letters), c(2, 5, 6))
  expect_error(well_index(NULL ~ c(b, e:f), LETTERS))
  expect_equal(well_index(NULL ~ c(B, E:F), LETTERS), c(2, 5, 6))
})


## map_well_names
## UNTESTED


## to_sentence
## UNTESTED


## listing
## UNTESTED


################################################################################


## well_to_substrate
## UNTESTED


## find_substrate
test_that("substrate names can be searched", {

  found <- find_substrate(c(wanted = "Fructose"), search = "exact")
  expect_is(found, "substrate_match")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate"), found[[1L]])

  found <- find_substrate(c(wanted = "Fructose"), search = "approx")
  expect_is(found, "substrate_match")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate", "D-Fucose",
    "L-Fucose"), found[[1L]])

})


## find_substrate
test_that("substrate names can be searched with patterns", {

  glob.pat <- c(A = "ampic*", B = "penic*", C = "random*")
  found <- find_substrate(glob.pat, search = "glob")
  expect_is(found, "substrate_match")
  expect_equal(3L, length(found))
  expect_equivalent(glob.pat, names(found))
  expect_equal("Ampicillin", found[[1L]])
  expect_equal("Penicillin G", found[[2L]])
  expect_equal(character(), found[[3L]])

  reg.pat <- c(A = "^ampic.*", B = "^penic.*", C = "^random.*")
  found.2 <- find_substrate(reg.pat, search = "regex")
  expect_equivalent(reg.pat, names(found.2))
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
test_that("greek letters can be expanded", {
  x <- c("A01 (a-D-Fructose)", "Penicillin G", "b-L-Glucose")
  wanted <- c("A01 (alpha-D-Fructose)", "Penicillin G", "beta-L-Glucose")
  got <- substrate_info(x, "greek")
  expect_equivalent(got, wanted)
  wanted <- c("A01 (&alpha;-D-Fructose)", "Penicillin G", "&beta;-L-Glucose")
  got <- substrate_info(x, "html")
  expect_equivalent(got, wanted)
})


################################################################################


