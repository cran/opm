

library(testthat)
context("Testing the functions of the OPM package for discretizing characters")


################################################################################


## best_cutoff
## UNTESTED


## discrete
test_that("characters can be discretized to strings in non-gap mode", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8))
  expect_equal(length(x), length(y))
  expect_is(y, "character")
  expect_equal("0", y[1L])
  expect_equal("V", y[length(y)])

  y <- discrete(x, c(0, 10))
  expect_equal(length(x), length(y))
  expect_true("0" < y[1L])
  expect_true("V" > y[length(y)])

  y <- discrete(x, c(2, 8), states = 10L)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("9", y[length(y)])

  y <- discrete(x, c(2, 8), states = 1)
  expect_equal(length(x), length(y))
  expect_equal(rep("0", length(y)), y)

})

## discrete
test_that("characters can be discretized with the real range", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, TRUE)
  expect_equal(length(x), length(y))
  expect_is(y, "character")
  expect_equal("0", y[1L])
  expect_equal("V", y[length(y)])

  y <- discrete(x, TRUE, gap = TRUE)
  expect_equal(length(x), length(y))
  expect_is(y, "character")
  expect_equal("0", y[1L])
  expect_equal("1", y[length(y)])
  expect_equal("?", y[length(y) / 2L])
  expect_equal(length(counts <- table(y)), 3L)
  expect_true(all(counts >= 4L))

})

## discrete
test_that("characters can be discretized to non-strings in non-gap mode", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8), output = "integer")
  expect_equal(length(x), length(y))
  expect_is(y, "integer")
  expect_equal(1L, y[1L])
  expect_equal(32L, y[length(y)])

  y <- discrete(x, c(2, 8), output = "logical")
  expect_equal(length(x), length(y))
  expect_is(y, "logical")
  expect_equal(FALSE, y[1L])
  expect_equal(TRUE, y[length(y)])

  y <- discrete(x, c(2, 8), output = "factor")
  expect_equal(length(x), length(y))
  expect_is(y, "factor")
  expect_equal("1", as.character(y[1L]))
  expect_equal("32", as.character(y[length(y)]))

  y <- discrete(x, c(2, 8), output = "numeric")
  expect_equal(x, y)

})

## discrete
test_that("characters can be gap-discretized to binary characters (strings)", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8), TRUE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("?", y[2L])
  expect_equal("?", y[length(y) - 1L])
  expect_equal("1", y[length(y)])

  y <- discrete(x, c(3, 7), TRUE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("0", y[2L])
  expect_equal("1", y[length(y) - 1L])
  expect_equal("1", y[length(y)])

  expect_error(y <- discrete(x, c(0, 10), TRUE))

  y <- discrete(x, c(2, 8), TRUE, states = 1L:10L)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("1", y[length(y)])
  z <- discrete(x, c(2, 8), TRUE, states = 10L)
  expect_equal(y, z)
  z <- discrete(x, c(2, 8), TRUE, states = as.character(0L:9L))
  expect_equal(y, z)

  y <- discrete(x, c(2, 8), TRUE, states = 3L:12L)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("1", y[length(y)])

})

## discrete
test_that(
  "characters can be gap-discretized to binary characters (non-strings)", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8), TRUE, output = "integer")
  expect_equal(length(x), length(y))
  expect_equal(0L, y[1L])
  expect_equal(1L, y[length(y)])

  y <- discrete(x, c(2, 8), TRUE, output = "logical")
  expect_equal(length(x), length(y))
  expect_equal(FALSE, y[1L])
  expect_equal(TRUE, y[length(y)])

  y <- discrete(x, c(2, 8), TRUE, output = "factor")
  expect_equal(length(x), length(y))
  expect_is(y, "factor")
  expect_equal("0", as.character(y[1L]))
  expect_equal("1", as.character(y[length(y)]))

  y <- discrete(x, c(2, 8), output = "numeric")
  expect_equal(x, y)

})

## discrete
test_that("characters can be gap-discretized to ternary characters (strings)", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8), TRUE, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("1", y[2L])
  expect_equal("1", y[length(y) - 1L])
  expect_equal("2", y[length(y)])

  y <- discrete(x, c(3, 7), TRUE, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("0", y[2L])
  expect_equal("2", y[length(y) - 1L])
  expect_equal("2", y[length(y)])

  expect_error(y <- discrete(x, c(0, 10), TRUE, middle.na = FALSE))

  y <- discrete(x, c(2, 8), TRUE, states = 1L:10L, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("2", y[length(y)])
  z <- discrete(x, c(2, 8), TRUE, states = 10L, middle.na = FALSE)
  expect_equal(y, z)
  z <- discrete(x, c(2, 8), TRUE, states = as.character(0L:9L),
    middle.na = FALSE)
  expect_equal(y, z)

  y <- discrete(x, c(2, 8), TRUE, states = 3L:12L, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("2", y[length(y)])

})

## discrete
test_that(
    "characters can be gap-discretized to ternary characters (non-strings)", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8), TRUE, output = "integer", middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal(0L, y[1L])
  expect_equal(2L, y[length(y)])

  expect_error(y <- discrete(x, c(2, 8), TRUE, output = "logical",
    middle.na = FALSE))

  y <- discrete(x, c(2, 8), TRUE, output = "factor", middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_is(y, "factor")
  expect_equal("0", as.character(y[1L]))
  expect_equal("2", as.character(y[length(y)]))

  y <- discrete(x, c(2, 8), output = "numeric", middle.na = FALSE)
  expect_equal(x, y)

})

## discrete
test_that("a matrix of characters can be discretized", {

  x <- matrix(seq(2, 8, 0.5)[-1L], ncol = 4L)
  rownames(x) <- letters[1L:3L]
  colnames(x) <- LETTERS[1L:4L]

  y <- discrete(x, c(2, 8), gap = FALSE)
  expect_is(y[1L], "character")
  expect_equal(dim(x), dim(y))
  expect_equal(rownames(x), rownames(y))
  expect_equal(colnames(x), colnames(y))

  y <- discrete(x, c(3, 3), gap = TRUE)
  expect_is(y[1L], "character")
  expect_equal(dim(x), dim(y))
  expect_equal(rownames(x), rownames(y))
  expect_equal(colnames(x), colnames(y))

  y <- discrete(x, c(3, 3), gap = TRUE, middle.na = FALSE)
  expect_is(y[1L], "character")
  expect_equal(dim(x), dim(y))
  expect_equal(rownames(x), rownames(y))
  expect_equal(colnames(x), colnames(y))

})


## do_disc
## UNTESTED


################################################################################


