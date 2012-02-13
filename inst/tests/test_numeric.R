
library(testthat)


context("Test the numeric helper functions of the OPM package.")


rand_range <- function(n, minstart = 0, maxstart = 100, maxrange = 100) {
  start <- runif(1L, min = minstart, max = maxstart)
  width <- runif(1L, min = 0, max = maxrange)
  runif(n, start, start + width)
}


test_that("the improved maximum can be calculated", {
  for (i in 1:100) {
    nums <- rand_range(10L)
    im <- improved_max(nums)
    im.5 <- improved_max(nums, 5)
    im.20 <- improved_max(nums, 20)
    expect_true(im.20 > im && im > im.5 && im.5 > max(nums))
  }
})


test_that("optimal ranges can be determined", {
  x <- 1:10
  expect_error(best_range(x, target = 8.9))
  expect_equal(c(0.5, 10.5), best_range(x, target = 10))
  expect_equal(c(0, 11), best_range(x, target = 10, offset = 0.5))
  expect_equal(c(1, 11), best_range(x, target = 10, align = "left"))
  expect_equal(c(0.5, 11.5), best_range(x, target = 10, align = "left",
    offset = 0.5))
  expect_equal(c(0, 10), best_range(x, target = 10, align = "right"))
  expect_equal(c(-0.5, 10.5), best_range(x, target = 10, align = "right",
    offset = 0.5))
  expect_equal(c(1, 10), best_range(x, target = NULL))
  expect_equal(c(0.5, 10.5), best_range(x, target = NULL, offset = 0.5))
})


test_that("cex can be guessed", {
  x <- 1:100
  got <- guess_cex(x)
  expect_equal(length(got), length(x))
  expect_equivalent(-1, cor.test(x, got, method = "spearman")$estimate)
  expect_warning(guess_cex(-1))
  expect_equal(Inf, guess_cex(0))
})


test_that("best layouts can be determined", {
  x <- 0:100
  got <- lapply(x, best_layout)
  prods <- sapply(got, Reduce, f = `*`)
  expect_true(all(prods >= x))
  expect_false(all(prods > x))
  expect_true(all(sapply(got, length) == 2L))
  expect_true(all(sapply(got, function(a) a[1] >= a[2])))
  expect_error(best_layout(-1))
})


test_that("best ranges can be determined", {
  for (i in 1:100) {
    # real range
    real.range <- range(nums <- rand_range(10L))
    got.range <- range(got <- best_range(nums, NULL))
    expect_true(isTRUE(all.equal(real.range, got.range)))
    # larger range
    large.diff <- real.range[2L] - real.range[1L] + 1
    got.range <- range(got <- best_range(nums, large.diff))
    expect_true(real.range[1L] > got.range[1L])
    expect_true(real.range[2L] < got.range[2L])
    # with offset
    got.range <- range(got <- best_range(nums, NULL, offset = 1))
    expect_true(real.range[1L] > got.range[1L])
    expect_true(real.range[2L] < got.range[2L])
    # with proportional offset
    got.range <- range(got <- best_range(nums, NULL, prop.offset = 0.1))
    expect_true(real.range[1L] > got.range[1L])
    expect_true(real.range[2L] < got.range[2L])
  }
})


