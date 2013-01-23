

library(testthat)
context("Testing the plotting functions of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


rand_range <- function(n, minstart = 0, maxstart = 100, maxrange = 100) {
  start <- runif(1L, min = minstart, max = maxstart)
  width <- runif(1L, min = 0, max = maxrange)
  runif(n, start, start + width)
}


################################################################################


## show
## UNTESTED

## print
## UNTESTED


################################################################################


## ranging
test_that("ranging can be applied", {

  x <- rand_range(10)
  got <- ranging(x)
  expect_equal(max(got), 1)
  expect_equal(min(got), 0)

  x <- rand_range(10)
  got <- ranging(x, fac = 3.5)
  expect_equal(max(got), 3.5)
  expect_equal(min(got), 0)

  x <- rand_range(10)
  got <- ranging(x, extended = FALSE)
  expect_equal(max(got), 1)
  expect_true(min(got) > 0)

  x <- rand_range(10)
  got <- ranging(x, extended = FALSE, fac = 6.1)
  expect_equal(max(got), 6.1)
  expect_true(min(got) > 0)

})


## guess_cex
test_that("cex can be guessed", {
  x <- 1:100
  got <- guess_cex(x)
  expect_equal(length(got), length(x))
  expect_equivalent(-1, cor.test(x, got, method = "spearman")$estimate)
  expect_warning(guess_cex(-1))
  expect_equal(Inf, guess_cex(0))
})

## best_layout
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

## best_range
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

## best_range
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

## improved_max
test_that("the improved maximum can be calculated", {
  for (i in 1:100) {
    nums <- rand_range(10L)
    im <- improved_max(nums)
    im.5 <- improved_max(nums, 5)
    im.20 <- improved_max(nums, 20)
    expect_true(im.20 > im && im > im.5 && im.5 > max(nums))
  }
})


## draw_ci
## UNTESTED


## main_title
test_that("the OPMX function main_title() can be applied to OPMS objects", {
  mt.got <- main_title(OPMS.INPUT, list())
  expect_is(mt.got, "character")
  expect_equal(length(mt.got), 1L)
})


## negative_control
test_that("
    the OPMX function negative_control() can be applied to OPMS objects", {
  nc.got <- negative_control(OPMS.INPUT, neg.ctrl = TRUE)
  expect_is(nc.got, "numeric")
  expect_equal(length(nc.got), 1L)
})


## select_colors
test_that("predefined color sets can be obtained", {
  for (arg in as.character(formals(select_colors)[[1L]])[-1L]) {
    got <- select_colors(arg)
    expect_is(got, "character")
    expect_true(length(got) >= 10L)
  }
})


## try_select_colors
## UNTESTED


## default_color_regions
## UNTESTED


## xy_plot
test_that("the OPMS methpd xy_plot() works", {
  opms.input <- OPMS.INPUT[, 1L:10L]
  expect_is(opms.input, "OPMS")
  got <- xy_plot(opms.input)
  expect_is(got, "trellis")
  got <- xy_plot(opms.input, include = "organism")
  expect_is(got, "trellis")
  expect_error(got <- xy_plot(opms.input, include = "doesnotexist"))
})


## level_plot
test_that("the OPMS methpd level_plot() works", {
  opms.input <- OPMS.INPUT[, 1L:10L]
  expect_is(opms.input, "OPMS")
  got <- level_plot(opms.input)
  expect_is(got, "trellis")
})


## ci_plot
test_that("a tie-fighter (CI) plot can be drawn", {
  legend <- ci_plot(THIN.AGG[, , 1:12], as.labels = list("organism", "run"),
    subset = "A", na.action = "ignore")
  expect_equal(c("1: Bacillus simplex 3", "2: Bacillus simplex 4"), legend)
  legend <- ci_plot(THIN.AGG[, , 1:6], as.labels = list("organism"),
    subset = "A", na.action = "ignore", legend.field = NULL, bg = "lightgrey",
    x = "bottom")
  expect_equal(c("1: Bacillus simplex", "2: Bacillus simplex"), legend)
})


## heat_map
test_that("a heatmap can be drawn", {

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "A", as.groups = list("organism"))
  mat.2 <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "A", as.groups = list("organism"), dataframe = TRUE)

  hm <- heat_map(mat, margins = c(5, 5), use.fun = "stats")
  expect_is(hm, "list")
  expect_equal(NULL, hm$colColMap)
  expect_equal(names(hm$rowColMap), metadata(THIN.AGG, "organism"))

  # Data frame version
  hm.2 <- heat_map(mat.2, as.labels = c("organism", "run"),
    as.groups = "organism", margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)

  # Distance given as function or list
  hm.2 <- heat_map(mat, distfun = dist, margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)
  hm.2 <- heat_map(mat, distfun = list(method = "euclidean"),
    margins = c(5, 5), use.fun = "stats")
  expect_equal(hm.2, hm)

  # Clustering function given in distinct ways
  hm <- heat_map(mat, hclustfun = hclust, margins = c(5, 5), use.fun = "stats")
  expect_false(identical(hm.2, hm))
  hm.2 <- heat_map(mat, hclustfun = "complete", margins = c(5, 5),
    use.fun = "stats")
  expect_equal(hm, hm.2)
  hm.2 <- heat_map(mat, hclustfun = list(method = "complete"),
    margins = c(5, 5), use.fun = "stats")
  expect_equal(hm, hm.2)

  # Column groups
  group_fun <- function(x) substr(x, 1, 1)
  hm <- heat_map(mat, margins = c(5, 5), c.groups = group_fun)
  groups <- group_fun(colnames(mat))
  hm.2 <- heat_map(mat, margins = c(5, 5), c.groups = groups)
  expect_equal(hm, hm.2)
  expect_equivalent(groups, names(hm$colColMap))

})


## radial_plot
test_that("a radial plot can be drawn", {
  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "A", as.groups = list("organism"))
  got <- radial_plot(mat)
  expected <- structure(select_colors()[1L:2L],
    names = c("Bacillus simplex 4", "Bacillus simplex 3"))
  expect_equal(got, expected)
})


