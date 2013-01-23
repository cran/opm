

library(testthat)
context("Testing the aggregation functions of the OPM package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## to_grofit_time
## UNTESTED


## to_grofit_data
## UNTESTED


## extract_curve_params
## UNTESTED


## param_names
test_that("param_names() is consistent with other settings", {
  expect_true(OPM_OPTIONS$curve.param %in% c(param_names(), "disc"))
})


################################################################################


## do_aggr
test_that("OPM objects can be aggregated using the fast method", {

  fast.agg <- do_aggr(SMALL, method = "opm-fast")

  expect_is(SMALL, "OPM")
  expect_false(is(SMALL, "OPMA"))
  expect_is(fast.agg, "OPM")
  expect_is(fast.agg, "OPMA")
  expect_false(has_aggr(SMALL))
  expect_true(has_aggr(fast.agg))
  expect_false(has_disc(SMALL))
  expect_false(has_disc(fast.agg))

  expect_equal(csv_data(fast.agg), csv_data(SMALL))
  expect_equal(metadata(fast.agg), metadata(SMALL))

  settings <- aggr_settings(fast.agg)
  expect_is(settings, "list")
  expect_equal(length(settings), 4L)
  expect_false(is.null(names(settings)))

  expect_equal(colnames(aggregated(SMALL.AGG)), colnames(aggregated(fast.agg)))
  expect_equal(rownames(aggregated(SMALL.AGG)), rownames(aggregated(fast.agg)))

  x <- aggregated(fast.agg)
  expect_is(x, "matrix")
  expect_equal(dim(x), c(12L, 10L))
  expect_equal(colnames(x), wells(fast.agg))
  expect_true(any(is.na(x)))
  x <- x[!grepl("^(mu|lambda)", rownames(x), perl = TRUE), ]
  expect_false(any(is.na(x)))

  x <- aggregated(fast.agg, subset = "mu")
  expect_is(x, "matrix")
  expect_equal(colnames(x), wells(fast.agg))
  expect_equal(dim(x), c(3L, 10L))

  x <- aggregated(fast.agg, subset = c("mu", "AUC"), ci = FALSE)
  expect_is(x, "matrix")
  expect_equal(colnames(x), wells(fast.agg))

})


## do_aggr
test_that("OPMS objects can be aggregated using the fast method", {

  fast.agg <- do_aggr(thin_out(OPMS.INPUT, 10), method = "opm-fast")

  expect_is(OPMS.INPUT, "OPMS")
  expect_is(fast.agg, "OPMS")
  expect_false(any(has_aggr(OPMS.INPUT)))
  expect_true(all(has_aggr(fast.agg)))
  expect_equal(metadata(OPMS.INPUT), metadata(fast.agg))

  agg.got <- aggregated(fast.agg)
  expect_is(agg.got, "list")
  expect_equal(length(agg.got), length(fast.agg))
  expect_true(all(sapply(agg.got, is.matrix)))
  other.agg <- aggregated(SMALL.AGG)
  for (agg in agg.got) {
    expect_equal(colnames(agg)[1:10], colnames(other.agg))
    expect_equal(rownames(agg), rownames(other.agg))
  }

})


## do_aggr
test_that("matrices can be aggregated", {

  x <- matrix(1:10, ncol = 2L)
  colnames(x) <- LETTERS[1:2]
  rownames(x) <- letters[1:5]

  groups <- list(Y = c("a", "a", "b", "a", "b"))
  got <- do_aggr(x, groups, mean)
  expect_is(got, "matrix")
  expect_equal(rownames(got), levels(as.factor(groups$Y)))
  expect_equal(colnames(got), colnames(x))

  groups <- c(groups, list(Z = rep("z", 5L)))
  got <- do_aggr(x, groups, mean)
  expect_is(got, "matrix")
  exp.rn <- paste(levels(as.factor(groups$Y)), levels(as.factor(groups$Z)),
    sep = ".")
  expect_equal(rownames(got), exp.rn)
  expect_equal(colnames(got), colnames(x))

  got <- do_aggr(x, groups, mean, sep = "||")
  exp.rn <- paste(levels(as.factor(groups$Y)), levels(as.factor(groups$Z)),
    sep = "||")
  expect_equal(rownames(got), exp.rn)
  expect_equal(colnames(got), colnames(x))

})


################################################################################


## pe_and_ci
## UNTESTED


## fast_estimate
## UNTESTED


################################################################################

