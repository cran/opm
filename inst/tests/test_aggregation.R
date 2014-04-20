

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


## summary
## UNTESTED


## pe_and_ci
## UNTESTED


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
  expect_true(all(vapply(agg.got, is.matrix, NA)))
  other.agg <- aggregated(SMALL.AGG)
  for (agg in agg.got) {
    expect_equal(colnames(agg)[1:10], colnames(other.agg))
    expect_equal(rownames(agg), rownames(other.agg))
  }

})


## do_aggr
test_that("MOPMX object can be agregated using the fast method", {
  junk <- capture.output(got <- do_aggr(MOPMX.1, method = "opm-fast"))
  expect_true(all(unlist(has_aggr(got))))
  expect_false(any(unlist(has_disc(got))))
  got <- do_disc(got, TRUE)
  expect_true(any(unlist(has_disc(got))))
})


################################################################################

