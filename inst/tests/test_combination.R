

library(testthat)
context("Testing the OPM(S) combination functions of the opm package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## to_opm_list
## UNTESTED

## try_opms
## UNTESTED

## [<-
test_that("MOPMX objects are restricted (bracket operator)", {
  x <- new("MOPMX")
  expect_equal(length(x), 0L)
  expect_error(x[1] <- 1)
  expect_equal(length(x), 0L)
  expect_error(x[1] <- "a")
  expect_equal(length(x), 0L)
  expect_is(x, "MOPMX")
  x[TRUE] <- NULL
  expect_equal(length(x), 0L)
  x["A"] <- SMALL
  expect_equal(length(x), 1L)
  expect_equal(names(x), "A")
  expect_equal(x$A, SMALL)
  expect_equal(x[["A"]], SMALL)
  expect_error(x[2] <- 1)
  expect_equal(length(x), 1L)
  expect_equal(names(x), "A")
  expect_equal(x$A, SMALL)
  expect_equal(x[["A"]], SMALL)
  x["B"] <- SMALL
  expect_equal(length(x), 2L)
  expect_equal(names(x), c("A", "B"))
  x["B"] <- SMALL
  #print(names(x))
  expect_equal(length(x), 2L)
  expect_equal(names(x), c("A", "B"))
  x["B"] <- NULL
  expect_equal(length(x), 1L)
  expect_equal(names(x), "A")
  expect_warning(x[3:4] <- SMALL)
  expect_equal(length(x), 3L)
  x[c(TRUE, FALSE, TRUE)] <- NULL
  expect_equal(length(x), 1L)
})

## [[<-
test_that("MOPMX objects are restricted (double bracket operator)", {
  x <- new("MOPMX")
  expect_equal(length(x), 0L)
  expect_error(x[["A"]] <- 1)
  expect_equal(length(x), 0L)
  expect_error(x[[2]] <- "a")
  expect_equal(length(x), 0L)
  x[[TRUE]] <- NULL
  expect_equal(length(x), 0L)
  x[["A"]] <- SMALL
  expect_equal(length(x), 1L)
  expect_equal(names(x), "A")
  x[["B"]] <- SMALL
  expect_equal(length(x), 2L)
  expect_equal(names(x), c("A", "B"))
  x[["B"]] <- SMALL
  expect_equal(length(x), 2L)
  expect_equal(names(x), c("A", "B"))
  x[["B"]] <- NULL
  expect_equal(length(x), 1L)
  expect_warning(x[[3]] <- SMALL)
  expect_equal(length(x), 2L)
  x[[3]] <- SMALL.WITH.MD
  expect_equal(length(x), 3L)
  x[[TRUE]] <- NULL
  expect_equal(length(x), 2L)
})

## $<-
test_that("MOPMX objects are restricted (dollar operator)", {
  x <- new("MOPMX")
  expect_equal(length(x), 0L)
  expect_error(x$A <- 1)
  expect_equal(length(x), 0L)
  expect_error(x$A <- "a")
  expect_equal(length(x), 0L)
  x$A <- NULL
  expect_equal(length(x), 0L)
  x$A <- SMALL
  expect_equal(length(x), 1L)
  x$B <- SMALL.WITH.MD
  expect_equal(length(x), 2L)
})

## +
test_that("`+`() can be used to put plates together", {
  x <- OPM.1 + OPM.2
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 2L)
  x <- x + OPM.2
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 3L)
  expect_error(x <- x + OPM.3)
  expect_error(x <- x + 5)
  expect_error(x <- x + "abc")
  x <- OPM.1 + OPM.1
  expect_equal(2L, dim(x)[1L])
  y <- x + OPM.1
  expect_equal(3L, dim(y)[1L])
  y <- x + list(OPM.1)
  expect_equal(3L, dim(y)[1L])
  y <- x + list(OPM.1, OPM.2)
  expect_equal(4L, dim(y)[1L])
  y <- x + x
  expect_equal(4L, dim(y)[1L])
})


## +
test_that("`+`() can be used to put MOPMX objects together", {
  got <- MOPMX.1 + OPM.1
  expect_equal(length(got), length(MOPMX.1) + 1L)
  got <- OPM.1 + MOPMX.1
  expect_equal(length(got), length(MOPMX.1) + 1L)
  got <- OPMS.INPUT + MOPMX.1
  expect_equal(length(got), length(MOPMX.1) + 1L)
  got <- MOPMX.1 + OPMS.INPUT
  expect_equal(length(got), length(MOPMX.1) + 1L)
  expect_error(MOPMX.1 + 5)
  expect_error(5 + MOPMX.1)
})


## c
test_that("c() can be used to put plates together", {
  x <- c(OPM.1)
  expect_is(x, "OPM")
  expect_equal(x, OPM.1)
  x <- c(OPM.1, 55L)
  expect_is(x, "list")
  expect_equal(length(x), 2L)
  x <- c(OPM.1, 55L, "abc")
  expect_is(x, "list")
  expect_equal(length(x), 3L)
  x <- c(OPM.1, OPM.2)
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 2L)
  x <- c(x, OPM.2)
  expect_is(x, "OPMS")
  expect_equal(dim(x)[1L], 3L)
  x <- c(x, 55L)
  expect_is(x, "list")
  expect_equal(length(x), 2L)
  x <- c(x, 55L, "abc")
  expect_is(x, "list")
  expect_equal(length(x), 4L)
})


## c
test_that("MOPMX objects can be combined with c()", {
  x <- new("MOPMX")
  expect_is(x, "MOPMX")
  got <- c(x, list(NULL), recursive = TRUE)
  expect_is(got, "MOPMX")
  got <- c(x, NULL)
  expect_is(got, "MOPMX")
  got <- c(x, list(NULL), recursive = FALSE)
  expect_false(is(got, "MOPMX"))
  got <- c(x, list(a = letters))
  expect_false(is(got, "MOPMX"))
  x <- c(x, SMALL, NULL, x)
  expect_is(x, "MOPMX")
  expect_equal(length(x), 1L)
})


## opms
test_that("opms() can be used to put plates together", {

  # single plate
  expect_is(opms(OPM.1), "OPM")

  # uniform plate types
  x <- list(a = OPM.1, b = OPM.2)
  x.opms <- opms(x)
  expect_is(x.opms, "OPMS")
  expect_equal(2L, length(x.opms))
  expect_equal(NULL, names(plates(x.opms)))

  # distinct plate types
  expect_error(opms(OPM.1, OPM.3),
    "plate types are not uniform: PM01 <=> PM20")

  # distinct plate types, automatically grouped
  grp <- opms(OPM.1, OPM.2, OPM.3, group = TRUE)
  expect_is(grp, "list")
  expect_equal(length(grp), 2L)
  expect_equal(length(grp[[1L]]), 2L)
  expect_equal(names(grp), c("PM01", "PM20"))

  # distinct plate types, automatically selected
  grp <- opms(OPM.1, OPM.2, OPM.3, group = "PM01")
  expect_is(grp, "OPMS")
  expect_equal(length(grp), 2L)
  expect_equal(plate_type(grp), "PM01")

})


## opms
test_that("opms() is robust with zero input", {
  for (precomputed in c(TRUE, FALSE))
    for (skip in c(TRUE, FALSE)) {
      x <- opms(precomputed = precomputed, skip = skip, group = FALSE)
      expect_is(x, "NULL")
      x <- opms(precomputed = precomputed, skip = skip, group = TRUE)
      expect_is(x, "MOPMX")
      expect_equal(length(x), 0L)
    }
})


################################################################################

