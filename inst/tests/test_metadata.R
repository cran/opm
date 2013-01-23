

library(testthat)
context("Testing the functions of the OPM package that deal with metadata")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## include_metadata
test_that("metadata can be included and CSV keys removed", {
  x <- include_metadata(OPM.1, MD, remove.keys = FALSE)
  expect_is(x, "OPM")
  expect_is(md <- metadata(x), "list")
  expect_equal(length(md), 4L)
  expect_true(all(c("Position", "Setup Time") %in% names(md)))
  x <- include_metadata(OPM.1, MD)
  expect_is(x, "OPM")
  expect_is(md <- metadata(x), "list")
  expect_equal(length(md), 2L)
  expect_true(setequal(names(md), c("Organism", "File")))
  expect_equal(md$Organism, ORGN)
  bad.md <- MD
  bad.md$Position[1L] <- "does not exist"
  expect_error(include_metadata(OPM.1, bad.md))
})


################################################################################


## metadata<-
test_that("metadata can be added individually", {
  x <- OPM.WITH.MD
  got <- metadata(x) <- list(A = 99)
  expect_equal(metadata(x), list(A = 99))
  expect_equal(got, list(A = 99))
  got <- metadata(x, "B") <- 40
  expect_equal(got, 40)
  expect_equal(metadata(x), list(A = 99, B = 40))
  got <- metadata(x, list(C = "K")) <- list(67)
  expect_equal(got, list(67))
  expect_equal(metadata(x), list(A = 99, B = 40, K = 67))
  got <- metadata(x, list("H", "I")) <- list(I = 9, H = "f")
  expect_equal(got, list(I = 9, H = "f"))
  expect_equal(metadata(x), list(A = 99, B = 40, K = 67, H = "f", I = 9))
  got <- metadata(x, c("A", 'Z')) <- -99
  expect_equal(metadata(x, "A"), c(99, Z = -99))
  expect_equal(got, -99)
})

## metadata<-
test_that("the metadata can be modified by setting them", {
  x <- OPMS.INPUT
  wanted <- list(a = 3, b = 7)
  md <- metadata(x) <- wanted
  expect_equal(md, wanted)
  got <- metadata(x)
  expect_equal(got[[1L]], wanted)
  expect_equal(got[[2L]], wanted)
  e.coli <- "E. coli"
  e.coli.list <- list(organism = e.coli)
  md <- metadata(x, "organism") <- e.coli
  expect_equal(md, e.coli)
  got <- metadata(x)
  expect_equal(got[[1L]], c(wanted, e.coli.list))
  expect_equal(got[[2L]], c(wanted, e.coli.list))
  md <- metadata(x, "organism") <- NULL
  expect_equal(md, NULL)
  got <- metadata(x)
  expect_equal(got[[1L]], wanted)
  expect_equal(got[[2L]], wanted)
})


################################################################################


## map_metadata
test_that("metadata can be mapped using a character vector", {
  map <- structure("Elephas maximus", names = ORGN)
  x <- map_metadata(OPM.WITH.MD, map)
  expect_equal(metadata(x)$Organism, "Elephas maximus")
})

## map_metadata
test_that("metadata can be mapped using a function", {
  map <- identity
  data <- map_metadata(OPM.WITH.MD, map)
  expect_equal(metadata(data), metadata(OPM.WITH.MD))
  data <- map_metadata(OPM.WITH.MD, map, values = FALSE)
  expect_equal(metadata(data), metadata(OPM.WITH.MD))
  # a mapping function that really changes something
  map <- function(x) rep('x', length(x))
  data <- map_metadata(OPM.WITH.MD, map)
  expect_equal(metadata(data), list(File = 'x', Organism = 'x'))
  # Modify only the selected classes
  map <- function(y) rep('y', length(y))
  data <- map_metadata(data, map, classes = "integer")
  expect_equal(metadata(data), list(File = 'y', Organism = 'y'))
  data <- map_metadata(data, map, classes = "character")
  expect_equal(metadata(data), list(File = 'y', Organism = 'y'))
  # And now the keys
  data <- map_metadata(OPM.WITH.MD, map, values = FALSE)
  expect_equal(names(metadata(data)), rep("y", 2L))
})


## map_metadata
test_that("the metadata can be modified using a mapping function", {
  got <- map_metadata(OPMS.INPUT, mapping = identity)
  expect_equal(got, OPMS.INPUT)
  got <- metadata(map_metadata(OPMS.INPUT, mapping = function(x) "x"))
  expect_equal(got, list(list(run = "x", organism = "x"),
    list(run = "x", organism = "x")))
  got <- metadata(map_metadata(OPMS.INPUT, mapping = function(x) "x",
    classes = "character"))
  expect_equal(got, list(list(run = 4L, organism = "x"),
    list(run = 3L, organism = "x")))
})


################################################################################


## metadata_chars
test_that("metadata characters can be received", {
  got <- metadata_chars(OPM.WITH.MD)
  exp <- sort(c(ORGN, INFILES[1L]))
  names(exp) <- exp
  expect_equal(got, exp)
  x <- OPM.WITH.MD
  metadata(x, "run") <- 4L
  got <- metadata_chars(x)
  expect_equal(got, exp)
  got <- metadata_chars(x, classes = "integer")
  exp <- sort(c(structure(4L, names = 4L), exp))
  expect_equal(got, exp)
  got <- metadata_chars(x, classes = "not.relevant", values = FALSE)
  exp <- sort(names(metadata(x)))
  names(exp) <- exp
  expect_equal(got, exp)
})

## metadata_chars
test_that("the metadata characters can be queried", {
  chars <- metadata_chars(OPMS.INPUT)
  expect_equal(chars, structure(ORGN, names = ORGN))
  chars <- metadata_chars(OPMS.INPUT, classes = "integer")
  expect_equal(chars, structure(c(3L, 4L, ORGN), names = c(3L, 4L, ORGN)))
  chars <- metadata_chars(OPMS.INPUT, values = FALSE)
  expect_equal(chars, structure(c("organism", "run"),
    names = c("organism", "run")))
})


################################################################################

