

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


## include_metadata
test_that("metadata can be included in MOPMX objects", {
  expect_false("A" %in% metadata_chars(MOPMX.1, value = FALSE))
  tpl <- collect_template(MOPMX.1)
  tpl$A <- 1:3
  x <- include_metadata(MOPMX.1, tpl)
  expect_true("A" %in% metadata_chars(x, value = FALSE))
})


## map_values
## UNTESTED

## edit
## UNTESTED


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


## metadata<-
test_that("metadata of MOPMX objects can be set", {
  x <- MOPMX.1
  metadata(x) <- data.frame(A = 1:3)
  got <- metadata_chars(x, value = FALSE)
  expect_equal(got, structure("A", names = "A"))
  x <- MOPMX.1
  metadata(x, -1) <- data.frame(A = 1:3)
  got <- metadata_chars(x, value = FALSE)
  expect_true(length(got) > 1L && "A" %in% got)
  metadata(x) <- list(X = "Y")
  got <- metadata_chars(x)
  expect_equal(got, structure("Y", names = "Y"))
  got <- metadata_chars(x, value = FALSE)
  expect_equal(got, structure("X", names = "X"))
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


## map_metadata
test_that("metadata within MOPMX objects can be mapped", {
  got <- map_metadata(MOPMX.1)
  expect_equal(got, MOPMX.1)
  got <- map_metadata(MOPMX.1, c(`Bacillus simplex` = "E. coli"))
  expect_false(identical(got, MOPMX.1))
  got <- metadata_chars(got)
  expect_true("E. coli" %in% got)
  expect_false("Bacillus simplex" %in% got)
})


################################################################################


## metadata
test_that("missing metadata result in an error if requested", {
  expect_is(OPM.1, "OPM")
  expect_equal(metadata(OPM.1), list())
  expect_equal(metadata(OPM.1, "Organism"), NULL)
  expect_error(metadata(OPM.1, "Organism", strict = TRUE))
})

## metadata
test_that("metadata have be included in example OPM object", {
  expect_is(OPM.WITH.MD, "OPM")
  exp.list <- list(File = csv_data(OPM.1, what = "filename"), Organism = ORGN)
  expect_equal(metadata(OPM.WITH.MD), exp.list)
  expect_equal(metadata(OPM.WITH.MD, "Organism"), ORGN)
  expect_equal(metadata(OPM.WITH.MD, list("File", "Organism")), exp.list)
  exp.list$Organism <- NULL
  exp.list$Org <- ORGN
  expect_equal(metadata(OPM.WITH.MD, list("File", "Org"), exact = FALSE),
    exp.list)
  exp.list$Org <- NULL
  exp.list <- c(exp.list, list(Org = NULL))
  expect_equal(metadata(OPM.WITH.MD, list("File", "Org"), exact = TRUE),
    exp.list)
})

## metadata
test_that("the OPMS metadata can be queried", {
  md.got <- metadata(OPMS.INPUT)
  expect_is(md.got, "list")
  expect_equal(length(md.got), length(OPMS.INPUT))
  expect_true(all(vapply(md.got, is.list, NA)))
  md.got <- metadata(OPMS.INPUT, "organism")
  expect_is(md.got, "character")
  expect_equal(length(md.got), length(OPMS.INPUT))
  md.got <- metadata(OPMS.INPUT, list("not.there"))
  expect_is(md.got, "list")
  expect_true(all(vapply(md.got, is.list, NA)))
  expect_true(all(vapply(md.got, names, "") == "not.there"))
  expect_true(all(vapply(md.got, function(x) is.null(x$not.there), NA)))
  expect_error(md.got <- metadata(OPMS.INPUT, list("not.there"), strict = TRUE))
  expect_equal(metadata(OPMS.INPUT), metadata(THIN.AGG))
})


## metadata
test_that("the MOPMX metadata can be queried", {
  got <- metadata(MOPMX.1)
  expect_is(got, "list")
  expect_equal(length(got), length(MOPMX.1))
  got <- metadata(MOPMX.1, "organism")
  expect_is(got, "list")
  expect_equal(length(got), length(MOPMX.1))
  expect_error(got <- metadata(MOPMX.1, "organism", strict = TRUE))
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


## metadata_chars
test_that("the MOPMX metadata characters can be queried", {
  got <- metadata_chars(MOPMX.1)
  expect_equal(got, structure(ORGN, names = ORGN))
  got.2 <- metadata_chars(MOPMX.1, classes = "integer")
  expect_true(length(got.2) > length(got))
})


################################################################################


