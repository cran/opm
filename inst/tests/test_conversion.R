

library(testthat)
context("Testing the conversion methods of the opm package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## merge
test_that("plates can be merged", {
  x <- merge(OPMS.INPUT)
  expect_is(x, "OPM")
  d1 <- dim(OPMS.INPUT)
  expect_equal(dim(x),
    c(dim(OPMS.INPUT[1L])[1L] + dim(OPMS.INPUT[2L])[1L], d1[3L]))
  expect_warning(y <- opms(OPMS.INPUT[1L], OPMS.INPUT[2L, 1L:13L, ]))
  x <- merge(y)
  expect_equal(dim(x), c(dim(y[1L])[1L] + dim(y[2L])[1L], d1[3L]))
})


## merge
test_that("character-matrix objects can be merged", {

  data <- matrix(1:10, ncol = 2L)
  rownames(data) <- paste("taxon", 1L:5L, sep = "_")
  colnames(data) <- paste("char", 1L:2L, sep = "_")
  data <- discrete(data, TRUE, TRUE)
  data <- as(data, CMAT)

  got <- merge(data, TRUE)
  expect_equal(got, data)

  got <- merge(data, FALSE)
  expect_equal(got, data)

  got <- merge(data, seq.int(nrow(data)))
  expect_equal(got, data)

  expect_error(got <- merge(data, seq.int(nrow(data))[-1L]))

  got <- merge(data, c(1, 2, 1, 2, 1))
  expect_equal(dim(got), c(2L, ncol(data)))

})


## plates
test_that("the plates can be obtained as a list", {
  pl <- plates(OPMS.INPUT)
  expect_is(pl, "list")
  expect_equal(length(pl), 2L)
  expect_true(all(sapply(pl, class) == "OPM"))
})


## oapply
## UNTESTED -- but see examples and tests for subsetting


## thin_out
test_that("OPM example data can be thinned out", {
  expect_error(thin_out(OPM.1, 0.5), "'factor' must be >= 1")
  thin <- thin_out(OPM.1, 1)
  expect_equal(OPM.1, thin)
  thin <- thin_out(OPM.1, 2)
  dims <- dim(thin)
  dims[1L] <- dims[1] * 2
  expect_equal(dims, dim(OPM.1))
})

## thin_out
test_that("OPMS example data can be thinned out", {
  dims <- dim(OPMS.INPUT)
  dims[2L] <- floor(dims[2L] / 10)
  thin <- thin_out(OPMS.INPUT, 10)
  expect_equal(dim(thin), dims)
  expect_equal(metadata(thin), metadata(OPMS.INPUT))
})


################################################################################


## gen_iii
test_that("the plate type can be changed to generation 3", {
  gen.3 <- gen_iii(OPM.1)
  expect_is(gen.3, "OPM")
  expect_equal(plate_type(gen.3), SPECIAL_PLATES[["gen.iii"]])
  expect_equal(metadata(gen.3), metadata(OPM.1))
  expect_equal(length(which(csv_data(gen.3) != csv_data(OPM.1))), 1L)
})

## gen_iii
test_that("the plate type can be changed to ecoplate", {
  eco <- gen_iii(OPM.1, "ECO")
  expect_is(eco, "OPM")
  expect_equal(plate_type(eco), SPECIAL_PLATES[["eco"]])
  expect_equal(metadata(eco), metadata(OPM.1))
  expect_equal(length(which(csv_data(eco) != csv_data(OPM.1))), 1L)
})

## gen_iii
test_that("the plate type of OPMS objects can be changed", {
  x <- gen_iii(OPMS.INPUT)
  expect_equal(class(x), class(OPMS.INPUT))
  expect_equal(dim(x), dim(OPMS.INPUT))
  expect_false(plate_type(x) == plate_type(OPMS.INPUT))
  x <- gen_iii(OPMS.INPUT, "Eco")
  expect_equal(class(x), class(OPMS.INPUT))
  expect_equal(dim(x), dim(OPMS.INPUT))
  expect_false(plate_type(x) == plate_type(OPMS.INPUT))
})


################################################################################


## flatten
test_that("example data can be flattened", {
  base.colnames <- c("Time", "Well", "Value")
  flat <- flatten(SMALL)
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), base.colnames)
  w <- wells(SMALL, full = TRUE)
  expect_equal(as.character(unique(flat$Well)), w)
  exp.len <- Reduce(`*`, dim(SMALL))
  expect_equal(exp.len, nrow(flat))
})

## flatten
test_that("example data with metadata can be flattened", {
  base.colnames <- c("Time", "Well", "Value")
  flat <- flatten(SMALL.WITH.MD, include = list("Organism"))
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("Organism", base.colnames))
  exp.len <- Reduce(`*`, dim(SMALL.WITH.MD))
  expect_equal(exp.len, nrow(flat))
  orgn <- as.character(unique(flat[, "Organism"]))
  expect_equal(orgn, ORGN)
})

## flatten
test_that("example data can be flattened with fixed entries", {
  base.colnames <- c("Time", "Well", "Value")
  flat <- flatten(SMALL.WITH.MD, fixed = list(A = 33, B = "zzz"))
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("A", "B", base.colnames))
  exp.len <- Reduce(`*`, dim(SMALL.WITH.MD))
  expect_equal(exp.len, nrow(flat))
  content.a <- unique(flat[, "A"])
  expect_equal(content.a, 33)
  content.b <- as.character(unique(flat[, "B"]))
  expect_equal(content.b, "zzz")
})

## flatten
test_that("OPMS objects can be flattened", {
  opms.input <- OPMS.INPUT[, 1L:10L]
  base.colnames <- c("Plate", "Time", "Well", "Value")
  flat <- flatten(opms.input)
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), base.colnames)
  exp.len <- Reduce(`*`, dim(opms.input))
  expect_equal(exp.len, nrow(flat)) # warning: depends on length of runs
  plate.nums <- unique(flat[, "Plate"])
  expect_equal(paste("Plate", 1:2), as.character(plate.nums))
})

## flatten
test_that("OPMS objects including metadata can be flattened", {
  opms.input <- OPMS.INPUT[, 1L:10L]
  base.colnames <- c("Plate", "Time", "Well", "Value")
  # Flatten with metadata no. 1
  flat <- flatten(opms.input, include = list("organism"))
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("organism", base.colnames))
  orgns <- unique(as.character(flat[, "organism"]))
  expect_equal(orgns, ORGN)
  # Flatten with metadata no. 2
  flat <- flatten(opms.input, include = list("organism", "run"))
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("organism", "run", base.colnames))
  runs <- unique(flat[, "run"])
  expect_equal(4:3, runs)
})


## flattened_to_factor
## UNTESTED


################################################################################


## extract_columns
## UNTESTED


## sort
## UNTESTED


## unique
## UNTESTED


## rep
## UNTESTED


## rev
## UNTESTED


################################################################################


## extract
test_that("aggregated parameters can be extracted as matrix", {

  rn <- paste(metadata(THIN.AGG, "organism"), metadata(THIN.AGG, "run"),
    sep = "||")
  gn <- paste(metadata(THIN.AGG, "run"), metadata(THIN.AGG, "organism"),
    sep = "||")

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), sep = "||")
  expect_is(mat, "matrix")
  expect_equal(dim(mat), c(2L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(NULL, attr(mat, "row.groups"))
  expect_equal(rn, rownames(mat))

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", as.groups = list("run", "organism"), sep = "||")
  expect_is(mat, "matrix")
  expect_equal(dim(mat), c(2L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(as.factor(gn), attr(mat, "row.groups"))
  expect_equal(rn, rownames(mat))

})


## extract
test_that("aggregated parameters can be extracted as matrix with CIs", {

  rn <- paste(metadata(THIN.AGG, "organism"), metadata(THIN.AGG, "run"),
    sep = "+++")
  gn <- paste(metadata(THIN.AGG, "run"), metadata(THIN.AGG, "organism"),
    sep = "+++")

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"), ci = TRUE,
    sep = "+++")
  expect_is(mat, "matrix")
  expect_is(mat[1L], "numeric")
  expect_equal(dim(mat), c(6L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(grepl(rn[1L], rownames(mat), fixed = TRUE), c(T, T, T, F, F, F))
  expect_equal(grepl(rn[2L], rownames(mat), fixed = TRUE), c(F, F, F, T, T, T))
  expect_equal(NULL, attr(mat, "row.groups"))

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "mu", ci = TRUE, as.groups = list("organism"), sep = "+++")
  expect_is(mat, "matrix")
  expect_is(mat[1L], "numeric")
  expect_equal(dim(mat), c(6L, 96L))
  expect_equal(colnames(mat), wells(THIN.AGG, full = TRUE))
  expect_equal(grepl(rn[1L], rownames(mat), fixed = TRUE), c(T, T, T, F, F, F))
  expect_equal(grepl(rn[2L], rownames(mat), fixed = TRUE), c(F, F, F, T, T, T))
  expect_equal(rep(as.factor(metadata(THIN.AGG, "organism")), each = 3L),
    attr(mat, "row.groups"))

})


## extract
test_that("aggregated parameters can be extracted as dataframe", {

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", dataframe = TRUE, sep = "***")
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(2L, 99L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter",
    wells(THIN.AGG, full = TRUE)))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 2L))
  expect_equal(as.character(mat[, 2L]), c("4", "3"))
  expect_equal(as.character(mat[, 3L]), rep("lambda", 2L))

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "mu", dataframe = TRUE, sep = "&",
    as.groups = list("run", "organism"))
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(2L, 101L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter",
    wells(THIN.AGG, full = TRUE), "run", "organism"))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_true(all(sapply(mat[, 100L:101L], is.factor)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 2L))
  expect_equal(as.character(mat[, 2L]), c("4", "3"))
  expect_equal(as.character(mat[, 3L]), rep("mu", 2L))
  expect_equal(as.character(mat[, 100L]), c("4", "3"))
  expect_equal(as.character(mat[, 101L]), rep(ORGN, 2L))

})


## extract
test_that("aggregated parameters can be extracted as dataframe with CIs", {

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE)
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(6L, 99L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter",
    wells(THIN.AGG, full = TRUE)))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 6L))
  expect_equal(as.character(mat[, 2L]), rep(c("4", "3"), each = 3L))
  expect_equal(as.character(mat[, 3L]) == rep("lambda", 6L),
    c(T, F, F, T, F, F))
  expect_true(all(grepl("lambda", as.character(mat[, 3L]), fixed = TRUE)))

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE,
    as.groups = list("run", "organism"))
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(6L, 101L))
  expect_equal(colnames(mat), c("organism", "run", "Parameter",
    wells(THIN.AGG, full = TRUE), "run", "organism"))
  expect_true(all(sapply(mat[, 1L:3L], is.factor)))
  expect_true(all(sapply(mat[, 4L:99L], is.numeric)))
  expect_true(all(sapply(mat[, 100L:101L], is.factor)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 6L))
  expect_equal(as.character(mat[, 2L]), rep(c("4", "3"), each = 3L))
  expect_equal(as.character(mat[, 3L]) == rep("lambda", 6L),
    c(T, F, F, T, F, F))
  expect_true(all(grepl("lambda", as.character(mat[, 3L]), fixed = TRUE)))
  expect_equal(mat[, 1L], mat[, 101L])
  expect_equal(mat[, 2L], mat[, 100L])

})


################################################################################


## to_yaml
test_that("YAML can be created from a list", {

  x <- list(a = 1:5, b = c("zonk", "wump"), c = list(c1 = 66, c2 = -3:-5))

  y <- to_yaml(x)
  expect_is(y, "character")
  expect_equal(length(y), 1L)
  expect_true(grepl("^---\n", y))
  expect_true(identical(x, yaml.load(y)))

  y2 <- to_yaml(x, sep = FALSE)
  expect_true(nchar(y) > nchar(y2))
  expect_false(grepl("^---\n", y2))
  expect_true(identical(x, yaml.load(y2)))

  expect_error(y3 <- to_yaml(x, line.sep = "\t"))

  y3 <- to_yaml(x, line.sep = "\r")
  expect_true(identical(nchar(y), nchar(y3)))
  expect_true(grepl("^---\r", y3))
  expect_false(grepl("\n", y3, fixed = TRUE))
  expect_true(identical(x, yaml.load(y3)))

})


## to_yaml
test_that("YAML creation deals with missing list names", {
  # this actually checks a behaviour of the current yaml package, which was not
  # included in its earliest versions
  x <- list(a = 1:10, 11:15, C = 16:20) # note empty key
  # the following code gives a warning on Ubuntu but not on Winbuilder!!!
  suppressWarnings(got <- yaml.load(as.yaml(x)))
  expect_equal(x, got)
  y <- c(x, list(21:25)) # note duplicate empty keys
  expect_error(suppressWarnings(got <- yaml.load(as.yaml(y))))
  y <- c(x, list(a = 21:25)) # note duplicate non-empty keys
  expect_error(suppressWarnings(got <- yaml.load(as.yaml(y))))
})


## to_yaml
test_that("OPM example data can be converted to YAML", {
  lines <- strsplit(to_yaml(SMALL), "\n", fixed = TRUE)[[1]]
  expect_equal("---", lines[1L])
  expect_equal("metadata: []", lines[2L])
  for (name in c("measurements:", "csv_data:")) {
    pos <- which(lines == name)
    expect_equal(1L, length(pos))
    expect_false(identical(lines[pos + 1L], "  []"))
  }
})

## to_yaml
test_that("OPM example data with metadata can be converted to YAML", {
  lines <- strsplit(to_yaml(OPM.WITH.MD[, 1L:10L]), "\n", fixed = TRUE)[[1]]
  expect_equal("---", lines[1L])
  for (name in c("measurements:", "csv_data:", "metadata:")) {
    pos <- which(lines == name)
    expect_equal(1L, length(pos))
    expect_false(identical(lines[pos + 1L], "  []"))
  }
})

## to_yaml
test_that("aggregated OPM data can be converted to YAML", {
  lines <- strsplit(to_yaml(SMALL.AGG), "\n", fixed = TRUE)[[1]]
  expect_equal("---", lines[1L])
  expect_equal("metadata: []", lines[2L])
  for (name in c("aggregated:", "aggr_settings:", "measurements:",
      "csv_data:")) {
    pos <- which(lines == name)
    expect_equal(1L, length(pos))
    expect_false(identical(lines[pos + 1L], "  []"))
  }
})

## to_yaml
test_that("OPMS example data can be converted to YAML", {
  lines <- strsplit(to_yaml(THIN.AGG), "\n", fixed = TRUE)[[1]]
  expect_equal("---", lines[1L])
  pats <- c("metadata", "measurements", "csv_data", "aggregated")
  for (pat in sprintf("^[ -] %s:$", pats)) {
    pos <- grep(pat, lines, perl = TRUE)
    expect_equal(length(THIN.AGG), length(pos))
  }
})


################################################################################

