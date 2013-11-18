

library(testthat)
context("Testing the conversion methods of the opm package")


if (!exists("TEST.DIR"))
  attach(objects_for_testing())


# Test whether or not object 'x' could be submitted to ci_plot.
#
is_ci_plottable <- function(x) {
  param_column_ok <- function(x) {
    pat <- sprintf("^(%s)", paste(param_names(), collapse = "|"))
    x <- sub(pat, "", x, perl = TRUE)
    y <- c("", " CI95 low", " CI95 high")
    !(length(x) %% length(y)) && all(x == y)
  }
  data_columns_ok <- function(x) {
    rest <- seq_len(nrow(x)) %% 3L
    all(x[rest == 1L, , drop = FALSE] >= x[rest == 2L, , drop = FALSE]) &&
      all(x[rest == 1L, , drop = FALSE] <= x[rest == 0L, , drop = FALSE])
  }
  is.data.frame(x) &&
    !is.na(pos <- match(RESERVED_NAMES[["parameter"]], colnames(x))) &&
    param_column_ok(x[, pos]) &&
    data_columns_ok(x[, (pos + 1L):ncol(x), drop = FALSE])
}


A.VALUES <- extract(c(THIN.AGG, THIN.AGG, THIN.AGG),
  as.labels = list("organism", "run"),
  subset = "A", dataframe = TRUE)

NESTED.MD <- SMALL.WITH.MD
metadata(NESTED.MD) <- list(A = list(B = 63, C = 64), K = "N")
tmp <- SMALL.WITH.MD
metadata(tmp) <- list(A = list(F = 63, C = 64), K = "T", O = letters)
NESTED.MD <- c(NESTED.MD, tmp)
rm(tmp)


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

  got <- merge(data, seq_len(nrow(data)))
  expect_equal(got, data)

  expect_error(got <- merge(data, seq_len(nrow(data))[-1L]))

  got <- merge(data, c(1, 2, 1, 2, 1))
  expect_equal(dim(got), c(2L, ncol(data)))

})


## plates
test_that("the plates can be obtained as a list", {
  pl <- plates(OPMS.INPUT)
  expect_is(pl, "list")
  expect_equal(length(pl), 2L)
  expect_true(all(vapply(pl, is, NA, "OPM")))
})


## oapply
## UNTESTED -- but see examples and tests for subsetting


################################################################################


## flatten
test_that("example data can be flattened", {
  base.colnames <- unname(RESERVED_NAMES[c("time", "well", "value")])
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
  base.colnames <- unname(RESERVED_NAMES[c("time", "well", "value")])
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
  base.colnames <- unname(RESERVED_NAMES[c("time", "well", "value")])
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
  base.colnames <- unname(RESERVED_NAMES[c("plate", "time", "well", "value")])
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
  base.colnames <- unname(RESERVED_NAMES[c("plate", "time", "well", "value")])
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
test_that("extract_columns() gets nested metadata right", {

  # 1
  got <- extract_columns(NESTED.MD, list(c("A", "C"), "K"))
  expect_equal(names(got), c("A.C", "K"))

  # 2
  got <- extract_columns(NESTED.MD, list(u = c("A", "C"), v = "K"))
  expect_equal(names(got), c("u", "v"))

  # 3
  old <- opm_opt(key.join = "~")
  on.exit(opm_opt(old))
  got <- extract_columns(NESTED.MD, list(c("A", "C"), "K"))
  expect_equal(names(got), c("A~C", "K"))
  opm_opt(old)

  # 4
  got <- extract_columns(NESTED.MD, list(u = "A"))
  expect_equal(names(got), c("u.B", "u.C"))

  # 4
  got <- extract_columns(NESTED.MD, list(z = "K"))
  expect_equal(names(got), "z")

})


## extract_columns
test_that("extract_columns() gets metadata right using character vector", {

  # 1
  got <- extract_columns(NESTED.MD, "A")
  expect_equal(names(got), c("B", "C"))

  # 2
  got <- extract_columns(NESTED.MD, c("A", "C"))
  expect_equal(names(got), "A.C")

  # 3
  old <- opm_opt(key.join = "~")
  on.exit(opm_opt(old))
  got <- extract_columns(NESTED.MD, c("A", "C"))
  expect_equal(names(got), "A~C")

})


## extract_columns
test_that("extract_columns() gets metadata right if a formula is used", {

  # 1
  got <- extract_columns(NESTED.MD, ~ A$C + K)
  expect_equal(names(got), c("A.C", "K"))

  # 2
  got <- extract_columns(NESTED.MD, ~ A$C)
  expect_equal(names(got), "A.C")

  # 3
  got <- extract_columns(NESTED.MD, ~ K)
  expect_equal(names(got), "K")

  # 4
  old <- opm_opt(key.join = "~")
  on.exit(opm_opt(old))
  got <- extract_columns(NESTED.MD, ~ A$C + K)
  expect_equal(names(got), c("A~C", "K"))

})


## extract_columns
test_that("extract_columns() works if a formula is used with joining", {

  # 1
  got <- extract_columns(NESTED.MD, ~ J(A$C) + K)
  expect_equal(names(got), c("A.C", "K"))

  # 2
  got <- extract_columns(NESTED.MD, ~ A$C + J(K))
  expect_equal(names(got), c("A.C", "K"))

  # 3
  got <- extract_columns(NESTED.MD, ~ J(A$C, K))
  expect_equal(names(got), c("A.C", "K", "A.C.K"))

  # 3
  got <- extract_columns(NESTED.MD, ~ J(A$C + K))
  expect_equal(names(got), c("A.C", "K", "A.C.K"))

  # 4
  old <- opm_opt(key.join = "~", comb.key.join = "!")
  on.exit(opm_opt(old))
  got <- extract_columns(NESTED.MD, ~ J(A$C + K))
  expect_equal(names(got), c("A~C", "K", "A~C!K"))

})


## extract_columns
test_that("extract_columns can be applied to a data frame", {
  x <- data.frame(A = letters[1:5], B = 1:5)
  got <- extract_columns(x, list(C = c("A", "B")), direct = TRUE)
  expect_is(got, "data.frame")
  expect_equal(attr(got, "joined.columns"), list(C = c("A", "B")))
  expect_equal(dim(got), c(5, 3))
  expect_equal(names(got), c("A", "B", "C"))
  expect_equal(rownames(got), rownames(x))
  expect_is(got$C, "factor")
  got <- extract_columns(x, list(C = c("A", "B")), direct = TRUE,
    as.labels = "A")
  expect_is(got, "data.frame")
  expect_equal(dim(got), c(5, 3))
  expect_equal(names(got), c("A", "B", "C"))
  expect_equal(rownames(got), as.character(got$A))
  expect_is(got$C, "factor")
})


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

  mat2 <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", as.groups = TRUE, sep = "||")
  expect_equivalent(mat, mat2)
  expect_equal(as.factor(rep(1L, length(THIN.AGG))), attr(mat2, "row.groups"))

  mat2 <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", as.groups = FALSE, sep = "||")
  expect_equivalent(mat, mat2)
  expect_equal(as.factor(seq(length(THIN.AGG))), attr(mat2, "row.groups"))

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
  expect_equal(colnames(mat), c("organism", "run",
    RESERVED_NAMES[["parameter"]],
    wells(THIN.AGG, full = TRUE)))
  expect_true(all(vapply(mat[, 1L:3L], is.factor, NA)))
  expect_true(all(vapply(mat[, 4L:99L], is.numeric, NA)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 2L))
  expect_equal(as.character(mat[, 2L]), c("4", "3"))
  expect_equal(as.character(mat[, 3L]), rep("lambda", 2L))

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "mu", dataframe = TRUE, sep = "&",
    as.groups = list("run", "organism"))
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(2L, 101L))
  expect_equal(colnames(mat), c("organism", "run",
    RESERVED_NAMES[["parameter"]],
    wells(THIN.AGG, full = TRUE), "run", "organism"))
  expect_true(all(vapply(mat[, 1L:3L], is.factor, NA)))
  expect_true(all(vapply(mat[, 4L:99L], is.numeric, NA)))
  expect_true(all(vapply(mat[, 100L:101L], is.factor, NA)))
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
  expect_equal(colnames(mat), c("organism", "run",
    RESERVED_NAMES[["parameter"]],
    wells(THIN.AGG, full = TRUE)))
  expect_true(all(vapply(mat[, 1L:3L], is.factor, NA)))
  expect_true(all(vapply(mat[, 4L:99L], is.numeric, NA)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 6L))
  expect_equal(as.character(mat[, 2L]), rep(c("4", "3"), each = 3L))
  expect_equal(as.character(mat[, 3L]) == rep("lambda", 6L),
    c(T, F, F, T, F, F))
  expect_true(all(grepl("lambda", as.character(mat[, 3L]), fixed = TRUE)))

  mat2 <- extract(THIN.AGG, as.labels = ~ organism + run,
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE)
  expect_equal(mat, mat2)

  mat2 <- extract(THIN.AGG, as.labels = ~ J(organism + run),
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE)
  expect_equal(dim(mat2), c(6L, 100L))
  expect_equal(mat, mat2[, -match("organism.run", colnames(mat2))])

  mat <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE,
    as.groups = ~ run - organism)
  expect_is(mat, "data.frame")
  expect_equal(dim(mat), c(6L, 101L))
  expect_equal(colnames(mat), c("organism", "run",
    RESERVED_NAMES[["parameter"]],
    wells(THIN.AGG, full = TRUE), "run", "organism"))
  expect_true(all(vapply(mat[, 1L:3L], is.factor, NA)))
  expect_true(all(vapply(mat[, 4L:99L], is.numeric, NA)))
  expect_true(all(vapply(mat[, 100L:101L], is.factor, NA)))
  expect_equal(as.character(mat[, 1L]), rep(ORGN, 6L))
  expect_equal(as.character(mat[, 2L]), rep(c("4", "3"), each = 3L))
  expect_equal(as.character(mat[, 3L]) == rep("lambda", 6L),
    c(T, F, F, T, F, F))
  expect_true(all(grepl("lambda", as.character(mat[, 3L]), fixed = TRUE)))
  expect_equal(mat[, 1L], mat[, 101L])
  expect_equal(mat[, 2L], mat[, 100L])

  mat2 <- extract(THIN.AGG, as.labels = list("organism", "run"),
    subset = "lambda", dataframe = TRUE, sep = "***", ci = TRUE,
    as.groups = ~ J(run - organism))
  expect_equal(dim(mat2), c(6L, 102L))
  expect_equivalent(mat, mat2[, -102L]) # TODO: unclear why different

})


## extract
test_that("extract works without grouping and without normalisation", {
  x <- extract(object = A.VALUES, as.groups = FALSE, norm.per = "none")
  expect_is(x, "data.frame")
  expect_false(is_ci_plottable(x))
  expect_equal(x, A.VALUES)
  expect_error(ci_plot(x)) # no CI were computed
})


## extract
test_that("extract works with grouping and without normalisation", {
  x <- extract(object = A.VALUES, as.groups = TRUE, norm.per = "none")
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(6L, 99L))
  expect_true(is_ci_plottable(x))
  got <- ci_plot(x[, 1L:9L])
  expect_equal(got, c("1: Bacillus simplex 3", "2: Bacillus simplex 4"))
})


## extract
test_that("extract works with grouping and 'plate.sub' normalisation", {
  x <- extract(object = A.VALUES, as.groups = TRUE, norm.per = "row")
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(6L, 99L))
  expect_true(is_ci_plottable(x))
  got <- ci_plot(x[, 1L:9L])
  expect_equal(got, c("1: Bacillus simplex 3", "2: Bacillus simplex 4"))
})


## extract
test_that("one cannot pass too many 'Parameter' columns to group_ci()", {
  Parameter <- rep("A", length(A.VALUES[, 1L]))
  xy <- cbind(A.VALUES, Parameter)
  expect_error(x <- extract(object = xy,
    as.groups = colnames(xy[, c(1L:3L, 102L)]),
    norm.per = "column", norm.by = TRUE))
})


## extract
test_that("extract works with grouping and 'plate.div' normalisation", {
  # 'as.groups' given as character-string of the column-names
  x <- extract(object = A.VALUES, as.groups = colnames(A.VALUES[, 1L:2L]),
    norm.per = "row", subtract = FALSE, norm.by = 10L)
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(6L, 99L))
  expect_true(is_ci_plottable(x))
  got <- ci_plot(x[, 1L:6L])
  expect_equal(got, c("1: Bacillus simplex 3", "2: Bacillus simplex 4"))
})


## extract
test_that("extract works with grouping and 'well.div' normalisation", {
  # as.groups given directly as character-string
  x <- extract(object = A.VALUES, as.groups = c("organism", "run"),
    norm.per = "column")
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(6L, 99L))
  expect_true(is_ci_plottable(x))
  got <- ci_plot(x[, 1L:6L])
  expect_equal(got, c("1: Bacillus simplex 3", "2: Bacillus simplex 4"))
})


## extract
test_that("extract works with grouping and 'well.sub' normalisation", {
  # only one column in as.groups
  x <- extract(object = A.VALUES, as.groups = "organism",
    norm.per = "row")
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(3L, 98L))
  expect_true(is_ci_plottable(x))
  got <- ci_plot(x[, 1L:6L])
  expect_equal(got, "1: Bacillus simplex")
})


## extract
test_that("extract yields error if incorrect 'as.groups' is passed", {
  # wrong columns in 'as.groups'-argument
  expect_error(x <- extract(object = A.VALUES,
    as.groups = c("organism", "run", "H06 (Acetoacetic Acid)"),
    norm.per = "column"))
  # => Error in extract(object = A.VALUES,
  #   as.groups = c("Strain", "Experiment",  :
  #   cannot find column name: H06 (Acetoacetic Acid)
})

## extract
test_that("extract() deals with duplicate column names", {
  x <- extract(object = A.VALUES,
    as.groups = c("organism", "run", "organism"),
    norm.per = "column", dup = "ignore")
  # =>  Warning message:
  # In extract(object = A.VALUES, as.groups = c("Strain", "Experiment",  :
  #   as.groups variable(s) are not unique
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(6L, 100L))
  expect_true(is_ci_plottable(x))
  got <- ci_plot(x[, 1L:10L])
  expect_equal(got, c("1: Bacillus simplex 3 Bacillus simplex",
    "2: Bacillus simplex 4 Bacillus simplex")) # duplication of names
})


################################################################################


## as.data.frame
## UNTESTED


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

