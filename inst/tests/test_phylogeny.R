

library(testthat)
context("Testing the phylogeny-related functions of the OPM package")


SIMPLE.MATRIX <- matrix(LETTERS[1:10], ncol = 2L)
rownames(SIMPLE.MATRIX) <- paste("taxon", 1L:5L, sep = "_")
colnames(SIMPLE.MATRIX) <- paste("char", 1L:2L, sep = "_")

SIMPLE.GROUPS <- integer(nrow(SIMPLE.MATRIX))
SIMPLE.GROUPS[1:2] <- 1
SIMPLE.GROUPS <- sprintf("group-%i", SIMPLE.GROUPS)

FLOAT.MATRIX <- SIMPLE.MATRIX
FLOAT.MATRIX[] <- 1:10
storage.mode(FLOAT.MATRIX) <- "double"

DISC.MATRIX <- SIMPLE.MATRIX
DISC.MATRIX[] <- 1:10
storage.mode(DISC.MATRIX) <- "integer"
DISC.MATRIX <- discrete(DISC.MATRIX, TRUE, TRUE)


################################################################################


## safe_labels
test_that("cleaning names for HTML recognizes previously escaped strings", {
  x <- c("&", "a sermon, a narrative & a prayer", "& this & that &",
    "& now the larch")
  y <- gsub("&", "&amp;", x, fixed = TRUE)
  got.x <- safe_labels(x, format = "html")
  got.y <- safe_labels(y, format = "html")
  expect_equal(got.x, got.y)
})

## safe_labels
test_that("cleaning names correctly converts string starts and ends", {
  x <- c(" ab-*cd\t", "/ab/cd/")
  got <- safe_labels(x, format = "nexus", enclose = FALSE)
  expect_equal(got, c("ab_cd", "ab_cd"))
})



## list2html
test_that("HTML can be recursively generated", {
  x <- list(a = 63, c = list(b = letters, structure(LETTERS, .Names = letters)))
  got <- list2html(x)
  expect_is(got, "character")
  expect_equal(length(got), 1L)
  got <- strsplit(got, "\\s*<[^>]+>\\s*", perl = TRUE)[[1]]
  expect_true(setequal(got[nzchar(got)],
    c(63, LETTERS, paste(letters, collapse = " "))))
})


## format
## UNTESTED


## html_args
## UNTESTED


## phylo_data
test_that("HTML tables can be created", {

  x <- DISC.MATRIX
  y <- phylo_data(x, "html")

  # general checks
  tags <- c("head", "body", "html")
  tags <- c(tags, sprintf("/%s", tags))
  tags <- sprintf("<%s>", tags)
  expect_is(y, "character")
  expect_equal(length(y), 16L)
  expect_true(all(grepl("^<", y, perl = TRUE)))
  expect_true(all(tags %in% y))

  # examine the table
  table <- grep("^<table", y, value = TRUE, perl = TRUE)
  expect_equal(length(table), 1L)
  table <- unlist(strsplit(table, "\\s*<[^>]+>\\s*", perl = TRUE))
  table <- table[nzchar(table)]
  values <- c(seq.int(nrow(x)), colnames(x), "-", "w", "+")
  expect_true(setequal(table, values))

  if (length(tidy())) {
    # tidy check must be OK
    tidy.check <- tidy(y, check = TRUE)
    expect_true("No warnings or errors were found." %in% tidy.check)
    # conversion must work
    yy <- phylo_data(x, "html", run.tidy = TRUE)
    expect_is(yy, "character")
    expect_false(identical(y, yy))
    expect_false(all(grepl("^<", yy[nzchar(yy)], perl = TRUE)))
    expect_false(any(tags %in% yy))
    expect_true(all(toupper(tags) %in% yy))
  } else
    warning("'tidy' not found -- cannot check HTML")

})


## phylo_data
test_that("HTML tables can be created and information added", {

  x <- DISC.MATRIX
  y <- phylo_data(x, "html")

  z <- phylo_data(x, "html", html.args = html_args(css.file = "x.css"))
  expect_equal(length(y) + 2L, length(z))

  z <- phylo_data(x, "html", html.args = html_args(
    meta = c(a = 99, b = 1:3)))
  expect_equal(length(y) + 2L, length(z))

  expect_error(z <- phylo_data(x, "html",
    html.args = html_args(meta = letters)))

  z <- phylo_data(x, "html", html.args = html_args(
    meta = c(a = 99, b = 1:3), meta = structure(letters, .Names = LETTERS)))
  expect_equal(length(y) + 3L, length(z))

  z <- phylo_data(x, "html", comments = "This is a title")
  expect_equal(length(y) + 2L, length(z))

  # adding zero to several headers
  zz <- phylo_data(x, "html", comments = "This is a title",
    html.args = html_args(headline = NULL))
  zz <- phylo_data(x, "html", comments = "This is a title",
    html.args = html_args(headline = "But this is the header"))
  expect_equal(length(z), length(zz))
  expect_true(length(which(z != zz)) %in% 1:2)
  zz <- phylo_data(x, "html", comments = "This is a title",
    html.args = html_args(headline = "But this is the header",
      headline = "...and another header"))
  expect_equal(length(y) + 3L, length(zz))

})


## phylo_data
test_that("HTML tables can be created and user-defined sections added", {

  x <- DISC.MATRIX
  y <- phylo_data(x, "html")

  z <- phylo_data(x, "html", html.args = html_args(
    prepend = "This is some text"))
  expect_equal(length(y) + 2L, length(z))

  zz <- phylo_data(x, "html", html.args = html_args(
    prepend = "This is some text", prepend = "And more text"))
  expect_equal(length(y) + 2L, length(zz))
  expect_true(length(which(z != zz)) %in% 1L:2L)

  z <- phylo_data(x, "html", html.args = html_args(
    prepend = "This is some text", append = "and appended text",
    prepend = "and more text",
    insert = "and inserted text"))
  expect_equal(length(y) + 6L, length(z))

  zz <- phylo_data(x, "html", html.args = html_args(
    prepend = "This is some text", append = "and appended text",
    prepend = list(text1 = "and more text", text2 = "and text"),
    insert = "and inserted text"))
  expect_equal(length(y) + 6L, length(zz))
  expect_true(length(which(z != zz)) %in% 1L:2L)

})


## phylo_data
test_that("HTML tables can be generated after joining", {

  x <- DISC.MATRIX
  y <- phylo_data(x, "html")

  yy <- phylo_data(x, "html", join = TRUE)
  expect_equal(y, yy)
  yy <- phylo_data(x, "html", join = FALSE)
  expect_equal(y, yy)

  yy <- phylo_data(x, "html", join = SIMPLE.GROUPS)
  expect_equal(length(y), length(yy))
  expect_true(length(setdiff(yy, y)) %in% 2L:3L) # times could differ
  ##cat(yy, sep = "\n")

})


## phylo_data
test_that("floating-point HTML tables can be created", {
  x <- FLOAT.MATRIX
  y <- phylo_data(x, "html")
  yy <- phylo_data(x, "html", join = SIMPLE.GROUPS)
  expect_equal(length(y), length(yy))
  expect_true(length(setdiff(yy, y)) %in% 2L:3L) # times could differ
  ##cat(yy, sep = "\n")
  expect_false(any(grepl("&plusmn;", y, fixed = TRUE)))
  expect_true(any(grepl("&plusmn;", yy, fixed = TRUE)))
})


## phylo_data
test_that("phylip and epf matrices can be created", {

  x <- DISC.MATRIX

  y <- phylo_data(x, "phylip")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 1L)
  expect_equal(y[1L], "5 2")

  y <- phylo_data(x, "epf")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 1L)
  expect_equal(y[1L], "5 2")

  y <- phylo_data(x, "phylip", join = SIMPLE.GROUPS)
  expect_is(y, "character")
  expect_equal(y[1L], "2 2")

  y <- phylo_data(x, "epf", join = SIMPLE.GROUPS)
  expect_equal(y[1L], "2 2")
  expect_equal(y[1L], "2 2")

})


## phylo_data
test_that("floating-point phylip and epf matrices can be created", {

  x <- FLOAT.MATRIX

  y <- phylo_data(x, "phylip")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 1L)
  expect_equal(y[1L], "5 2")

  y <- phylo_data(x, "epf")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 1L)
  expect_equal(y[1L], "5 2")

  expect_error(y <- phylo_data(x, "phylip", join = SIMPLE.GROUPS))
  expect_error(y <- phylo_data(x, "epf", join = SIMPLE.GROUPS))

})


## phylo_data
test_that("Hennig86 matrices can be created", {

  x <- SIMPLE.MATRIX

  y <- phylo_data(x, "hennig")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 10L)

  wanted <- c("procedure /;", ";", "ccode - .;", "nstates 32;", "'", "xread",
    "&[numeric]")
  expect_true(all(wanted %in% y))

  comment(x) <- "this is a comment"
  z <- phylo_data(x, "hennig")
  expect_equal(length(which(y != z)), 1L)
  expect_false(comment(x) %in% y)
  expect_true(comment(x) %in% z)

  # warning because < 4 organisms
  expect_warning(z <- phylo_data(x, "hennig", join = SIMPLE.GROUPS))
  expect_true(all(wanted %in% z))
  expect_true(length(z) < length(y))

  x <- FLOAT.MATRIX
  storage.mode(x) <- "integer"
  y <- phylo_data(x, "hennig") # compare this with 'double' storage mode below
  expect_true(all(wanted %in% y))

})


## phylo_data
test_that("Hennig86 matrices with continuous data can be created", {

  x <- FLOAT.MATRIX

  y <- phylo_data(x, "hennig")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 10L)

  wanted <- c("procedure /;", ";", "ccode - .;", "nstates cont;", "'", "xread",
    "&[continuous]")
  expect_true(all(wanted %in% y))

  comment(x) <- "this is a comment"
  z <- phylo_data(x, "hennig")
  expect_equal(length(which(y != z)), 1L)
  expect_false(comment(x) %in% y)
  expect_true(comment(x) %in% z)

  # warning because < 4 organisms
  expect_warning(z <- phylo_data(x, "hennig", join = SIMPLE.GROUPS))
  expect_true(all(wanted %in% z))
  expect_true(length(z) < length(y))

  expect_false(length(which(grepl("-", y, fixed = TRUE))) > 1L)
  expect_true(length(which(grepl("-", z, fixed = TRUE))) > 1L)

})


## phylo_data
test_that("nexus matrices can be created", {

  x <- SIMPLE.MATRIX

  y <- phylo_data(x, "nexus")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 19L)
  expect_equal(y[c(1L, 2L, 7L)], c("#NEXUS", "", "begin data;"))

  z <- phylo_data(x, "nexus", join = TRUE)
  expect_equal(y, z)
  z <- phylo_data(x, "nexus", join = seq.int(nrow(x)))
  expect_equal(y, z)

  z <- phylo_data(x, "nexus", join = SIMPLE.GROUPS)
  expect_true(length(y) > length(z))

})


## phylo_data
test_that("floating-point nexus matrices can be created", {

  x <- FLOAT.MATRIX

  expect_warning(y <- phylo_data(x, "nexus"))
  expect_is(y, "character")

  expect_equal(length(y), nrow(x) + 18L)
  expect_equal(y[c(1L, 2L, 7L)], c("#NEXUS", "", "begin data;"))

  expect_warning(z <- phylo_data(x, "nexus", join = TRUE))
  expect_equal(y, z)
  expect_warning(z <- phylo_data(x, "nexus", join = seq.int(nrow(x))))
  expect_equal(y, z)

  # ... but not if they contain ambiguities
  expect_error(z <- phylo_data(x, "nexus", join = SIMPLE.GROUPS))

})


## phylo_data
test_that("nexus matrices cannot be created with missing names", {

  x <- SIMPLE.MATRIX

  colnames(x) <- NULL
  expect_warning(y <- phylo_data(x, "nexus"))
  expect_equal(length(y), nrow(x) + 18L)
  expect_equal(y[c(1L, 2L, 7L)], c("#NEXUS", "", "begin data;"))

  rownames(x) <- NULL
  expect_error(y <- phylo_data(x, "nexus"))

  xx <- matrix(1:10, ncol = 2L)
  rownames(xx) <- paste("taxon", 1L:5L, sep = "_")
  colnames(xx) <- paste("char", 1L:2L, sep = "_")
  yy <- phylo_data(xx, "nexus")
  colnames(xx) <- LETTERS[1:ncol(xx)]
  rownames(xx) <- NULL
  expect_error(yy <- phylo_data(xx, "nexus"))

})


## phylo_data
test_that("nexus matrices can be created with other conversions", {

  x <- SIMPLE.MATRIX

  y <- phylo_data(x, "nexus")
  z <- phylo_data(x, "nexus", indent = 2L)
  expect_equal(length(y), length(z))
  expect_false(all(y == z))
  expect_true(any(y == z))
  expect_false(any(nchar(z) > nchar(y)))
  expect_false(all(nchar(z) < nchar(y)))
  expect_true(any(nchar(z) < nchar(y)))

  y <- phylo_data(x, "nexus")
  z <- phylo_data(x, "nexus", enclose = FALSE)
  expect_equal(length(y), length(z))
  expect_false(all(y == z))
  expect_true(any(y == z))
  expect_false(any(nchar(z) > nchar(y)))
  expect_false(all(nchar(z) < nchar(y)))
  expect_true(any(nchar(z) < nchar(y)))

  z <- phylo_data(x, "nexus", paup.block = TRUE)
  expect_true(all(y %in% z))
  expect_true(length(y) < length(z))
  expect_false("begin paup;" %in% y)
  expect_true("begin paup;" %in% z)

})


