

library(testthat)


context(
  "Test the functions of the OPM package for creating phylogenetic data.")


################################################################################
#
# Character discretization
#


test_that("characters can be discretized to strings in non-gap mode", {

  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8))
  expect_equal(length(x), length(y))
  expect_is(y, "character")
  expect_equal("0", y[1L])
  expect_equal("V", y[length(y)])
  
  y <- discrete(x, c(0, 10))
  expect_equal(length(x), length(y))
  expect_true("0" < y[1L])
  expect_true("V" > y[length(y)])
  
  y <- discrete(x, c(2, 8), states = 10L)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("9", y[length(y)])
  
  y <- discrete(x, c(2, 8), states = 1)
  expect_equal(length(x), length(y))
  expect_equal(rep("0", length(y)), y)
  
})
  

test_that("characters can be discretized with the real range", {
  
  x <- seq(2, 8, 0.5)
  
  y <- discrete(x, TRUE)
  expect_equal(length(x), length(y))
  expect_is(y, "character")
  expect_equal("0", y[1L])
  expect_equal("V", y[length(y)])
  
  expect_warning(y <- discrete(x, TRUE, gap = TRUE))
  expect_equal(length(x), length(y))
  expect_is(y, "character")
  expect_equal("0", y[1L])
  expect_equal(rep("?", length(y) - 2L), y[c(-1L, -length(y))])
  expect_equal("1", y[length(y)])
  
})



test_that("characters can be discretized to non-strings in non-gap mode", {

  x <- seq(2, 8, 0.5)
  
  y <- discrete(x, c(2, 8), output = "integer")
  expect_equal(length(x), length(y))
  expect_is(y, "integer")
  expect_equal(1L, y[1L])
  expect_equal(32L, y[length(y)])
  
  y <- discrete(x, c(2, 8), output = "logical")
  expect_equal(length(x), length(y))
  expect_is(y, "logical")
  expect_equal(FALSE, y[1L])
  expect_equal(TRUE, y[length(y)])
  
  y <- discrete(x, c(2, 8), output = "factor")
  expect_equal(length(x), length(y))
  expect_is(y, "factor")
  expect_equal("1", as.character(y[1L]))
  expect_equal("32", as.character(y[length(y)]))
  
  y <- discrete(x, c(2, 8), output = "numeric")
  expect_equal(x, y)

})


test_that(
  "characters can be discretized with a gap to binary characters (strings)", {
  
  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8), TRUE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("?", y[2L])
  expect_equal("?", y[length(y) - 1L])
  expect_equal("1", y[length(y)])

  y <- discrete(x, c(3, 7), TRUE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("0", y[2L])
  expect_equal("1", y[length(y) - 1L])
  expect_equal("1", y[length(y)])
  
  expect_error(y <- discrete(x, c(0, 10), TRUE))
  
  y <- discrete(x, c(2, 8), TRUE, states = 1L:10L)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("1", y[length(y)])
  z <- discrete(x, c(2, 8), TRUE, states = 10L)
  expect_equal(y, z)
  z <- discrete(x, c(2, 8), TRUE, states = as.character(0L:9L))
  expect_equal(y, z)
  
  y <- discrete(x, c(2, 8), TRUE, states = 3L:12L)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("1", y[length(y)])
  
})


test_that(
  "characters can be discretized with a gap to binary characters (non-strings)",
  {
  
  x <- seq(2, 8, 0.5)
  
  y <- discrete(x, c(2, 8), TRUE, output = "integer")
  expect_equal(length(x), length(y))
  expect_equal(0L, y[1L])
  expect_equal(1L, y[length(y)])
  
  y <- discrete(x, c(2, 8), TRUE, output = "logical")
  expect_equal(length(x), length(y))
  expect_equal(FALSE, y[1L])
  expect_equal(TRUE, y[length(y)])
  
  y <- discrete(x, c(2, 8), TRUE, output = "factor")
  expect_equal(length(x), length(y))
  expect_is(y, "factor")
  expect_equal("0", as.character(y[1L]))
  expect_equal("1", as.character(y[length(y)]))
  
  y <- discrete(x, c(2, 8), output = "numeric")
  expect_equal(x, y)

})


test_that(
  "characters can be discretized with a gap to ternary characters (strings)", {
  
  x <- seq(2, 8, 0.5)

  y <- discrete(x, c(2, 8), TRUE, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("1", y[2L])
  expect_equal("1", y[length(y) - 1L])
  expect_equal("2", y[length(y)])

  y <- discrete(x, c(3, 7), TRUE, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("0", y[2L])
  expect_equal("2", y[length(y) - 1L])
  expect_equal("2", y[length(y)])
  
  expect_error(y <- discrete(x, c(0, 10), TRUE, middle.na = FALSE))
  
  y <- discrete(x, c(2, 8), TRUE, states = 1L:10L, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("2", y[length(y)])
  z <- discrete(x, c(2, 8), TRUE, states = 10L, middle.na = FALSE)
  expect_equal(y, z)
  z <- discrete(x, c(2, 8), TRUE, states = as.character(0L:9L), 
    middle.na = FALSE)
  expect_equal(y, z)
  
  y <- discrete(x, c(2, 8), TRUE, states = 3L:12L, middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal("0", y[1L])
  expect_equal("2", y[length(y)])
  
})


test_that(paste("characters can be discretized with a gap to ternary",
  "characters (non-strings)"), {
  
  x <- seq(2, 8, 0.5)
  
  y <- discrete(x, c(2, 8), TRUE, output = "integer", middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_equal(0L, y[1L])
  expect_equal(2L, y[length(y)])
  
  expect_error(y <- discrete(x, c(2, 8), TRUE, output = "logical",
    middle.na = FALSE))
  
  y <- discrete(x, c(2, 8), TRUE, output = "factor", middle.na = FALSE)
  expect_equal(length(x), length(y))
  expect_is(y, "factor")
  expect_equal("0", as.character(y[1L]))
  expect_equal("2", as.character(y[length(y)]))
  
  y <- discrete(x, c(2, 8), output = "numeric", middle.na = FALSE)
  expect_equal(x, y)

})


test_that("a matrix of characters can be discretized", {
  
  x <- matrix(seq(2, 8, 0.5)[-1L], ncol = 4L)
  rownames(x) <- letters[1L:3L]
  colnames(x) <- LETTERS[1L:4L]
  
  y <- discrete(x, c(2, 8), gap = FALSE)
  expect_is(y[1L], "character")
  expect_equal(dim(x), dim(y))
  expect_equal(rownames(x), rownames(y))
  expect_equal(colnames(x), colnames(y))
  
  y <- discrete(x, c(3, 3), gap = TRUE)
  expect_is(y[1L], "character")
  expect_equal(dim(x), dim(y))
  expect_equal(rownames(x), rownames(y))
  expect_equal(colnames(x), colnames(y))

  y <- discrete(x, c(3, 3), gap = TRUE, middle.na = FALSE)
  expect_is(y[1L], "character")
  expect_equal(dim(x), dim(y))
  expect_equal(rownames(x), rownames(y))
  expect_equal(colnames(x), colnames(y))

})


################################################################################


test_that("discrete characters can be joined, indicating ambiguity", {
  
  # Vectors
  expect_equal(join_discrete(c(1L, 0L, 0L, 1L)), "(01)")
  expect_equal(join_discrete(list(2L, 2L, 2L)), "2")
  expect_equal(join_discrete(c(0L, 0L, "?")), "0")
  expect_equal(join_discrete(c("?", "?", "?")), "?")
  err.msg <- "need strings of length 1"
  expect_error(join_discrete(c(0L, 2L, 31L)), err.msg)
  expect_error(join_discrete(c()), err.msg)

  # Matrices
  m <- matrix(c("E", "D", "?", "0", "2", "1"), ncol = 2)
  expect_equal(join_discrete(m, 1L), c("(0E)", "(2D)", "1"))
  expect_equal(join_discrete(m), c("(DE)", "(012)"))
  expect_equal(join_discrete(m, 2L), c("(DE)", "(012)"))

})


################################################################################
#
# Phylogenetic data export
#


test_that("headers can be created", {
  
  x <- matrix(LETTERS[1:10], ncol = 2L)  
  rownames(x) <- paste("taxon", 1L:5L, sep = "_")
  colnames(x) <- paste("char", 1L:2L, sep = "_")
  
  y <- phylo_header(x, "phylip")
  expect_equal(y, "5 2")
  
  y <- phylo_header(x, "epf")
  expect_equal(y, "5 2")
  
  y <- phylo_header(x, "nexus")
  expect_equal(length(y), 8L)
  expect_equal(y[1L:3L], c("#NEXUS", "", "begin data;"))

  colnames(x) <- NULL
  expect_warning(y <- phylo_header(x, "nexus"))
  expect_equal(length(y), 7L)
  expect_equal(y[1L:3L], c("#NEXUS", "", "begin data;"))

})


test_that("entire phylip and epf matrices can be created", {
  
  x <- matrix(LETTERS[1:10], ncol = 2L)  
  rownames(x) <- paste("taxon", 1L:5L, sep = "_")
  colnames(x) <- paste("char", 1L:2L, sep = "_")
  
  y <- phylo_data(x, "phylip")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 1L)
  
  y <- phylo_data(x, "epf")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 1L)

})


test_that("entire nexus matrices can be created", {
  
  x <- matrix(LETTERS[1:10], ncol = 2L)  
  rownames(x) <- paste("taxon", 1L:5L, sep = "_")
  colnames(x) <- paste("char", 1L:2L, sep = "_")
  
  y <- phylo_data(x, "nexus")
  expect_is(y, "character")
  expect_equal(length(y), nrow(x) + 14L)

  colnames(x) <- NULL
  expect_warning(y <- phylo_data(x, "nexus"))

  rownames(x) <- NULL
  expect_warning(expect_error(y <- phylo_data(x, "nexus")))

  xx <- matrix(1:10, ncol = 2L)
  rownames(xx) <- paste("taxon", 1L:5L, sep = "_")
  colnames(xx) <- paste("char", 1L:2L, sep = "_")
  expect_warning(yy <- phylo_data(xx, "nexus"))
  colnames(xx) <- LETTERS[1:ncol(xx)]
  rownames(xx) <- NULL
  expect_warning(expect_error(yy <- phylo_data(xx, "nexus")))

})


test_that("entire nexus matrices can be created, part 2", {
  
  x <- matrix(LETTERS[1:10], ncol = 2L)  
  rownames(x) <- paste("taxon", 1L:5L, sep = "_")
  colnames(x) <- paste("char", 1L:2L, sep = "_")

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




