

library(testthat)


context("Testing the list-associated methods.")


################################################################################
#
# Mapping values
#


test_that("we can map values in character vectors", {
  
  map <- c(a = '1', b = '2', c = '3')
  x <- c("d", "c", "b", "a", "A")
  names(x) <- LETTERS[1L:5L]
  exp <- c("d", "3", "2", "1", "A")
  names(exp) <- names(x)
  
  got <- map_values(x, map)
  expect_equal(exp, got)
  
  map.2 <- as.character(1L:3L) # no names => all mappings unsuccessful
  got <- map_values(x, map.2)
  expect_equal(x, got)
  
})


test_that("we can map values in lists using character vectors", {
  
  map <- c(a = '1', b = '2', c = '3')
  x <- c("d", "c", "b", "a", "A")
  names(x) <- LETTERS[1L:5L]
  exp <- c("d", "3", "2", "1", "A")
  names(exp) <- names(x)

  xy <- list(x = x, y = 1:10)
  got <- map_values(xy, map)
  expect_is(got, "list")
  expect_equal(got[[1L]], exp)
  expect_equal(got[[2L]], 1:10)
  expect_equal(names(got), names(xy))
  
  got <- map_values(xy, map, coerce = "integer")
  expect_is(got, "list")
  expect_equal(got[[1L]], exp)
  expect_equal(got[[2L]], as.character(1:10))
  expect_equal(names(got), names(xy))
  
})


################################################################################
#
# Mapping names
#


test_that("we can map and get names in lists", {
  
  x <- list(a = 99, b = list(xx = c(a = "NA", b = "99.5", c = "10e+06")), 
    c = 8, d = "Z")

  # Using a character vector
  map <- c(a = "b", b = "a", xx = "yy", c = "d", d = "e")
  got <- map_names(x, map)
  exp <- list(b = 99, a = list(yy = c(a = "NA", b = "99.5", c = "10e+06")), 
    d = 8, e = "Z")
  expect_equal(got, exp)
  
  # Using a function
  got <- map_names(x, identity)
  expect_equal(got, x)
  
  # Conducting just a query
  got <- map_names(x)
  exp <- c("a", "b", "c", "d", "xx")
  names(exp) <- exp
  expect_equal(got, exp)
  
})


################################################################################
#
# Querying with contains()
#


test_that("we can query a list with a list with exact matches", {
  
  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))
  
  query <- list(c = list(y = 100), d = 1:2)
  expect_true(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))
  
  query <- list(a = 99, c = list(z = 101))
  expect_true(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))
  
  query <- list()
  expect_true(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))
  
})
  

test_that("we can query a list with a list without matches", {
  
  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))
  
  query <- list(b = 99, c = list(z = 101))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))  
  
})


test_that("we can query a list with a list with only non-exact matches", {
  
  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))
  
  query <- list(c = list(y = c(100, 101)), d = 1:3)
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))
  
  query <- list(c = list(y = 101), d = list(1:2))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(a = "99", c = list(z = 101))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_true(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))
  
  query <- list(c = list(y = 100), d = list(1:2))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_true(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_true(contains(x, query, values = FALSE, exact = FALSE))

})
  

test_that("we can query a list with a list with missing names", {
  
  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))
  
  query <- list(list(i = 1, j = 2))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))
  
  query <- list(1, 2)
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))

  query <- list(13, a = 99, 2)
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))
  
})


################################################################################
#
# NA reparation
#


test_that("we can repair NAs in a character vector", {
  x <- c("abc", " ", "NA", " NA", "           NA", "123", "NA ")
  got <- repair_na_strings(x)
  expect_equal(got, c("abc", " ", NA, NA, NA, "123", "NA "))
})


test_that("we can repair NAs in a list", {
  
  x <- list(a = 99, b = list(xx = c("NA", "99.5", "10e+06")), c = 8, d = "Z")
  wanted <- list(a = 99, b = list(xx = c(NA_real_, 99.5, 10e+06)), c = 8,
    d = "Z")
  
  got <- repair_na_strings(x)
  expect_equal(wanted, got)
  
  got <- repair_na_strings(x, "numeric")
  expect_equal(wanted, got)
  
  got <- repair_na_strings(x, "integer")
  wanted$b$xx <- c(NA_integer_, as.integer(x$b$xx[2L:3L]))
  expect_equal(wanted, got)

  got <- repair_na_strings(x, "complex")
  wanted$b$xx <- c(NA_complex_, as.complex(x$b$xx[2L:3L]))
  expect_equal(wanted, got)

})


################################################################################


test_that("we can repair partially missing names in a list", {
  
  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))
  got <- repair_names(x)
  exp <- list(a = 99, `2` = list(i = 1, j = 2), d = 1:2,
    c = list(`1` = 99, y = 100, z = 101))
  expect_equal(exp, got)
  
  got <- repair_names(x, TRUE)
  expect_equal(exp, got)

  got <- repair_names(x, FALSE)
  exp <- list(99, list(i = 1, j = 2), 1:2, list(99, 100, 101))
  expect_equal(exp, got)
  
})


################################################################################
#
# List traversal
#


test_that("we can traverse a list", {
  x <- list(a = 9, b = 17, k = 88)
  func <- function(x, y) x + y
  got <- traverse(object = x, func = func, cores = 1L, y = 3)
  expect_equal(got, list(a = 12, b = 20, k = 91))
  got <- traverse(object = x, func = func, cores = 2L, y = 3)
  expect_equal(got, list(a = 12, b = 20, k = 91))
})


################################################################################
#
# List insertion
#


test_that("we can insert a list in a list", {
  
  x <- list(a = 9, b = 17, k = 88)
  y <- list(b = -17, k = 0)
  
  got <- insert(x, y, .force = FALSE)
  expect_equal(x, got)
  
  got <- insert(x, y, .force = TRUE)
  expect_equal(got, list(a = 9, b = -17, k = 0))
  
  z <- list(x = NULL, xx = "318")
  got <- insert(x, c(y, z), .force = FALSE)
  expect_equal(c(x, z), got)
  
})

test_that("we can insert anything and nothing in a list", {
  
  x <- list(a = 9, b = 17:18, k = 88)
  y <- list(b = -17, k = 0)
  
  got <- insert(x, b = -17, k = 0:3, .force = FALSE)
  expect_equal(x, got)
  
  got <- insert(x, b = -17, k = 0:3, .force = TRUE)
  expect_equal(got, list(a = 9, b = -17, k = 0:3))
  
  z <- list(x = NULL, xx = "318")
  got <- insert(x, x = NULL, xx = "318", .force = FALSE)
  expect_equal(c(x, z), got)
 
  got <- insert(x)
  expect_equal(x, got)
  
})



