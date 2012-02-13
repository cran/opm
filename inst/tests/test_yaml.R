

library(testthat)


context("Testing the YAML-specific methods.")


test_that("we can create YAML from a list", {
  
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

