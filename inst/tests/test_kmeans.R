

library(testthat)
context("Testing the k-means related functions of the OPM package")


## to_kmeans
test_that("conversion to 'kmeans' objects works", {
  x <- c(1, 2, 4, 5, 7, 8)
  x.ck <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, 3)
  x.k <- kmeans(x, 3)
  got <- to_kmeans(x.ck, x)
  expect_equal(names(x.k), names(got))
  expect_equal(sort(x.k$centers), sort(got$centers))
  expect_equal(sort(x.k$withinss), sort(got$withinss))
  expect_equal(x.k$tot.withinss, got$tot.withinss)
  expect_equal(x.k$totss, got$totss)
  expect_equal(x.k$betweenss, got$betweenss)
  expect_equal(x.k$size, got$size)
})


## calinski
## UNTESTED

## plot
## UNTESTED

## borders
## UNTESTED

## hist
## UNTESTED

## prepare_k
## UNTESTED

## run_kmeans
## UNTESTED

