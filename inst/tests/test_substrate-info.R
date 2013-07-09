
library(testthat)
context("Testing the substrate mapping of the OPM package for consistency")


## SUBSTRATE_INFO
test_that("substrate names fit together in ID map and well map", {
  expect_true(all(rownames(SUBSTRATE_INFO) %in% WELL_MAP))
})


## SUBSTRATE_INFO
test_that("CAS numbers and other IDs are OK", {
  na_or_match <- function(x, pattern) {
    expect_true(all(is.na(x) | grepl(pattern, x, perl = TRUE)))
  }
  expect_true(setequal(colnames(SUBSTRATE_INFO),
    c("METACYC", "CAS", "KEGG", "MESH", "DRUG", "CHEBI", "Description")))
  na_or_match(SUBSTRATE_INFO[, "CAS"], "^CAS \\d+(-\\d+)+$")
  na_or_match(SUBSTRATE_INFO[, "KEGG"], "^C\\d{5}$")
  na_or_match(SUBSTRATE_INFO[, "DRUG"], "^D\\d{5}$")
  na_or_match(SUBSTRATE_INFO[, "CHEBI"], "^CHEBI:\\d+$")
  # the Metacyc IDs are less regular, let alone the MeSH names
})

