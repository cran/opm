
library(testthat)
context("Testing the substrate mapping of the OPM package for consistency")


## SUBSTRATE_INFO
test_that("CAS numbers and other IDs are OK", {
  na_or_match <- function(x, pattern, what) {
    expect_true(all(is.na(x) | grepl(pattern, x, FALSE, TRUE)),
      sprintf("=> not all %s IDs matched expected pattern", what))
  }
  expect_true(setequal(colnames(SUBSTRATE_INFO),
    c("METACYC", "CAS", "KEGG", "MESH", "DRUG", "CHEBI", "SEED")))
  na_or_match(cas <- SUBSTRATE_INFO[, "CAS"], "^\\d", "CAS")
  expect_true(all(is_cas(cas[!is.na(cas)]))) # more specific check
  na_or_match(SUBSTRATE_INFO[, "KEGG"], "^C\\d{5}$", "KEGG-compound")
  na_or_match(SUBSTRATE_INFO[, "DRUG"], "^D\\d{5}$", "KEGG-drug")
  na_or_match(SUBSTRATE_INFO[, "CHEBI"], "^\\d+$", "ChEBI")
  # the Metacyc IDs are less regular, let alone the MeSH names
  na_or_match(SUBSTRATE_INFO[, "METACYC"], "^\\w+((\\+|--?)\\w+)*$", "Metacyc")
  na_or_match(SUBSTRATE_INFO[, "MESH"], "^[^\\s].*[^\\s]$", "MeSH")
  na_or_match(SUBSTRATE_INFO[, "SEED"], "^cpd\\d+$", "SEED")
})

