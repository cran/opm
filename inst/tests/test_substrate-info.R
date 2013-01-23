
library(testthat)
context("Testing the substrate mapping of the OPM package for consistency")


## SUBSTRATE_INFO
test_that("substrate names fit together in ID map and well map", {
  expect_true(all(rownames(SUBSTRATE_INFO) %in% WELL_MAP))
})


## SUBSTRATE_INFO
test_that("CAS numbers other IDs are OK", {
  expect_true(setequal(colnames(SUBSTRATE_INFO),
    c("METACYC", "CAS", "KEGG", "MESH")))
  expect_true(all(is.na(SUBSTRATE_INFO[, "CAS"]) | grepl("^CAS \\d+(-\\d+)+$",
    SUBSTRATE_INFO[, "CAS"], perl = TRUE)))
  expect_true(all(is.na(SUBSTRATE_INFO[, "KEGG"]) | grepl("^[A-Z]\\d+$",
    SUBSTRATE_INFO[, "KEGG"], perl = TRUE)))
  expect_true(all(is.na(SUBSTRATE_INFO[, "MESH"]) | grepl("^[A-Z]\\d+$",
    SUBSTRATE_INFO[, "MESH"], perl = TRUE)))
  # the Metacyc IDs are less regular
})

