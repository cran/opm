

library(testthat)
context("Testing the opm naming functions")


# get example objects
if (!exists("TEST.DIR"))
  attach(objects_for_testing())


################################################################################


## opm_string
## UNTESTED


################################################################################


## opm_files
## UNTESTED


## param_names
test_that("param_names() is consistent with other settings", {
  expect_true(OPM_OPTIONS$curve.param %in%
      c(param_names(), param_names("disc.name")))
})


## select_colors
test_that("predefined color sets can be obtained", {
  for (arg in as.character(formals(select_colors)[[1L]])[-1L]) {
    got <- select_colors(arg)
    expect_is(got, "character")
    expect_true(length(got) >= 10L)
  }
})


################################################################################


## plate_type
test_that("plate types can be explicitely queried", {
  expect_equal(plate_type(OPM.1), "PM01")
  pt.got <- plate_type(OPMS.INPUT)
  expect_is(pt.got, "character")
  expect_equal(length(pt.got), 1L)
})


## plate_type
test_that("plate names can be normalized", {
  # Normal input arguments
  x <- c("<strange>", "PM-M3 A", "PM09", "pm10b", "pmM10D", "PM1")
  exp <- c("<strange>", "PM-M03-A", "PM09", "PM10-B", "PM-M10-D", "PM01")
  got <- plate_type(x, subtype = TRUE)
  expect_equal(got, exp)
  # Microstation plates
  x <- c("<strange>", "ECO", "SFN2", "GP2", "SF-N2", "G-N2")
  exp <- c("<strange>", "ECO", "SF-N2", "SF-P2", "SF-N2", "SF-N2")
  got <- plate_type(x, subtype = TRUE)
  expect_equal(got, exp)
  # Lately added identification plates
  x <- c("<strange>", " an2", "fF", "yT ")
  exp <- c("<strange>", "AN2", "FF", "YT")
  got <- plate_type(x, subtype = TRUE)
  expect_equal(got, exp)
  # The internally used names must already be normalized
  standard.names <- names(PLATE_MAP)
  expect_equal(plate_type(standard.names), standard.names)
  appended <- paste(standard.names, letters)
  expect_equal(plate_type(appended), standard.names)
  expect_equal(names(PLATE_MAP), colnames(WELL_MAP))
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


## map_param_names
test_that("curve parameter names can be mapped", {
  x <- map_param_names()
  expect_true(all(CURVE_PARAMS %in% unlist(x)))
  y <- map_param_names(opm.fast = TRUE)
  expect_true(!any(names(y) %in% names(x)))
  expect_equivalent(x, y)
  x <- map_param_names(plain = TRUE)
  expect_true(setequal(unlist(x), param_names()))
})


## clean_coords
test_that("well names can be cleaned", {
  x <- c("  Z07\t", "D\n11\r", " A06  ", " B7")
  got <- clean_coords(x)
  expect_equal(got, c("Z07", "D11", "A06", "B07"))
})


## clean_plate_positions
test_that("plate positions can be cleaned", {
  x <- c("21-B", " 9-A", "1", "15", "8-B", NA)
  got <- clean_plate_positions(x)
  expect_true(all(grepl("^\\d{2}-[A-Z?]$", got[!is.na(x)], FALSE, TRUE)))
  expect_warning(got.2 <- clean_plate_positions(got))
  expect_equal(got, got.2)
})


## well_index
test_that("well indices given as formula can be mapped", {
  expect_equal(well_index(1:10), 1:10)
  expect_equal(well_index(), TRUE)
  got <- well_index(letters)
  expect_is(got, "character")
  expect_equal(length(got), 26L)
  expect_equal(toupper(got), got)
  expect_error(well_index(NULL ~ a:c))
  expect_equal(well_index(NULL ~ a:c, letters), 1:3)
  expect_equal(well_index(NULL ~ c(b, e:f), letters), c(2, 5, 6))
  expect_error(well_index(NULL ~ c(b, e:f), LETTERS))
  expect_equal(well_index(NULL ~ c(B, E:F), LETTERS), c(2, 5, 6))
})


## map_well_names
## UNTESTED


## to_sentence
## UNTESTED


################################################################################


## wells
test_that("information on the contained wells can be received", {
  w.got <- wells(OPMS.INPUT)
  expect_is(w.got, "character")
  expect_equal(length(w.got), dim(OPMS.INPUT)[3L])
  w.got <- wells(OPMS.INPUT)
  expect_is(w.got, "character")
  expect_equal(length(w.got), dim(OPMS.INPUT)[3L])
})


## wells
test_that("substrate names can be translated", {

  plate.1 <- "PM01"
  exp.1 <- c(A01 = "Negative Control", A02 = "L-Arabinose")
  got <- wells(c("A01", "A02"), plate = plate.1, full = TRUE)

  plates.2 <- c(plate.1, "PM02")
  exp.2 <- c(A01 = "Negative Control", A02 = "Chondroitin Sulfate C")
  exp.2 <- cbind(exp.1, exp.2)
  colnames(exp.2) <- plates.2
  got <- wells(c("A01", "A02"), plate = plates.2, full = TRUE)
  expect_equal(got, exp.2)

  # Partial matching is allowed
  plates.2 <- c(plate.1, "PM02")
  exp.2 <- c(A01 = "Negative Control", A02 = "Chondroitin Sulfate C")
  exp.2 <- cbind(exp.1, exp.2)
  colnames(exp.2) <- c(plates.2[1L], "PM02")
  got <- wells(c("A01", "A02"), plate = plates.2, full = TRUE)
  expect_equal(got, exp.2)

})


## listing
## UNTESTED


################################################################################


## find_substrate
test_that("substrate names can be searched", {

  found <- find_substrate(c(wanted = "Fructose"), search = "exact")
  expect_is(found, "substrate_match")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate"), found[[1L]])

  found <- find_substrate(c(wanted = "Fructose"), search = "approx")
  expect_is(found, "substrate_match")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate", "D-Fucose",
    "L-Fucose"), found[[1L]])

})


## find_substrate
test_that("substrate names can be searched with patterns", {

  glob.pat <- c(A = "ampic*", B = "penic*", C = "random*")
  found <- find_substrate(glob.pat, search = "glob")
  expect_is(found, "substrate_match")
  expect_equal(3L, length(found))
  expect_equivalent(glob.pat, names(found))
  expect_equal(sprintf("Ampicillin #%i", 1:4), found[[1L]])
  expect_equal(sprintf("Penicillin G #%i", 1:4), found[[2L]])
  expect_equal(character(), found[[3L]])

  reg.pat <- c(A = "^ampic.*", B = "^penic.*", C = "^random.*")
  found.2 <- find_substrate(reg.pat, search = "regex")
  expect_equivalent(reg.pat, names(found.2))
  names(found.2) <- glob.pat
  expect_equal(found, found.2)

})


################################################################################


## find_positions
test_that("positions within PM plates can be found", {

  query <- c("D-Fructose", "Penicillin G #1", "This is not present")
  got <- find_positions(query)
  expect_is(got, "list")
  expect_equal(query, names(got))
  expect_true(all(vapply(got, is.matrix, NA)))
  expect_true(all(dim(got[[1L]] > 0L)))
  expect_true(all(dim(got[[2L]] > 0L)))
  expect_true(all(dim(got[[3L]] == 0L)))

  query <- find_substrate("Fructose", search = "exact")
  got <- find_positions(query)
  expect_is(got, "list")
  expect_equal(1L, length(got))
  expect_equal("Fructose", names(got))
  got <- got[[1L]]
  expect_is(got, "list")
  expect_equal(query[[1L]], names(got))
  expect_true(all(vapply(got, is.matrix, NA)))

})


################################################################################


## is_cas
test_that("CAS numbers are recognized", {
  expect_true(all(is_cas(c("CAS 554-91-6", "554-94-9", "CAS 107-07-3"))))
  expect_true(all(!is_cas(c("CAS 554-91-7", "foo", "554-94-8", "bar"))))
  expect_true(all(is.na(is_cas(c(NA, NA, NA)))))
})


## substrate_info
test_that("Greek letters can be expanded and HTML returned", {
  x <- c("A01 (a-D-Fructose)", "Penicillin G", "b-L-Glucose #1",
    "N-Acetyl-Glucosamine")
  wanted <- c("A01 (alpha-D-Fructose)", "Penicillin G", "beta-L-Glucose #1",
    "N-Acetyl-Glucosamine")
  got <- substrate_info(x, "greek")
  expect_equivalent(got, wanted)
  wanted <- c("A01 (&alpha;-<small>D</small>-Fructose)", "Penicillin G",
    "&beta;-<small>L</small>-Glucose #1", "<i>N</i>-Acetyl-Glucosamine")
  got <- substrate_info(x, "html")
  expect_equivalent(got, wanted)
  # an R expression equivalent to the first HTML entry would be:
  # expression(
  #  paste("A01", " (", alpha, "-", scriptstyle("D"), "-", "Fructose, ")")
  # )
})


## substrate_info
test_that("concentrations can be extracted", {
  x <- c("D09 (D-Serine #1)", "A03", "C12 [D-Serine #2]",
    "Negative Control #3", "L-Arginine", "E10 (Lincomycin)", "[Strange #1]")
  got <- substrate_info(x, "concentration")
  wanted <- c(1L, NA, 2L, 3L, NA, NA, NA)
  names(wanted) <- x
  expect_equal(got, wanted)
})


## substrate_info
test_that("URLs can be returned", {
  x <- c("D-Fructose", "D-Serine #2", "L-Arginine")
  for (target in c("kegg", "drug", "metacyc", "mesh", "chebi", "cas")) {
    urls <- substrate_info(x, target, browse = -1L)
    expect_equal(length(urls), length(x))
    expect_is(urls, "character")
    # the next test checks that everything has already been escaped
    got <- vapply(urls, URLencode, "")
    got[got == "NA"] <- NA_character_
    expect_equal(urls, got)
  }
})


################################################################################


## web_query
## UNTESTED


## collect
## UNTESTED


################################################################################


