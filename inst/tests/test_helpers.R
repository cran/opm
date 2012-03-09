

library(testthat)


context("Test the helper functions of the OPM package.")


################################################################################
#
# Miscellaneous utilities
#


test_that("lengths can be asserted", {
  x <- 3
  y <- 9:10
  z <- 'a'
  expect_equal(c("x", "z"), assert_length(x, z))
  expect_error(assert_length(x, y), "need object 'y' of length 1")
  expect_error(assert_length(x, y, .wanted = 2L), 
    "need object 'x' of length 2")
  expect_error(assert_length(y, z, .wanted = 2L), 
    "need object 'z' of length 2")
  expect_equal("y", assert_length(y, .wanted = 2L))
})


test_that("uniformity can be checked", {
  x <- list(a = 1:2, b = 1:2, a = 1:2)
  expect_true(isTRUE(is_uniform(x)))
  x <- c(x, list(c = 1:6))
  expect_false(isTRUE(isuni <- is_uniform(x)))
  expect_equal(isuni, x[c(1L, 4L)])
})


test_that("listings can be created", {
  x <- c(t = "abc", k = "xyz")
  xl <- listing(x)
  expect_is(xl, "character")
  expect_equal(length(xl), 1L)
  expect_equal(xl, listing(as.list(x)))
  expect_false(xl == listing(x, style = "table"))
  expect_false(xl == listing(x, collapse = "\r\n"))
  expect_false(xl == listing(x, header = "test"))
  expect_true(nchar(xl) < nchar(listing(x, header = "test")))
})


test_that("character vectors can be split regularly", {
  
  x <- c(
    "ibb_blastall.sim2_IS.log",
    "ibb_blastall.sim2_LS.log",
    "ibb_blastall.sim2_SS.log",
    "ibb_blat.sim2_IS.log",
    "ibb_blat.sim2_LS.log",
    "ibb_blat.sim2_SS.log",
    "ibb_megablast.sim2_IS.log",
    "ibb_megablast.sim2_LS.log",
    "ibb_megablast.sim2_SS.log"
  )
  
  got <- separate(x, c("#", "?", "%"))
  expect_is(got, "matrix")
  expect_equal(ncol(got), 1L)
  expect_equal(x, got[, 1L])

  got <- separate(x, "")
  expect_is(got, "matrix")
  expect_equal(ncol(got), 1L)
  expect_equal(x, got[, 1L])

  got <- separate(x, c(".", "_"))
  expect_is(got, "matrix")
  expect_equal(dim(got), c(length(x), 5L))
  expect_true(all(got[, 1L] == "ibb"))
  expect_true(all(got[, 3L] == "sim2"))
  expect_true(all(got[, 5L] == "log"))
  
  got.2 <- separate(x, c("_", "."))
  expect_equal(got.2, got)
  got.2 <- separate(x, "_.")
  expect_equal(got.2, got)
  got.2 <- separate(x, c("_-.", "#%&()"))
  expect_equal(got.2, got)
  
})


## UNTESTED: glob_to_regex


################################################################################
#
# Creating strings
#


test_that("strings can be trimmed", {
  x <- c("abcd", "a", "", "xy-", "zzz")  
  got <- trim_string(x, 2)
  expect_equal(got, c("a.", "a", "", "x.", "z."))
  got.2 <- trim_string(x, 2, word.wise = TRUE)
  expect_equal(got, got.2)
})


test_that("annotations in parentheses can be added to a string", {
  x <- c("A07", "B11")
  y <- c("Sodium Bromide", "Calcium Nitrate")
  expect_equal("A07 (Sodium Bromide)", add_in_parens(x, y)[1L])
  expect_equal("A07\n(Sodium Bromide)", 
    add_in_parens(x, y, paren.sep = "\n")[1L])
  expect_equal("A07 [Sodium Bromide]", add_in_parens(x, y, brackets = TRUE)[1L])
  expect_equal("B11 (Calcium Nitrate)", add_in_parens(x, y)[2L])
  expect_equal("A07 (Sodium Bromide)", add_in_parens(x, y, 100L)[1L])
  expect_equal("B11 (Calcium Nitrate)", add_in_parens(x, y, 100L)[2L])
  expect_equal("A07 (Sod.)", add_in_parens(x, y, 10L)[1L])
  expect_equal("B11 (Cal.)", add_in_parens(x, y, 10L)[2L])
  expect_equal("A07 (S.)", add_in_parens(x, y, 8L)[1L])
  expect_equal("B11 (C.)", add_in_parens(x, y, 8L)[2L])
  expect_equal("A07", add_in_parens(x, y, 7L)[1L])
  expect_equal("B11", add_in_parens(x, y, 7L)[2L])
  expect_equal("A07 (.)", add_in_parens(x, y, 7L, clean = FALSE)[1L])
  expect_equal("B11 (.)", add_in_parens(x, y, 7L, clean = FALSE)[2L])
})
          

test_that("annotations can be added with word-wise abbreviation", {
  x <- c("A07", "B11")
  y <- c("Sodium Bromide", "Calcium Nitrate")
  got <- add_in_parens(x, y, word.wise = TRUE)
  expect_equal("A07 (Sodium Bromide)", got[1L])
  expect_equal("B11 (Calcium Nitrate)", got[2L])
  got <- add_in_parens(x, y, 10L, word.wise = TRUE)
  expect_equal("A07 (SdB.)", got[1L])
  expect_equal("B11 (ClN.)", got[2L])
  got <- add_in_parens(x, y, 8L, word.wise = TRUE)
  expect_equal("A07 (S.)", got[1L])
  expect_equal("B11 (C.)", got[2L])
  got <- add_in_parens(x, y, 7L, word.wise = TRUE)
  expect_equal("A07", got[1L])
  expect_equal("B11", got[2L])
  got <- add_in_parens(x, y, 7L, word.wise = TRUE, clean = FALSE)
  expect_equal("A07 (.)", got[1L])
  expect_equal("B11 (.)", got[2L])
})


################################################################################
#
# Plate, substrate, well, and curve parameter names
#


test_that("curve parameter names can be mapped", {
  x <- map_grofit_names()
  expect_true(all(CURVE_PARAMS %in% unlist(x)))
  y <- map_grofit_names(opm.fast = TRUE)
  expect_true(!any(names(y) %in% names(x)))
  expect_equivalent(x, y)
})


## UNTESTED: param_names


test_that("plate names can be normalized", {

  # Normal input arguments
  x <- c("<strange>", "PM-M3 A", "PM09", "pm10b", "pmM10D", "PM1")
  exp <- c("<strange>", "PM-M03-A", "PM09", "PM10-B", "PM-M10-D", "PM01")
  got <- normalize_plate_name(x, TRUE)
  expect_equal(got, exp)

  # Strange input arguments
  expect_equal(normalize_plate_name(1:10), as.character(1:10))
  expect_equal(normalize_plate_name(NULL), character())
  
  # The internally used names must already be normalized
  standard.names <- names(PLATE_MAP)
  expect_equal(normalize_plate_name(standard.names), standard.names)
  standard.names <- colnames(WELL_MAP)
  expect_equal(normalize_plate_name(standard.names), standard.names)
  expect_equal(names(PLATE_MAP), colnames(WELL_MAP))

})


## UNTESTED: map_well_names


test_that("substrate names can be translated", {

  plate.1 <- "PM01"
  exp.1 <- c(A01 = "Negative Control", A02 = "L-Arabinose")
  got <- well_to_substrate(plate.1, c("A01", "A02"))

  plates.2 <- c(plate.1, "PM02")
  exp.2 <- c(A01 = "Negative Control", A02 = "Chondroitin Sulfate C")
  exp.2 <- cbind(exp.1, exp.2)
  colnames(exp.2) <- plates.2
  got <- well_to_substrate(plates.2, c("A01", "A02"))
  expect_equal(got, exp.2)

  # Partial matching is allowed
  plates.2 <- c(plate.1, "PM02")
  exp.2 <- c(A01 = "Negative Control", A02 = "Chondroitin Sulfate C")
  exp.2 <- cbind(exp.1, exp.2)
  colnames(exp.2) <- c(plates.2[1L], "PM02")
  got <- well_to_substrate(plates.2, c("A01", "A02"))
  expect_equal(got, exp.2)

})


test_that("substrate names can be searched", {
  
  found <- find_substrate("Fructose", search = "exact")
  expect_is(found, "list")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate"), found[[1L]])

  found <- find_substrate("Fructose", search = "approx")
  expect_is(found, "list")
  expect_equal(1L, length(found))
  expect_equal("Fructose", names(found))
  expect_equal(c("D-Fructose", "D-Fructose-6-Phosphate", "D-Fucose", 
    "L-Fucose"), found[[1L]])

})
  

test_that("substrate names can be searched with patterns", {

  glob.pat <- c("ampic*", "penic*", "random*")
  found <- find_substrate(glob.pat, search = "glob")
  expect_is(found, "list")
  expect_equal(3L, length(found))
  expect_equal(glob.pat, names(found))
  expect_equal("Ampicillin", found[[1L]])
  expect_equal("Penicillin G", found[[2L]])
  expect_equal(character(), found[[3L]])

  reg.pat <- c("^ampic.*", "^penic.*", "^random.*")
  found.2 <- find_substrate(reg.pat, search = "regex")
  expect_equal(reg.pat, names(found.2))
  names(found.2) <- glob.pat
  expect_equal(found, found.2)
  
})


test_that("positions within PM plates can be found", {
  
  query <- c("D-Fructose", "Penicillin G", "This is not present")
  got <- find_positions(query)
  expect_is(got, "list")
  expect_equal(query, names(got))
  expect_true(all(sapply(got, is.matrix)))
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
  expect_true(all(sapply(got, is.matrix)))

})
          

################################################################################
#
# Data selection
#


test_that("rows can be picked", {
  
  x <- data.frame(a = 1:10, b = 11:20, c = letters[1:10], 
    stringsAsFactors = FALSE)

  got <- pick_from(x, list(a = 4:5, b = 14))
  expect_equal(colnames(got), colnames(x))
  expect_equal(dim(got), c(1, 3))
  expect_equal(as.list(got[1, ]), list(a = 4, b = 14, c = "d"))

  got <- pick_from(x, list(a = 4:5, b = 15:14))
  expect_equal(colnames(got), colnames(x))
  expect_equal(dim(got), c(2, 3))
  expect_equal(as.list(got[2, ]), list(a = 5, b = 15, c = "e"))

  got <- pick_from(x, list(a = 4:5, b = 16:17))
  expect_equal(colnames(got), colnames(x))
  expect_equal(dim(got), c(0, 3))

  y <- x[1, , drop = FALSE]
  got <- pick_from(y, list(a = 4:5, b = 14))
  expect_equal(dim(got), c(0L, 3L))
  got <- pick_from(y, list(a = 1:2, b = 11:14))
  expect_equal(dim(got), c(1L, 3L))
  
  y <- rbind(x, c(NA, NA, NA, NA))
  got <- pick_from(y, list(a = 1, b = 11))
  expect_equal(dim(got), c(1L, 3L))
  
  expect_error(pick_from(y, list(a = 1, z = 11)))
  
})


################################################################################
#
# Colors
#


test_that("colors can be assorted", {
  input <- c("white", "grey", "black")
  got <- max_rgb_contrast(input)
  expect_equal(c("grey", "black", "white"), got)
  input <- c("white", "darkgrey", "lightgrey", "black")
  got <- max_rgb_contrast(input)
  expect_equal(c("lightgrey", "black", "white", "darkgrey"), got)
})


## UNTESTED: select_colors default_color_regions


################################################################################


## UNTESTED: draw_ci kubrick


################################################################################
#
# Grouping utilities
#



test_that("logical vectors can be converted to factors", {
  
  x <- c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE) # clean input
  got <- group_by_sep(x)
  expect_equal(as.factor(c(1L, 1L, 1L, 2L, 2L, 3L, 3L)), got)

  got <- group_by_sep(x, include = FALSE)
  expect_equal(as.factor(c(NA, 1L, 1L, NA, 2L, NA, 3L)), got)
  
  x <- c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE) # leading FALSE
  got <- group_by_sep(x)
  expect_equal(as.factor(c(1L, 2L, 2L, 2L, 3L, 3L, 4L, 4L)), got)
  
  # adjacent TRUEs and trailing TRUE
  x <- c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  got <- group_by_sep(x)
  expect_equal(as.factor(c(1L, NA, 2L, 2L, 3L, 3L, NA, 4L, 4L, 5L)), got)  
  
  # several adjacent TRUEs
  x <- c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
  got <- group_by_sep(x)
  expect_equal(as.factor(c(1L, NA, NA, NA, 2L, 2L, 3L, 3L)), got)
  
})



test_that("character vectors can be converted to factors", {
  
  x <- c(">abc", ">def", "acgtagg", ">hij", "gatattag", "aggtagga") # FASTA
  got <- group_by_sep(x, "^>")
  expect_equal(as.factor(c(NA, 1L, 1L, 2L, 2L, 2L)), got)
  got <- group_by_sep(x, "^>", include = FALSE)
  expect_equal(as.factor(c(NA, NA, 1L, NA, 2L, 2L)), got)
  
})


