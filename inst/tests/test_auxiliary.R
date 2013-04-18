

library(testthat)
context("Testing the helper functions of the OPM package")


################################################################################
################################################################################
#
# Miscellaneous helper functions
#


## opm_string
## UNTESTED


## last
test_that("an objects last elements can be accessed", {
  expect_equal(c("x", "y", "z"), last(letters, 3))
  expect_equal(letters, last(letters, 26))
  expect_error(last(letters, 27))
})


## md_data_frame
## UNTESTED


## reduce_to_mode
## UNTESTED


## is_uniform
test_that("uniformity can be checked", {
  x <- list(a = 1:2, b = 1:2, a = 1:2)
  expect_true(isTRUE(is_uniform(x)))
  x <- c(x, list(c = 1:6))
  expect_false(isTRUE(isuni <- is_uniform(x)))
  expect_equal(isuni, x[c(1L, 4L)])
})


## is_constant
test_that("constantness can be checked", {

  x <- list(a = 1:2, b = 1:2, a = 1:2)
  expect_true(is_constant(x))

  x <- c(x, list(c = 1:6))
  expect_false(is_constant(x))
  # expect_true(is_constant(x, set.like = TRUE))

  y <- list(a = 1:2, b = 1, c = 2)
  expect_false(is_constant(y))
  # expect_false(is_constant(y, set.like = TRUE))

  x <- matrix(c(1, 2, 3, 2, 2, 2), ncol = 2)
  expect_equal(is_constant(x), c(FALSE, TRUE, FALSE))
  expect_equal(is_constant(x, 2), c(FALSE, TRUE))
  expect_equal(is_constant(x, 0), FALSE)
  expect_error(is_constant(x, 3))

})


## pick_from
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


## assert_splittable_matrix
## UNTESTED


################################################################################
################################################################################
#
# String processing
#


## create_formula
## UNTESTED


## metadata_key
test_that("we can convert formulas for use as metadata keys", {

  var <- c("A", "B")
  f <- ~ a $ b $ c + I(var) * ("d" + e) + c("f", "g", "h") | i$"j"
  got <- metadata_key(f, TRUE)
  expect_equal(got, ~ a.b.c + A.B * (d + e) + c(f, g, h) | i.j)
  got <- metadata_key(f, FALSE)
  expect_equal(got, list(a.b.c = c("a", "b", "c"), A.B = c("A", "B"),
    d = "d", e = "e", f = "f", g = "g", h = "h", i.j = c("i", "j")))
  got <- metadata_key(f, FALSE, remove = c("A.B", "i.j"))
  expect_equal(got, list(a.b.c = c("a", "b", "c"),
    d = "d", e = "e", f = "f", g = "g", h = "h"))

  f <- Value ~ Well
  got <- metadata_key(f, FALSE)
  expect_equal(got, c(Well = "Well"))
  got <- metadata_key(f, FALSE, remove = RESERVED_NAMES)
  expect_equal(got, NULL)

})


## metadata_key
test_that("we can convert lists for use as formulas", {

  x <- list(c("a", "b"), list(K = "t", I = c("D", "E")))
  got <- metadata_key(x, TRUE)
  expect_equal(got, ~ a.b + K + I)
  got <- metadata_key(x, TRUE, ops = c("+", "|"))
  expect_equal(got, ~ a.b + K | I)
  got <- metadata_key(x, TRUE, remove = "K")
  expect_equal(got, ~ a.b + I)

  x <- list("run")
  got <- metadata_key(x, TRUE)
  expect_equal(got, ~ run)

})


## metadata_key
test_that("some edge cases are correctly handled", {
  x <- character()
  names(x) <- character()
  expect_error(metadata_key(x, TRUE))
  expect_equal(x, metadata_key(x, FALSE))
  expect_equal(NULL, metadata_key(NULL, FALSE))
  x <- numeric()
  got <- metadata_key(x, TRUE)
  expect_equal(metadata_key(got, TRUE), got)
  x <- ~ list(list(), list())
  expect_equal(x, metadata_key(x, TRUE))
})


## parse_time
test_that("time strings can be parsed", {
  expect_warning(got <- parse_time(c("11.11.1911 11:11:11", "xxx")))
  expect_is(got, "POSIXlt")
  expect_equal(c(FALSE, TRUE), is.na(got))
})


## separate
test_that("character vectors can be split regularly", {

  # a typical usage example: a bunch of filenames from which the settings
  # under which they have been created are to be extracted
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

  got <- separate(x, NULL)
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

## separate
test_that("character vectors can be split regularly even if constant", {

  x <- paste("x", letters)
  got <- separate(x, keep.const = TRUE, split = " ")
  expect_is(got, "matrix")
  expect_equal(dim(got), c(26L, 2L))
  got <- separate(x, keep.const = FALSE, split = " ")
  expect_is(got, "matrix")
  expect_equal(dim(got), c(26L, 1L))
  got <- separate(x, keep.const = FALSE, split = " ", simplify = TRUE)
  expect_is(got, "character")
  expect_equal(got, letters)

  x <- paste("x", "y")
  got <- separate(x, keep.const = FALSE, split = " ", simplify = TRUE)
  expect_is(got, "character")
  expect_identical(got, "x")
  got <- separate(x, keep.const = FALSE, split = " ", simplify = FALSE)
  expect_is(got, "matrix")
  expect_equal(dim(got), c(1L, 0L))

})

## separate
test_that("character vectors can be split regularly with multiple separators", {
  x <- c(
    "ibb__blastall.sim2_IS.log",
    "ibb_blastall..sim2_LS.log",
    "ibb_blastall.sim2_SS.log"
    )
  got <- separate(x, keep.const = TRUE, split = "_.", simplify = TRUE)
  expect_is(got, "matrix")
  expect_equal(dim(got), c(3, 5))
})

## separate
test_that("character vectors can be split regularly in list-wise mode", {
  x <- c(
    "a, b",
    "a, c",
    NA_character_,
    "c, d, a"
    )
  got <- separate(x, keep.const = FALSE, split = ",", simplify = TRUE,
    list.wise = TRUE)
  expect_is(got, "matrix")
  expect_equal(dim(got), c(4, 3))
  expect_equal(colnames(got), c("b", "c", "d"))
  expect_true(all(is.na(got[3L, ])))
  expect_is(got, "matrix")
  got <- separate(x, keep.const = TRUE, split = ",", simplify = TRUE,
    list.wise = TRUE)
  expect_equal(dim(got), c(4, 4))
  expect_equal(colnames(got), c("a", "b", "c", "d"))
})

## separate
test_that("factors can be split regularly", {
  x <- as.factor(c(
    "ibb_blastall.sim2_IS.log",
    "ibb_blastall.sim2_LS.log",
    "ibb_blastall.sim2_SS.log",
    "ibb_blat.sim2_IS.log",
    "ibb_blat.sim2_LS.log",
    "ibb_blat.sim2_SS.log",
    "ibb_megablast.sim2_IS.log",
    "ibb_megablast.sim2_LS.log",
    "ibb_megablast.sim2_SS.log"
    ))
  got <- separate(x)
  expect_is(got, "data.frame")
  expect_equal(dim(got), c(9L, 5L))
  got <- separate(x, keep.const = FALSE)
  expect_is(got, "data.frame")
  expect_equal(dim(got), c(9L, 2L))
})

## separate
## UNTESTED (data-frame method)


## glob_to_regex
test_that("wildcards can be converted to regular expressions", {
  # from http://docstore.mik.ua/orelly/perl/cookbook/ch06_10.htm
  # with some adaptations and
  x <- c("list.?", "project.*", "*old", "type*.[ch]", "*.*", "*")
  wanted <- c("^list\\..$", "^project\\.", "^.*old$", "^type.*\\.\\[ch]$",
    "^.*\\.", "^")
  got <- glob_to_regex(x)
  expect_equal(wanted, got)
  x <- c("^anc-+k", "+us$hs+")
  got <- glob_to_regex(x)
  expect_equal(c("^\\^anc-\\+k$", "^\\+us\\$hs\\+$"), got)
})


## trim_string
test_that("strings can be trimmed", {
  x <- c("abcd", "a", "", "xy-", "zzz")
  got <- trim_string(x, 2)
  expect_equal(got, c("a.", "a", "", "x.", "z."))
  got.2 <- trim_string(x, 2, word.wise = TRUE)
  expect_equal(got, got.2)
})


## add_in_parens
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

## add_in_parens
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


## html_head
## UNTESTED


## tidy
## UNTESTED


################################################################################
################################################################################
#
# Easter eggs
#


## kubrick
test_that("Stanley Kubrick is honoured", {
  expect_message(kubrick())
})


################################################################################
################################################################################
#
# Mapping functions
#


## as
## UNTESTED


## prepare_class_names
## UNTESTED


## map_values
test_that("values in character vectors can be mapped", {
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


## map_values
test_that("values in lists can be mapped using character vectors", {

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


## map_values
test_that("values in lists can be mapped by cleaning", {
  x <- list(A = 13, B = list(B1 = NULL, B2 = -5), C = "z", character(),
    D = list(D1 = NULL, D2 = list()))
  got <- map_values(x, NULL)
  expect_equal(got, list(A = 13, B = list(B2 = -5), C = "z"))
  got <- map_values(x, NULL, "numeric")
  expect_equal(got, list(A = "13", B = list(B2 = "-5"), C = "z"))
})


## map_values
test_that("values in lists can be mapped using expressions", {
  x <- list(a = 1:5, b = letters[1:3], K = list(K1 = 3, 89))
  assign("z", 7.5, 1)
  # 1
  got <- map_values(x, expression(a <- a, u <- a + z))
  expect_equal(got, c(x, list(u = x$a + z)))
  # 2
  expect_error(map_values(x, expression(u <- a + z), baseenv()))
  # 3
  got <- map_values(x, expression(u <- a + z, v <- u))
  expect_equivalent(got, c(x, list(u = x$a + z, v = x$a + z)))
  # 4
  b <- 4
  got <- map_values(x, expression(rm(b)))
  x$b <- NULL
  expect_equal(got, x)
  expect_equal(b, 4)
  # 5
  got <- map_values(x, expression(b <- NULL))
  expect_equal(got, c(x, list(b = NULL)))
})


## map_names
test_that("names in lists can be mapped and received", {
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

## map_names
test_that("names in lists with missing names can be mapped", {
  x <- list(a = list(1:2, 5:6), b = 3:8)
  map <- c(a = "A", b = "B")
  got <- map_names(x, map)
  expect_equal(got, list(A = list(1:2, 5:6), B = 3:8))
  got <- map_names(x, toupper)
  expect_equal(got, list(A = list(1:2, 5:6), B = 3:8))
  x <- list(list(), list())
  expect_equivalent(character(), map_names(x))
})


################################################################################
################################################################################
#
# YAML reparation
#

## repair_na_strings
test_that("NAs in a character vectors can be repaired", {
  x <- c("abc", " ", "NA", " NA", "           NA", "123", "NA ")
  got <- repair_na_strings(x)
  expect_equal(got, c("abc", " ", NA, NA, NA, "123", "NA "))
})

## repair_na_strings
test_that("NAs in a list can be repaired", {

  x <- list(a = 99, b = list(xx = c("NA", "99.5", "1e+06")), c = 8,
    d = c("NA", "Z"))
  wanted <- list(a = 99, b = list(xx = c(NA_real_, 99.5, 1000000)), c = 8,
    d = c(NA, "Z"))

  got <- repair_na_strings(x)
  expect_equal(wanted, got)

  got <- repair_na_strings(x, "double")
  expect_equal(wanted, got)

  got <- repair_na_strings(x, "integer")
  wanted$b$xx <- c(NA_integer_, as.integer(x$b$xx[2L:3L]))
  expect_equal(wanted, got)

  got <- repair_na_strings(x, "complex")
  wanted$b$xx <- c(NA_complex_, as.complex(x$b$xx[2L:3L]))
  expect_equal(wanted, got)

})


################################################################################
################################################################################
#
# Lists
#


## insert
test_that("a list can be inserted in a list", {
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

## insert
test_that("anything and nothing can be inserted in a list", {
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


## contains
test_that("a list can be queried with a list with exact matches", {

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

## contains
test_that("a list can be queried with a list without matches", {

  x <- list(a = 99, list(i = 1, j = 2), d = 1:2, c = list(99, y = 100, z = 101))

  query <- list(b = 99, c = list(z = 101))
  expect_false(contains(x, query, values = TRUE, exact = TRUE))
  expect_false(contains(x, query, values = FALSE, exact = TRUE))
  expect_false(contains(x, query, values = TRUE, exact = FALSE))
  expect_false(contains(x, query, values = FALSE, exact = FALSE))

})

## contains
test_that("a list can be queried with a list with only non-exact matches", {

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

## contains
test_that("a list can be queried with a list with missing names", {

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
################################################################################
#
# Global options
#

## opm_opt
## UNTESTED



################################################################################
################################################################################
#
# update()
#


## update
test_that("character-matrix objects can be updated by converting NAs", {

  data <- matrix(1:10, ncol = 2L)
  rownames(data) <- paste("taxon", 1L:5L, sep = "_")
  colnames(data) <- paste("char", 1L:2L, sep = "_")
  data <- discrete(data, TRUE, TRUE)
  data <- as(data, CMAT)

  for (x in list(data, data + 1L, data * 2L)) {
    expect_true(any(is.na(x)))
    got <- update(x)
    expect_equal(length(unique(c(x))), length(unique(c(got))))
    expect_false(any(is.na(got)))

    expect_true(all(got[is.na(x)] > min(got, na.rm = TRUE)))
    expect_true(all(got[is.na(x)] < max(got, na.rm = TRUE)))

    wanted <- x == min(x, na.rm = TRUE)
    wanted <- wanted & !is.na(wanted)
    expect_true(all(x[wanted] == got[wanted]))

    wanted <- max(x, na.rm = TRUE)
    wanted <- x == wanted
    wanted <- wanted & !is.na(wanted)
    expect_true(all(x[wanted] <= got[wanted]))
  }

})

## update
test_that("character-matrix objects can be updated by deletion", {

  data <- matrix(1:10, ncol = 2L)
  rownames(data) <- paste("taxon", 1L:5L, sep = "_")
  colnames(data) <- paste("char", 1L:2L, sep = "_")
  data <- discrete(data, TRUE, TRUE)
  data <- as(data, CMAT)

  got <- update(data, "delete.ambig")
  expect_equal(got, data) # no ambiguities
  got <- update(data, "delete.ambig")
  expect_equal(got, data, na.rm = FALSE) # na.rm no effect here

  got <- update(data, "delete.const")
  expect_equal(length(got), 0L)
  got <- update(data, "delete.const")
  expect_equal(length(got), 0L, na.rm = FALSE)

  got <- update(data, "delete.uninf")
  expect_equal(length(got), 0L)
  got <- update(data, "delete.uninf")
  expect_equal(length(got), 0L, na.rm = FALSE)

  data <- merge(data, c(1, 1, 2, 2, 2))

  got <- update(data, "delete.ambig")
  expect_equal(got, data)
  got <- update(data, "delete.ambig", na.rm = FALSE)
  expect_equal(dim(got), c(2, 1))

  got <- update(data, "delete.const")
  expect_equal(dim(got), c(2, 0))
  got <- update(data, "delete.const", na.rm = FALSE)
  expect_equal(got, data)

  got <- update(data, "delete.uninf")
  expect_equal(dim(got), c(2, 0))
  got <- update(data, "delete.uninf", na.rm = FALSE)
  expect_equal(dim(got), c(2, 1))

})


