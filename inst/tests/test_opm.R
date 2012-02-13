

library(testthat)


# IO testing is done in 'test_io.R' , with the exception of read_single_opm() 
# applied to example file 1 in new CSV format, which is done here because some 
# input data are needed.
#
context("Testing the methods of the OPM and OPMA classes.")


loc <- source_location()
if (is.null(loc))
  loc <- .path.package("opm")
  
EXAMPLE.FILE.1 <- file.path(test_file_dir(loc), "Example_1.csv.xz")
OPM.1 <- read_single_opm(EXAMPLE.FILE.1)
ORGN <- "Bacillus simplex"
MD <- data.frame(File = filename(OPM.1), Position = position(OPM.1),
  `Setup Time` = setup_time(OPM.1), Organism = ORGN, 
   check.names = FALSE, stringsAsFactors = FALSE)
MD[2L, ] <- rep(NA_character_, ncol(MD))
OPM.WITH.MD <- include_metadata(OPM.1, MD, remove.csv.data = FALSE)
SMALL <- OPM.1[, 1L:10L]
SMALL.WITH.MD <- OPM.WITH.MD[, 1L:10L]
SMALL.AGG <- do_aggr(SMALL, boot = 0L, cores = 1L)


################################################################################
#
# Getter functions for the measurements
#


## UNTESTED: mesurements [see the example]


test_that("data from example file 1 can be subset", {
  expect_is(OPM.1, "OPM")
  small <- OPM.1[31:40, 11:20]
  expect_is(small, "OPM")
  expect_equal(dim(small), c(10, 10))
  small <- OPM.1[31:35, ]
  expect_is(small, "OPM")
  expect_equal(dim(small), c(5, 96))
  small <- OPM.1[, 11:20]
  expect_is(small, "OPM")
  expect_equal(dim(small), c(384, 10))
})

  
test_that("data from example file 1 can be thinned out", {
  expect_error(thin_out(OPM.1, 0.5), "'factor' must be >= 1")
  thin <- thin_out(OPM.1, 1)
  expect_equal(OPM.1, thin)
  thin <- thin_out(OPM.1, 2)
  dims <- dim(thin)
  dims[1L] <- dims[1] * 2
  expect_equal(dims, dim(OPM.1))
})


## UNTESTED: well, wells [see the examples]


test_that("measurements from example file 1 can be explicitely queried", {
  expect_equal(hours(OPM.1), 95.75) # see also the examples
  expect_equal(dim(OPM.1), c(384, 96))
  expect_equal(351, max(OPM.1))
  expect_equal(56, max(OPM.1, 'A01'))
  expect_equal(56, max(OPM.1, 1L))
  expect_equal(15, minmax(OPM.1))
  expect_equal(56, minmax(OPM.1, 'A01'))
  expect_equal(56, minmax(OPM.1, 1L))
})


################################################################################
#
# Getter and setter functions for the CSV data
#
  
  
test_that("CSV data from example file 1 can be accessed", {
  
  expect_equal(length(csv_data(OPM.1)), 10L)
  expect_equal(length(csv_data(OPM.WITH.MD)), 10L)
  expect_equal(length(csv_data(SMALL)), 10L)
  expect_equal(length(csv_data(SMALL.WITH.MD)), 10L)
  expect_equal(length(csv_data(SMALL.AGG)), 10L)
  
  # Picking CSV data
  picked <- csv_data(OPM.1, c("File", "Setup Time"))
  expect_is(picked, "character")
  expect_equivalent(picked, c(filename(OPM.1), setup_time(OPM.1)))
  missing.key <- "19825761285616"
  error.msg <- paste("could not find key", missing.key)
  expect_error(csv_data(OPM.1, c("File", missing.key)), error.msg)
 
})
  
  
test_that("CSV data from example file 1 can be explicitely queried", {
  expect_equal(filename(OPM.1), EXAMPLE.FILE.1)
  expect_equal(plate_type(OPM.1), "PM01")
  expect_equal(setup_time(OPM.1), "8/30/2010 11:28:54 AM")
  expect_equal(position(OPM.1), "21-B")
})


test_that("the plate type can be changed to generation 3", {
  gen.3 <- gen_iii(OPM.1)
  expect_is(gen.3, "OPM")
  expect_equal(plate_type(gen.3), "Gen III")
  expect_equal(metadata(gen.3), metadata(OPM.1))
  expect_equal(length(which(csv_data(gen.3) != csv_data(OPM.1))), 1L)
})


################################################################################
#
# Other getter functions
#

test_that("we can ask for aggregated values", {
  expect_false(has_aggr(OPM.1))  
})


test_that("we can print a summary", {
  expect_that(x <- summary(OPM.1), shows_message())  
  expect_is(x, "list")
  expect_true(length(x) > 7L)
})


################################################################################
#
# Metadata functions (including the WMD ones)
#
 

## UNTESTED: collect_template [see the examples]


test_that("missing metadata result in an error if requested", {
  expect_is(OPM.1, "OPM")
  expect_equal(metadata(OPM.1), list())
  expect_equal(metadata(OPM.1, "Organism"), NULL)
  expect_error(metadata(OPM.1, "Organism", strict = TRUE))
})


test_that("metadata can be included in object from from example file 1", {
  expect_is(OPM.WITH.MD, "OPM")
  exp.list <- list(File = filename(OPM.1), Organism = ORGN)
  expect_equal(metadata(OPM.WITH.MD), exp.list)
  expect_equal(metadata(OPM.WITH.MD, "Organism"), ORGN)
  expect_equal(metadata(OPM.WITH.MD, list("File", "Organism")), exp.list)
  exp.list$Organism <- NULL
  exp.list$Org <- ORGN
  expect_equal(metadata(OPM.WITH.MD, list("File", "Org"), exact = FALSE),
    exp.list)
  exp.list$Org <- NULL
  exp.list <- c(exp.list, list(Org = NULL))
  expect_equal(metadata(OPM.WITH.MD, list("File", "Org"), exact = TRUE),
    exp.list)
  bad.md <- MD
  bad.md$Position[1L] <- "does not exist"
  expect_error(include_metadata(OPM.1, bad.md))
})


test_that("metadata can be included and CSV keys removed", {
  x <- include_metadata(OPM.1, MD)
  expect_is(x, "OPM")
  expect_equal(metadata(x), list(Organism = ORGN))
})


test_that("metadata can be queried with infix-littlek", {
  
  expect_false("Organism" %k% OPM.1)
  expect_false("Organism" %K% OPM.1)
  
  expect_true("Organism" %k% OPM.WITH.MD)
  expect_true(c("Organism", "File") %k% OPM.WITH.MD)
  expect_false("not there" %k% OPM.WITH.MD)
  expect_true(list(Organism = "dummy", File = "dummy") %k% OPM.WITH.MD)
  expect_false(list(`not there` = "dummy") %k% OPM.WITH.MD)
  
})


test_that("metadata can be queried with infix-largek", {
  
  expect_false("Organism" %K% OPM.1)
  expect_false("Organism" %K% OPM.1)
  
  expect_true("Organism" %K% OPM.WITH.MD)
  expect_false(c("Organism", "File") %K% OPM.WITH.MD)
  expect_false("not there" %K% OPM.WITH.MD)
  expect_true(list(Organism = "dummy", File = "dummy") %K% OPM.WITH.MD)
  expect_false(list(`not there` = "dummy") %K% OPM.WITH.MD)

})


test_that("metadata can be queried with infix-littleq", {

  expect_false(c(Organism = "Bacillus simplex") %q% OPM.1)
  expect_true(c(Organism = "Bacillus simplex") %q% OPM.WITH.MD)

  # Factors should be recognized
  x <- OPM.WITH.MD
  metadata(x, "Organism") <- as.factor(metadata(x, "Organism"))
  expect_true(c(Organism = "Bacillus simplex") %q% x)
  expect_true(list(Organism = "Bacillus simplex") %q% x)
  
  expect_true(character() %q% OPM.WITH.MD)
  expect_false(c(Organism = "Bacillus subtilis") %q% OPM.WITH.MD)
  expect_false(list(Organism = "Bacillus subtilis") %q% OPM.WITH.MD)
  expect_false(c(`not there` = "missing") %q% OPM.WITH.MD)
  expect_false("missing" %q% OPM.WITH.MD)
  expect_false(list(`not there` = "missing") %q% OPM.WITH.MD)

})


test_that("metadata can be queried with infix-largeq", {
  expect_false(c(Organism = "Bacillus simplex") %Q% OPM.1)
  expect_true(c(Organism = "Bacillus simplex") %Q% OPM.WITH.MD)
  # Factors should not be recognized in strict mode
  x <- OPM.WITH.MD
  metadata(x, "Organism") <- as.factor(metadata(x, "Organism"))
  expect_false(c(Organism = "Bacillus simplex") %Q% x)
  expect_false(list(Organism = "Bacillus simplex") %Q% x)
})


test_that("metadata can be added individually", {
  x <- OPM.WITH.MD
  got <- metadata(x) <- list(A = 99)
  expect_equal(metadata(x), list(A = 99))
  expect_equal(got, list(A = 99))
  got <- metadata(x, "B") <- 40
  expect_equal(got, 40)
  expect_equal(metadata(x), list(A = 99, B = 40))
  got <- metadata(x, list(C = "K")) <- list(67)
  expect_equal(got, list(67))
  expect_equal(metadata(x), list(A = 99, B = 40, K = 67))
  got <- metadata(x, list("H", "I")) <- list(I = 9, H = "f")
  expect_equal(got, list(I = 9, H = "f"))
  expect_equal(metadata(x), list(A = 99, B = 40, K = 67, H = "f", I = 9))
  got <- metadata(x, c("A", 'Z')) <- -99
  expect_equal(metadata(x, "A"), c(99, Z = -99))
  expect_equal(got, -99)
})


test_that("metadata characters can be received", {
  
  got <- metadata_chars(OPM.WITH.MD)
  exp <- sort(c(ORGN, EXAMPLE.FILE.1))
  names(exp) <- exp
  expect_equal(got, exp)
  
  x <- OPM.WITH.MD
  metadata(x, "run") <- 4L
  got <- metadata_chars(x)
  expect_equal(got, exp)
  got <- metadata_chars(x, coerce = "integer")
  exp <- sort(c(structure(4L, names = 4L), exp))
  expect_equal(got, exp)
  
  got <- metadata_chars(x, coerce = "not.relevant", values = FALSE)
  exp <- sort(names(metadata(x)))
  names(exp) <- exp
  expect_equal(got, exp)  
  
})


test_that("metadata can be mapped using a character vector", {
  map <- structure("Elephas maximus", names = ORGN)
  x <- map_metadata(OPM.WITH.MD, map)
  expect_equal(metadata(x)$Organism, "Elephas maximus")
})


test_that("metadata can be mapped using a function", {
  map <- identity
  data <- map_metadata(OPM.WITH.MD, map)
  expect_equal(metadata(data), metadata(OPM.WITH.MD))
  data <- map_metadata(OPM.WITH.MD, map, values = FALSE)
  expect_equal(metadata(data), metadata(OPM.WITH.MD))
  
  map <- function(x) rep('x', length(x))
  data <- map_metadata(OPM.WITH.MD, map)
  expect_equal(metadata(data), list(File = 'x', Organism = 'x'))
  
  # Modify only the selected classes
  map <- function(y) rep('y', length(y))
  data <- map_metadata(data, map, classes = "integer")
  expect_equal(metadata(data), list(File = 'x', Organism = 'x'))
  data <- map_metadata(data, map, classes = "character")
  expect_equal(metadata(data), list(File = 'y', Organism = 'y'))
  
  # And now the keys
  data <- map_metadata(OPM.WITH.MD, map, values = FALSE)
  expect_equal(names(metadata(data)), rep("y", 2L))
})
  

################################################################################
#
# Conversion functions: OPM <=> lists (including conversion to YAML)
#


test_that("data from example file 1 can be converted to a list", {
  
  # Converting to list and back
  opm.list <- as(OPM.1, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPM")
  expect_equal(OPM.1, opm.back)
  
  # Converting with metadata to list and back
  opm.list <- as(OPM.WITH.MD, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPM")
  expect_equal(OPM.WITH.MD, opm.back)

  # Converting aggregated stuff to list and back
  opm.list <- as(SMALL.AGG, "list")
  expect_is(opm.list, "list")
  opm.back <- as(opm.list, "OPMA")
  expect_equal(SMALL.AGG, opm.back)
  
})


test_that("data from example file 1 can be converted to YAML", {
  yaml.str <- to_yaml(SMALL)
  lines <- strsplit(yaml.str, "\n", fixed = TRUE)[[1]]
  expect_equal("---", lines[1L])
  expect_equal("metadata: []", lines[2L])
  for (name in c("measurements:", "csv_data:")) {
    pos <- which(lines == name)
    expect_equal(1L, length(pos))
    expect_false(identical(lines[pos + 1L], "  []"))
  }
})


test_that("data from example file 1 with metadata can be converted to YAML", {
  yaml.str <- to_yaml(SMALL.WITH.MD)
  lines <- strsplit(yaml.str, "\n", fixed = TRUE)[[1]]
  expect_equal("---", lines[1L])
  for (name in c("measurements:", "csv_data:", "metadata:")) {
    pos <- which(lines == name)
    expect_equal(1L, length(pos))
    expect_false(identical(lines[pos + 1L], "  []"))
  }
})


test_that(
    "data from example file 1 with aggregated data can be converted to YAML", {
  yaml.str <- to_yaml(SMALL.AGG)
  lines <- strsplit(yaml.str, "\n", fixed = TRUE)[[1]]
  expect_equal("---", lines[1L])
  expect_equal("metadata: []", lines[2L])
  for (name in c("aggregated:", "aggr_settings:", "measurements:", 
      "csv_data:")) {
    pos <- which(lines == name)
    expect_equal(1L, length(pos))
    expect_false(identical(lines[pos + 1L], "  []"))
  }
})


################################################################################
#
# Curve parameter estimation
#


test_that("data from example file 1 can be aggregated", {
  expect_equal(length(csv_data(SMALL.AGG)), 10L)
  expect_is(SMALL, "OPM")
  expect_false(is(SMALL, "OPMA"))
  expect_is(SMALL.AGG, "OPM")
  expect_is(SMALL.AGG, "OPMA")
  expect_false(has_aggr(SMALL))
  expect_true(has_aggr(SMALL.AGG))
})
  
  
test_that("aggregated data can be queried", {
  settings <- aggr_settings(SMALL.AGG)
  expect_is(settings, "list")
  expect_equal(length(settings), 2L)
  expect_equivalent(names(settings), c("program", "options"))
  aggr <- aggregated(SMALL.AGG)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(12L, 10L))
  aggr <- aggregated(SMALL.AGG, subset = "mu")
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(3L, 10L))
  aggr <- aggregated(SMALL.AGG, subset = c("mu", "AUC"), ci = FALSE)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(2L, 10L))
  aggr <- aggregated(SMALL.AGG, subset = c("mu", "lambda", "AUC"), ci = TRUE)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(SMALL.AGG))
  expect_equal(dim(aggr), c(9L, 10L))
})
  

test_that("data from example file 1 can be aggregated using the fast method", {
  fast.agg <- do_aggr(SMALL, program = "opm-fast")
  expect_equal(length(csv_data(fast.agg)), 10L)
  expect_is(SMALL, "OPM")
  expect_false(is(SMALL, "OPMA"))
  expect_is(fast.agg, "OPM")
  expect_is(fast.agg, "OPMA")
  expect_false(has_aggr(SMALL))
  expect_true(has_aggr(fast.agg))
  expect_equal(colnames(aggregated(SMALL.AGG)), colnames(aggregated(fast.agg)))
  expect_equal(rownames(aggregated(SMALL.AGG)), rownames(aggregated(fast.agg)))
})


test_that("data aggregated using the fast method can be queried", {
  fast.agg <- do_aggr(SMALL, program = "opm-fast")
  settings <- aggr_settings(fast.agg)
  expect_is(settings, "list")
  expect_equal(length(settings), 2L)
  expect_equivalent(names(settings), c("program", "options"))
  aggr <- aggregated(fast.agg)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(fast.agg))
  expect_equal(dim(aggr), c(12L, 10L))
  aggr <- aggregated(fast.agg, subset = "mu")
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(fast.agg))
  expect_equal(dim(aggr), c(3L, 10L))
  aggr <- aggregated(fast.agg, subset = c("mu", "AUC"), ci = FALSE)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(fast.agg))
  expect_equal(dim(aggr), c(2L, 10L))
  aggr <- aggregated(fast.agg, subset = c("mu", "lambda", "AUC"), ci = TRUE)
  expect_is(aggr, "matrix")
  expect_equal(colnames(aggr), wells(fast.agg))
  expect_equal(dim(aggr), c(9L, 10L))
})


test_that("objects with aggregated data can be subset", {
  
  # Keep the aggregated data
  tiny <- SMALL.AGG[, 1L:5L]
  expect_is(tiny, "OPMA")
  expect_equal(csv_data(tiny), csv_data(SMALL.AGG))
  expect_equal(metadata(tiny), metadata(SMALL.AGG))
  expect_equal(wells(tiny), colnames(aggregated(tiny)))

  # Drop the aggregated data
  tiny <- SMALL.AGG[, 1L:5L, drop = TRUE]
  expect_is(tiny, "OPM")
  expect_equal(csv_data(tiny), csv_data(SMALL.AGG))
  expect_equal(metadata(tiny), metadata(SMALL.AGG))

})


################################################################################
#
# Plots
#


test_that("data from example file 1 can be flattened", {
  base.colnames <- c("Time", "Well", "Value")
  flat <- flatten(SMALL)
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), base.colnames)
  w <- wells(SMALL, full = TRUE)
  expect_equal(as.character(unique(flat$Well)), w)
  exp.len <- Reduce(`*`, dim(SMALL))
  expect_equal(exp.len, nrow(flat))
})


test_that("data from example file 1 can be flattened with metadata", {
  base.colnames <- c("Time", "Well", "Value")
  flat <- flatten(SMALL.WITH.MD, include = "Organism")
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("Organism", base.colnames))
  exp.len <- Reduce(`*`, dim(SMALL.WITH.MD))
  expect_equal(exp.len, nrow(flat))
  orgn <- as.character(unique(flat[, "Organism"]))
  expect_equal(orgn, ORGN)
})


test_that("data from example file 1 can be flattened with fixed entries", {
  base.colnames <- c("Time", "Well", "Value")
  flat <- flatten(SMALL.WITH.MD, fixed = list(A = 33, B = "zzz"))
  expect_is(flat, "data.frame")
  expect_equal(colnames(flat), c("A", "B", base.colnames))
  exp.len <- Reduce(`*`, dim(SMALL.WITH.MD))
  expect_equal(exp.len, nrow(flat))
  content.a <- unique(flat[, "A"])
  expect_equal(content.a, 33)
  content.b <- as.character(unique(flat[, "B"]))
  expect_equal(content.b, "zzz")
})

  
test_that("data from example file 1 can be plotted", {
  
  expect_equal(length(csv_data(SMALL)), 10L)
  
  # Draw levelplot
  expect_error(got <- level_plot(SMALL, colors = "black"), 
    "need at least two non-NA values to interpolate")
  got <- level_plot(SMALL)
  expect_is(got, "trellis")
  
  # Draw xyplot
  got <- xy_plot(SMALL)
  expect_is(got, "trellis")
  
})





