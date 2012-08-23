#!/usr/local/bin/Rscript --vanilla


################################################################################
#
# run_opm.R -- R script for non-interactive use of the opm package
#
# (C) 2012 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This script is distributed under the terms of the GPL. For further details
# see the opm package.
#
################################################################################


library(optparse, quietly = TRUE, warn.conflicts = FALSE)
library(opm, quietly = TRUE, warn.conflicts = FALSE)


MD.OUTFILE <- "metadata.csv"
RESULT.VALUES <- c("clean", "plot", "split", "template", "yaml")
RESULT.DESCS <- c(
  "Clean filenames by removing non-word characters except dots and dashes.",
  "Draw plots as postscript files, one per input file.",
  "Split OmniLog(R) CSV files into one file per plate.",
  "Collect a template for adding metadata.",
  "Convert input OmniLog(R) CSV (or opm YAML) files to opm YAML."
)
names(RESULT.DESCS) <- RESULT.VALUES


################################################################################
#
# Functions for each running mode
#


run_clean_mode <- function(input, opt) {
  files <- explode_dir(input, include = opt$include, exclude = opt$exclude)
  clean_filenames(files, overwrite = opt$overwrite == "yes")
}


#-------------------------------------------------------------------------------


run_plot_mode <- function(input, opt) {
  if (!nzchar(opt$dir))
    opt$dir <- NULL
  plot_fun <- if (opt$level)
    level_plot
  else
    xy_plot
  io_fun <- function(infile, outfile) {
    x <- read_opm(infile, gen.iii = opt$Gen3)
    postscript(outfile)
    print(plot_fun(x))
    dev.off()
  }
  batch_process(names = input, proc = opt$processes, out.ext = "ps", 
    io.fun = io_fun, outdir = opt$dir, verbose = !opt$quiet, 
    overwrite = opt$overwrite, include = opt$include, exclude = opt$exclude)
}


#-------------------------------------------------------------------------------


run_split_mode <- function(input, opt) {
  files <- explode_dir(input, include = opt$include, exclude = opt$exclude)
  split_files(files, pattern = '^("Data File",|Data File)', outdir = opt$dir)
}


#-------------------------------------------------------------------------------


run_template_mode <- function(input, opt) {
  mdfile <- opt$mdfile
  if (is.null(mdfile)) {
    message(sprintf("NOTE: Using default metadata template outfile '%s'.",
      MD.OUTFILE))
    mdfile <- MD.OUTFILE
    previous <- if (file.exists(mdfile))
      mdfile
    else
      NULL
  } else
    previous <- mdfile
  collect_template(object = input, outfile = mdfile, previous = previous,
    include = opt$include, exclude = opt$exclude, sep = opt$sep)
}


#-------------------------------------------------------------------------------


run_yaml_mode <- function(input, opt) {
  if (opt$coarse || opt$fast) {
    proc <- opt$processes
    opt$processes <- 1L
  } else
    proc <- 1L
  if (!nzchar(opt$dir))
    opt$dir <- NULL
  if (opt$aggregate) {
    aggr.args <- list(boot = opt$bootstrap, verbose = !opt$quiet, 
      cores = opt$processes)
    if (opt$fast)
      aggr.args$program <- "opm-fast"
  } else
    aggr.args <- NULL
  md.args <- if (is.null(opt$mdfile))
    NULL
  else
    list(md = opt$mdfile, sep = opt$sep, missing.error = TRUE,
      replace = opt$exchange)
  batch_opm_to_yaml(names = input, proc = proc,
    aggr.args = aggr.args, md.args = md.args, outdir = opt$dir,
    verbose = !opt$quiet, overwrite = opt$overwrite, include = opt$include,
    exclude = opt$exclude, gen.iii = opt$Gen3) 
}


################################################################################
#
# Option processing
#


option.parser <- OptionParser(option_list = list(

  make_option(c("-a", "--aggregate"), action = "store_true", default = FALSE,
    help = "Aggregate by estimating curve parameters [default: %default]"),

  make_option(c("-b", "--bootstrap"), type = "integer", default = 100L,
    help = "Number of bootstrap replicates when aggreating [default: %default]",
    metavar = "NUMBER"),

  make_option(c("-c", "--coarse"), action = "store_true", default = FALSE,
    help = "Use coarse-grained parallelization, if any [default: %default]"),

  make_option(c("-d", "--dir"), type = "character", default = ".",
    help = "Output directory (empty => input directory) [default: %default]"),

  make_option(c("-e", "--exclude"), type = "character", default = "", 
    help = "File exclusion globbing pattern [default: <none>]",
    metavar = "PATTERN"),

  make_option(c("-f", "--fast"), action = "store_true", default = FALSE,
    help = "When aggregating, use fast method [default: %default]"),
  
  # A bug in Rscript causes '-g' to generate strange warning messages.
  # See https://stat.ethz.ch/pipermail/r-devel/2008-January/047944.html
  make_option(c("-G", "--Gen3"), action = "store_true", default = FALSE,
    help = "Change plate type to generation III [default: %default]"),
  
  make_option(c("-i", "--include"), type = "character", default = NULL, 
    help = "File inclusion globbing pattern [default: <see package>]", 
    metavar = "PATTERN"),

  make_option(c("-l", "--level"), action = "store_true", default = FALSE, 
    help = "When plotting, draw levelplot [default: %default]"),            
  
  make_option(c("-m", "--mdfile"), type = "character",
    default = NULL, metavar = "NAME",
    help = "Metadata infile (also used as outfile if given) [default: <none>]"),

  make_option(c("-o", "--overwrite"), type = "character", default = "older",
    help = "Overwrite pre-existing output files [default: %default]", 
    metavar = "MODE"),

  make_option(c("-p", "--processes"), type = "integer", default = 1L,
    help = paste("Number of processes to spawn (>1 needs 'multicore')", 
      "[default: %default]"), 
    metavar = "NUMBER"),
  
  make_option(c("-q", "--quiet"), action = "store_true", default = FALSE,
    help = "Do not run verbosely [default: %default]"),

  make_option(c("-r", "--result"), type = "character", default = "yaml",
    metavar = "MODE", help = sprintf(
      "Main result mode; possible values: %s [default: %%default]", 
      paste(RESULT.VALUES, collapse = ", "))),

  make_option(c("-s", "--sep"), type = "character", default = "\t",
    help = "Field separator for metadata files [default: <tab>]", 
    metavar = "CHAR"),
  
  make_option(c("-x", "--exchange"), action = "store_true", default = FALSE,
    help = paste("Exchange old by new metadata instead of appending",
      "[default: %default]"))
  
))


opt <- parse_args(option.parser, positional_arguments = TRUE)
input <- opt$args
opt <- opt$options
if (is.null(opt$include))
  opt$include <- list()


################################################################################
#
# Running the proper script
#


if (length(input) == 0L) {
  print_help(option.parser)
  message(listing(RESULT.DESCS, header = "The output modes are:", footer = "",
    begin = 5L, indent = 10L))
  quit(status = 1L)
}


switch(match.arg(opt$result, RESULT.VALUES),
  clean = run_clean_mode(input, opt),
  plot = run_plot_mode(input, opt),
  split = run_split_mode(input, opt),
  template = run_template_mode(input, opt),
  yaml = run_yaml_mode(input, opt),
  stop("unknown 'result' mode: ", opt$result)      
)

