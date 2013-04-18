
################################################################################
#
# PREAMBLE
#

# This is example R code for using opm in a standardized setting to create
# files describing Phenotype MicroArray results run in PM mode, with special
# emphasis on the kind of information needed in microbial taxonomy.
#
# It reads all CSV files within the working directory and produces YAML, HTML
# and PDF output. It naively assumes that the relevant meta-information defining
# the organism groups is found within the CSV files. Apart from that, it would
# need only minimal adaptations for real-world use after copying its two pages
# into an R text editor.
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This file is distributed under the terms of the GPL.

library(opm) # version >= 0.8-0 is needed

# Names of replicate IDs. If not within the input CSV data, automatically
# generated.
#
replicate <- "Replicate"

# Name of the organism entries used. If not directly found within the CSV data,
# an error would be raised.
#
organism <- "Strain Number"


################################################################################
#
# INPUT SECTION
#

# Data input. We assume that only the CSV files within the working directory
# should be input and that the data read should be grouped by plate type.
#
x <- read_opm(getwd(), convert = "grp", include = list("csv"))

# Select some CSV data, convert them and enter them as metadata. This needs to
# be adapted to the way CSV data have been recorded.
#
for (i in 1:length(x)) {

  # Creates data frame without converting strings to factors.
  #
  md <- to_metadata(csv_data(x[[i]]))
  
  # Create replicate IDs if they are not included.
  #
  if (!replicate %in% names(md)) {
    md[, replicate] <- 1:nrow(md)
    message("NOTE: inserting replicate IDs")
  }

  # Adding metadata is easiest via a data frame whose order of rows is the the
  # same than the order of OPM objects within the OPMS object. We get such a
  # data frame from the CSV data, of course.
  #
  metadata(x[[i]]) <- md[, c(organism, replicate)]
  
}

################################################################################
#
# COMPUTING SECTION: AGGREGATION AND DISCRETIZATION
#

for (i in 1:length(x)) {

  # That's the time-consuming step here. Specify at most as many cores as you
  # really have on your machine, and keep in mind that parallelization does not
  # work under Windows (for reasons caused by R itself).
  #
  x[[i]] <- do_aggr(x[[i]], boot = 0, cores = 8, method = "splines",
    options = set_spline_options("smooth.spline"))
  
  # The discretization is using exact k-means partitioning, without estimation
  # of an intermediary state. This is OK if > 1 replicates are there and one can
  # calculate ambiguity in another manner (see below).
  #
  x[[i]] <- do_disc(x[[i]], cutoff = FALSE)

}
quit()

################################################################################
#
# OUTPUT SECTION
#

# Copy the CSS file that comes with opm to the current working directory and set
# it as default for HTML tables.
#
opm_opt(css.file = "opm_styles.css")
file.copy(grep("[.]css$", opm_files("auxiliary"), value = TRUE),
  opm_opt("css.file"), overwrite = TRUE)

# For each plate type, create each of the following files:
#
#   * YAML file
#   * HTML file with text
#   * HTML file with table
#   * PDF file with XY plot
#
for (name in names(x)) {

  # Write YAML file. As it contains aggregated and discretized data and
  # settings, too, full reproducibility is guaranteed.
  #
  write(to_yaml(x[[name]]), sprintf("Data_%s.yml", name))

  # Write textual description of discretized results to a file, grouped per
  # strain.
  #
  write(phylo_data(listing(x[[name]], as.groups = organism, html = TRUE)),
    sprintf("Description_%s.html", name))

  # Write HTML table describing the discretized results.
  #
  write(phylo_data(x[[name]], format = "html", as.labels = organism),
    sprintf("Table_%s.html", name))

  # Draw XY plot into PDF file.
  #
  pkgutils::mypdf(sprintf("Plot_%s.pdf", name))
  print(xy_plot(x[[name]], include = list(organism, replicate)))
  dev.off()

}

################################################################################

