#' # Analysing phenotype microarray data: converting CSV files

#' This is example R code for using **opm** in a standardised setting to create
#' files describing phenotype microarray results run in PM mode, with special
#' emphasis on the kind of information needed in microbial taxonomy.
#'
#' It reads all CSV files within the working directory and produces YAML, HTML
#' and PDF output. This code naively assumes that the relevant meta-information
#' defining the organism groups is found within the CSV files. Apart from that,
#' it would need only minimal adaptations for real-world use after modifying
#' a copy of it within an R text editor.
#'
#' But note that `collect_template` and `include_metadata` offer a much more
#' flexible mechanism for including metadata.
#'
#' For advanced users, we recommend to use `batch_opm` instead for most of the
#' steps conducted below.
#'
#' Author: *Markus Goeker*


library(opm)


#' ## Default settings

#' Names of replicate IDs. If not within the input CSV data, automatically
#' generated below. Technically not mandatory, but useful for addressing each
#' plate.
#'
replicate <- "Replicate"

#' Name of the organism entries used. If not directly found within the CSV data,
#' a warning would be raised. If you have no strain numbers, use the term that
#' best describes the data you have.
#'
organism <- "Strain Number"


#' ## Data input

#' We assume that only the CSV files within the working directory should be
#' input and that the data read should be grouped by plate type. This code fails
#' if unreadable CSV files are there (the `include` and/or `exclude` argument
#' would be needed).
#'
x <- read_opm(getwd(), convert = "grp", include = list("csv"))

stopifnot(names(x) == plate_type(x)) # the names already indicate the plate type

x # shows a summary of the contained elements


#' ### Select some CSV data, convert them and enter them as metadata


#' This needs to be adapted to the way CSV data have been recorded. Remember
#' that `collect_template` and `include_metadata` offer a much more flexible
#' mechanism for including metadata.
#'
#' Create data frame without converting strings to factors.
#'
md <- to_metadata(csv_data(x))

#' Create replicate IDs if they are not included.
#'
if (!replicate %in% names(md)) {
  message("NOTE: inserting replicate IDs")
  md[, replicate] <- seq_len(nrow(md))
} else if (any(bad <- is.na(md[, replicate]))) {
  message("NOTE: inserting replicate IDs")
  md[bad, replicate] <- seq_len(nrow(md))[bad] # might yield non-unique IDs
  md[, replicate] <- make.unique(md[, replicate]) # now enforce unique IDs
}

#' Insert a dummy organism entry if none is there, with a warning.
#'
if (!organism %in% names(md)) {
  md[, organism] <- rep_len("Unkown organism", nrow(md))
  warning("inserting dummy organism name", immediate. = TRUE)
} else if (any(bad <- is.na(md[, organism]))) {
  md[bad, organism] <- "Unkown organism"
  warning("inserting dummy organism name", immediate. = TRUE)
}

#' Adding metadata is easiest via a data frame whose order of rows is the same
#' than the order of `OPM` objects within the `OPMS` object and the order of
#' `OPMX` objects within a `MOPMX` object. We get such a data frame from the CSV
#' data, of course.
#'
metadata(x) <- md[, c(organism, replicate)]


#' ## Computing section: aggregation and discretisation

#' That's the time-consuming step here. Specify at most as many cores as you
#' really have on your machine, and keep in mind that parallelisation does not
#' work under Windows (for reasons caused by R itself).
#'
if (grepl("windows", Sys.info()[["sysname"]], TRUE, TRUE)) {
  nc <- 1
} else {
  nc <- 8
}

x <- do_aggr(x, boot = 0, cores = nc, method = "splines",
  options = set_spline_options("smooth.spline"))

#' Let's have a look at the upper part of the aggregation settings.
#'
head(aggr_settings(x, join = "json"), 1)


#' This discretisation is using exact k-means partitioning, without estimation
#' of an intermediary state. This is OK if > 1 replicates are there and one
#' can calculate ambiguity in another manner (see below).
#'
x <- do_disc(x, cutoff = FALSE)

#' Let's have a look at the upper part of the discretisation settings.
#'
head(disc_settings(x, join = "json"), 1)


#' ## Output section


#' Set the `CSS` file that comes with **opm** as default for HTML tables.
#'
opm_opt(css.file = opm_files("css"))

#' An alternative would be to copy it to the current working directory as
#' follows (and use the copy):
#' `file.copy(opm_files("css")[[1]], opm_opt("css.file"), overwrite = TRUE)`
#' `opm_opt(css.file = "opm_styles.css")`


#' For each plate type, create each of the following files:
#'
#'   * YAML file
#'   * HTML file with text
#'   * HTML file with table
#'   * PDF file with x-y-plot
#'
for (name in names(x)) {

  # Write YAML file. As it contains aggregated and discretised data and
  # settings, too, full reproducibility is guaranteed.
  #
  write(to_yaml(x[[name]]), sprintf("Data_%s.yml", name))

  # Write textual description of discretised results to a file, grouped per
  # strain. Embed the content of the `CSS` file.
  #
  write(phylo_data(listing(x[[name]], as.groups = organism, html = TRUE),
      html.args = html_args(embed.css = TRUE)),
    sprintf("Description_%s.html", name))

  # Write HTML table describing the discretised results. This cannot be done
  # unless there are several replicates. Embed the content of the `CSS` file.
  #
  if (length(x[[name]]) > 1)
    write(phylo_data(x[[name]], format = "html", as.labels = organism,
        html.args = html_args(embed.css = TRUE)),
      sprintf("Table_%s.html", name))

  # Draw x-y-plot into PDF file.
  #
  pkgutils::mypdf(sprintf("Plot_%s.pdf", name))
  print(xy_plot(x[[name]], include = list(organism, replicate)))
  dev.off()

}


