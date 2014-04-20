#' # How can we identify substrates that yield a positive reaction?
#'
#' A Phenotype Microarray plate consists of 96 wells with up to 96 different
#' substrates including positive and negative control.
#' The physiological reactions towards these 96 substrates may be negative,
#' weak, or positive.
#'
#' This example demonstrates how to identify substrates that yield a positive
#' reaction in the bacterial strain *Escherichia coli* `DSM` 18039.
#'
#' Author: *Johannes Sikorski*
#'
#'
#' ### Load R packages and data

library(opm)
library(opmdata)
data(vaas_et_al)

#' The data set `vaas_et_al` contains data from four bacterial strains.
#' For more details, see the respective
#' [publication](http://dx.doi.org/10.1371%2Fjournal.pone.0034846).

#' ### Extract the plates data of strain *Escherichia coli* `DSM` 18039

DSM18039 <- vaas_et_al[c(Species = "Escherichia coli", Strain = "DSM18039",
  Experiment = "First replicate")]

#' `DSM18039` consists of 10 plates:

dim(DSM18039)

#' ### Discretise the maximum height (`A`) values into positive, weak, and
#' ### negative reactions
#'
#' * The R package `opm` uses k-means partitioning to classify values into
#'   positive, weak, and negative reactions.
#' * Estimating weak reactions is optional at this step.
#' * There is also the concept of ambiguous reactions.
#' * There are other methods for discretisation.

DSM18039 <- do_disc(DSM18039)

#' ### Which wells contain substrates that yield positive reactions in all
#' ### plates?

wells(subset(DSM18039, positive = "all"))

#' ### Textual listing of substrates that yield positive reactions
#' * Only substrates that yield positive reactions are listed. The output of
#' weak or negative reactions is suppressed.

listing(subset(DSM18039, positive = "all"), ~ Strain)

#' # Synopsis
#' * The R package `opm` contains functionality to discretise any of the four
#'   aggregated parameters either using k-means partitioning or other methods.
#' * The results can be retrieved in various forms, such as names of wells
#'   or names of substrates
#' * Additionally the results can be exported for phylogenetic analysis
#' * The results can be directly used for graphical display of either
#'   raw measurements or aggregated data, for example:
#'
#' ### X-Y plot of substrates that yield positive reactions
#' The colour coding is according to the 10 different replicates.
#+ Figure1, fig.width = 12, fig.height = 12

xy_plot(subset(DSM18039, positive = "all"), include = list("Plate number"),
  neg.ctrl = 50, legend.fmt = list(space = "right"))

