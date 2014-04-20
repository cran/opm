#' # How can we decide whether curve topologies are significantly different?
#'
#' Assume you have run only four PM plates of the same plate type. Assume you
#' have tested by this the phenotypes of four biological specimens, e.g.
#' bacterial strains. For each strain one plate was utilised. Unfortunately, you
#' do not have replicate plates per bacterial strain.
#'
#' ### The following question then arises:
#' * If four curves look fairly similar, how can we assess some statistical
#' information whether they could be the same or be rather significantly
#' different?
#'
#' In order to assess this, we make use of bootstrapped curve parameters, which
#' allow us to determine a 95 % confidence interval.
#'
#' The data set `wittmann_et_al` contains Generation-III data for numerous
#' strains of the bacterial species *Achromobacter xylosoxidans* (see the
#' respective publication).
#'
#'
#' Author: *Johannes Sikorski*
#'
#'
#' ### Load R packages and data

library(opm)
library(opmdata)
data(wittmann_et_al)

#' ### Subset for an appropriate data set for demonstration purpose

wittmann_small <- subset(wittmann_et_al,
    query = list(strain = c("CCUG 41513", "CCUG 2203"), replicate = "2")) +
  subset(wittmann_et_al,
    query = list(strain = c("LMG 7051", "CCUG 48135"), replicate = "1"))

#' ### Check the dimensions and metadata
#'
dim(wittmann_small)
to_metadata(wittmann_small)

#' ### Plot the raw kinetic values for well `G07 (D-Malic Acid)`
#'
#+ Figure1, fig.width = 10, fig.height = 5

xy_plot(wittmann_small[, , "G07"],
  include = list("strain"),
  col = c("red", "green", "blue", "black"),
  legend.fmt = list(space = "right"), lwd = 2, neg.ctrl = 50)

#' ### Result:
#' * The overall curve topology is quite similar.
#' * The following question arises:
#'
#' ### Do strains differ in any of the estimated curve parameters?
#' To answer this, we make use of the 95% confidence intervals obtained
#' during aggregation of curve parameters using bootstrap procedures.
#'
#' ### Show aggregated data for strain `LMG` 7051, well `G07 (D-Malic Acid)`
#' * the aggregated data contain 95% confidence intervals from bootstrapping
#' * enter `?do_aggr` to learn how to bootstrap during curve parameter
#'   aggregation

aggregated(subset(wittmann_small[, , "G07"], list(strain = "LMG 7051")),
  full = TRUE)

#' ### Plot confidence interval for the maximum height value (A)
#+ Figure2, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
  subset = "A", x = "topright", legend.field = NULL, cex = 0.8)

#' ### Conclusions:
#' * strains `CCUG` 41513 and `CCUG` 2203 can not be distinguished by their
#'   maximum height (A)
#' * similarly, also strains `LMG` 7051 and `CCUG` 48135 can not be
#'   distinguished by A
#' * however, both pairs of strains differ significantly in their A value
#'
#' ### Plot confidence interval for the area under the curve (`AUC`)
#+ Figure3, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
  subset = "AUC", x = "topright", legend.field = NULL, cex = 0.8)

#' ### Conclusions:
#' * all strains show different `AUC` values
#'
#' ### Plot confidence interval for the steepness of the slope (mu)
#+ Figure4, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
  subset = "mu", x = "topright", legend.field = NULL, cex = 0.8)

#' ### Conclusions:
#' * all strains show different mu values
#'
#' ### Plot confidence interval for the lag phase (lambda)
#+ Figure5, fig.width = 5, fig.height = 3.5

ci_plot(wittmann_small[, , "G07"], as.labels = "strain",
  subset = "lambda", x = "topleft", legend.field = NULL, cex = 0.8)

#' ### Conclusions:
#' * strains `LMG` 7051 and `CCUG` 2203 can not be distinguished by their
#' lag phase length
#' * the other pairs of strains can be distinguished
#'
#' # Synopsis
#' Even if no experimental replicates exist, very similar curve topologies
#' can be tested for differences in aggregated curve parameters using 95%
#' confidence interval values derived from bootstrapping.
