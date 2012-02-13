\name{do_aggr}
\alias{do_aggr}
\title{Aggregate kinetics}
\arguments{
  \item{object}{\code{\link{OPM}} object}

  \item{...}{Passed to \code{\link{curve_params}}. See
  there.}
}
\value{
  \code{\link{OPMA}} object.
}
\description{
  Aggregate data using curve parameter estimation and
  include them in a novel object together with previously
  collected information.
}
\examples{
data(vaas_1)

# Run a fast estimate of A and AUC without bootstrapping
copy <- do_aggr(vaas_1, program = "opm-fast", boot = 0,
  options = list(as.pe = "pe"))
stopifnot(has_aggr(vaas_1), has_aggr(copy))
stopifnot(identical(aggr_settings(vaas_1)$program, "grofit"))
stopifnot(identical(aggr_settings(copy)$program, "opm-fast"))

# Compare the results to the ones precomputed with grofit
a.grofit <- aggregated(vaas_1, "A", ci = FALSE)
a.fast <-  aggregated(copy, "A", ci = FALSE)
plot(a.grofit, a.fast)
stopifnot(cor.test(a.fast, a.grofit)$estimate > 0.999)
auc.grofit <- aggregated(vaas_1, "AUC", ci = FALSE)
auc.fast <-  aggregated(copy, "AUC", ci = FALSE)
plot(auc.grofit, auc.fast)
stopifnot(cor.test(auc.fast, auc.grofit)$estimate > 0.999)

\dontrun{

  # Without confidence interval (CI) estimation
  x <- do_aggr(vaas_1, boot = 0, verbose = TRUE)
  aggr_settings(x)
  aggregated(x)

  # Calculate CIs with 100 bootstrap (BS) replicates, using 4 cores
  x <- do_aggr(vaas_1, boot = 100, verbose = TRUE, cores = 4)
  aggr_settings(x)
  aggregated(x)
}
}
\seealso{
  grofit::grofit

  Other aggregation-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{curve_params}},
  \code{\link{fast_estimate}}, \code{\link{param_names}}
}
\keyword{smooth}

\docType{methods}
\alias{do_aggr-methods}
\alias{do_aggr,OPM-method}
\usage{
  \S4method{do_aggr}{OPM}(object, ...) 
}
