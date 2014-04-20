\name{set_spline_options}
\alias{set_spline_options}
\title{Spline options}
\usage{
  set_spline_options(type = c("tp.spline", "p.spline", "smooth.spline"),
    knots = NULL, gamma = 1,
    est.method = c("REML", "ML", "GCV"), s.par = NULL,
    correlation = NULL, save.models = FALSE,
    filename = NULL, ...)
}
\arguments{
  \item{type}{Character scalar. Specifies the spline type
  which should be fitted. This can be either thin plate
  splines (\kbd{tp.spline}), penalised B-splines (i.e,
  P-splines \kbd{p.spline}) or smoothing splines
  (\kbd{smooth.spline}).}

  \item{knots}{Integer scalar. Determines the number of
  knots. Per default, the number of knots is chosen
  adaptively to the number of unique observations.}

  \item{gamma}{Integer scalar. Specifies a constant
  multiplier to inflate the degrees of freedom in the
  \code{"GCV"} \code{method} to increase penalisation of
  models that are too close to the data and thus not smooth
  enough.}

  \item{est.method}{Character scalar. The smoothing
  parameter estimation method. Currently, only
  \code{"REML"}, code{"ML"} and \code{"GCV"} are supported.
  This argument is ignored for \code{type =
  "smooth.spline"}. For details see \code{\link[mgcv]{gam}}
  and \code{\link[mgcv]{gamm}} (see package \pkg{mgcv}).}

  \item{s.par}{list. Further arguments to be passed to the
  smoother \code{\link[mgcv]{s}} (see package \pkg{mgcv}).
  Note that the \pkg{mgcv} options \code{k} and \code{bs}
  are specified using \code{type} and \code{knots} in
  \pkg{opm}.}

  \item{correlation}{An optional \code{"corStruct"} object
  (see the help topic \code{corClasses} in the \pkg{nlme}
  package) as used to define correlation structures in
  package \pkg{nlme}. For better coverage of confidence
  intervals and slightly improved spline fits it is advised
  to use an AR process of order 1 or 2. However, this
  correction for auto-correlated error terms results in
  increased run time.}

  \item{save.models}{Should the models be saved (on the
  disk) for further inspections and plotting?}

  \item{filename}{File name of the models. Per default a
  name is auto-generated based on date and time. The file
  is always generated in the current working directory.}

  \item{...}{Additional arguments to be passed to
  \code{\link[mgcv]{gam}} or \code{\link{smooth.spline}}.}
}
\value{
  List of options.
}
\description{
  Function to set up spline options which can be passed to
  \code{\link{do_aggr}}. %and \code{\link{fit_spline}}.
}
\author{
  Benjamin Hofner
}
\keyword{misc}

