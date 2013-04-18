\name{set_spline_options}
\alias{set_spline_options}
\title{Spline options}
\usage{
  set_spline_options(type = c("tp.spline", "p.spline", "smooth.spline"),
    knots = NULL, gamma = 1, est.method = c("REML", "GCV"),
    s.par = NULL, save.models = FALSE, filename = NULL,
    ...)
}
\arguments{
  \item{type}{Character scalar. Specifies the spline type
  which should be fitted. This can be either thin plate
  splines (\dQuote{tp.spline}), penalized B-splines (i.e,
  P-splines \dQuote{p.spline}) or smoothing splines
  (\dQuote{smooth.spline}).}

  \item{knots}{Integer scalar. Determines the number of
  knots. Per default, the number of knots is chosen
  adaptively to the number of unique observations.}

  \item{gamma}{Integer scalar. Specifies a constant
  multiplier to inflate the degrees of freedom in the
  \code{"GCV"} \code{method} to increase penalization of
  models that are too close to the data and thus not smooth
  enough.}

  \item{est.method}{Character scalar. The smoothing
  parameter estimation method. Currently, only
  \code{"REML"} and \code{"GCV"} are supported. This
  argument is ignored for \code{type = "smooth.spline"}.
  For details see \code{\link[mgcv]{gam}} (see package
  \pkg{mgcv}).}

  \item{s.par}{list. Further arguments to be passed to the
  smoother \code{\link[mgcv]{s}} (see package \pkg{mgcv}).
  Note that the \pkg{mgcv} options \code{k} and \code{bs}
  are specified using \code{type} and \code{knots} in
  \pkg{opm}.}

  \item{save.models}{Should the models be saved (on the
  disk) for further inspections and plotting?}

  \item{filename}{Filename of the models. Per default a
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

