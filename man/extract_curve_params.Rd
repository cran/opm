\name{extract_curve_params}
\alias{extract_curve_params}
\alias{extract_curve_params.grofit}
\alias{extract_curve_params.opm_model}
\title{Parameter extraction}
\usage{
  extract_curve_params(x, ...)

  \method{extract_curve_params}{grofit} (x, ...)

  \method{extract_curve_params}{opm_model} (x, all = FALSE,
    ...)
}
\arguments{
  \item{x}{Object of class \sQuote{grofit} or
  \sQuote{opm_model}.}

  \item{all}{Logic. Should [TODO]}

  \item{...}{Additional arguments.}
}
\value{
  Matrix.
}
\description{
  Extract and rename estimated curve parameters.
}
\keyword{internal}

