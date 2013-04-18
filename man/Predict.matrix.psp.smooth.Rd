\name{Predict.matrix.psp.smooth}
\alias{Predict.matrix.psp.smooth}
\title{Prediction method for P-splines}
\usage{
  \method{Predict.matrix}{psp.smooth} (object, data)
}
\arguments{
  \item{object}{An object of class \code{psp.smooth}.}

  \item{data}{A \code{data.frame} for which the smooth term
  is to be evaluated.}
}
\value{
  Matrix.
}
\description{
  Prediction method for P-splines fitted via \code{gam(y ~
  s(x, bs = "psp", ...))}. Used internally in
  \code{\link{do_aggr}} with \code{method = "spline.fit"}
  and \code{set_spline_options(type = "p.spline")}.
}
\author{
  Benjamin Hofner
}
\keyword{internal}

