\name{radial_plot}
\alias{radial_plot}
\title{Radial plot}
\arguments{
  \item{object}{Data frame, numeric matrix or \sQuote{OPMS}
  object (with aggregated values) to be plotted.}

  \item{rp.type}{Character vector. These and the following
  arguments are passed to \code{plotrix::radial.plot}. See
  there for details.}

  \item{radlab}{Logical scalar.}

  \item{show.centroid}{Logical scalar.}

  \item{show.grid.labels}{Logical scalar.}

  \item{lwd}{Numeric scalar.}

  \item{mar}{Numeric vector of length 4.}

  \item{line.col}{Character or numeric vector.}

  \item{...}{Optional arguments passed to
  \code{plotrix::radial.plot}.}

  \item{draw.legend}{Logical scalar. Whether to draw a
  legend. Ignored unless \code{object} has row names
  (because these are used to generate the description).}

  \item{x}{Legend position, passed to \code{legend} from
  the \pkg{graphics} package. Ignored unless
  \code{draw.legend} is \code{TRUE}.}

  \item{y}{Optional 2nd legend coordinate. Also passed to
  that function.}

  \item{xpd}{Logical scalar. Also passed to that function.}

  \item{pch}{Integer scalar. Also passed to that function.}

  \item{legend.args}{List of optional other arguments
  passed to that function.}

  \item{as.labels}{Character, numeric or logical vector
  indicating the positions of the columns to be joined and
  used as row labels. If \code{NULL} or empty, the row
  names of \code{object} are used. See
  \code{\link{extract}} for details.}

  \item{sep}{Character scalar determining how to join row
  names. See \code{\link{extract}} for details.}

  \item{subset}{Character scalar passed to the
  \sQuote{OPMS} method of \code{\link{extract}}.}

  \item{extract.args}{Optional list of arguments passed to
  that method.}
}
\value{
  A vector with the row names of \code{object} as names and
  the corresponding colours as values, equivalent to the
  legend; \code{NULL} if no row names are present.
}
\description{
  A wrapper for \code{radial.plot} from the \pkg{plotrix}
  package with some adaptations likely to be useful for
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data.
  The data frame and \sQuote{OPMS} methods extract a
  numeric matrix from a given data frame or \sQuote{OPMS}
  object and pass the result to the matrix method.
}
\note{
  The default positioning of the legend is not necessarily
  very useful, but suitable combinations of \code{margin},
  \code{x} and \code{y} can be found for given data sizes.
  Plotting entire plates usually makes not much sense (see
  the examples).
}
\examples{
data("vaas_4")

# Matrix method
x <- extract(vaas_4, as.labels = list("Species", "Strain"))
(y <- radial_plot(x[, 1:5]))
stopifnot(is.character(y), names(y) == rownames(x))

# 'OPMS' method
(yy <- radial_plot(vaas_4[, , 1:5], as.labels = list("Species", "Strain")))
stopifnot(identical(y, yy)) # should also yield the same picture than above

# Data-frame method
x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
(y <- radial_plot(x[, 1:8], as.labels = c("Species", "Strain")))
stopifnot(is.character(y), names(y) == paste(x$Species, x$Strain))
}
\seealso{
  plotrix::radial.plot graphics::legend

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{select_colors}}, \code{\link{show}},
  \code{\link{xy_plot}}
}
\keyword{hplot}

\docType{methods}
\alias{radial_plot-methods}
\alias{radial_plot,matrix-method}
\alias{radial_plot,data.frame-method}
\alias{radial_plot,OPMS-method}
\usage{
  \S4method{radial_plot}{matrix}(object, rp.type = "p",
    radlab = FALSE, show.centroid = TRUE, show.grid.labels = 1, lwd = 3,
    mar = c(2, 2, 2, 2), line.col = opm_opt("colors"), draw.legend = TRUE,
    x = "bottom", y = NULL, xpd = TRUE, pch = 15, legend.args = list(), ...) 

  \S4method{radial_plot}{data.frame}(object, as.labels, sep = " ",
    ...) 

  \S4method{radial_plot}{OPMS}(object, as.labels,
    subset = opm_opt("curve.param"), sep = " ", extract.args = list(), ...) 

}
