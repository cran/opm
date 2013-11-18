\name{radial_plot}
\alias{radial_plot}
\title{Radial plot}
\arguments{
  \item{object}{\code{\link{OPMS}} object (with aggregated
  values) to be plotted. Alternatively a data frame or a
  numeric matrix, but the methods for these objects are
  rarely needed.}

  \item{as.labels}{For the \code{\link{OPMS}} method, a
  metadata selection defining the data to be joined and
  used as row names, ultimately yielding the legend. If
  \code{NULL} or empty, ignored. See \code{\link{extract}}
  for details. For the data-frame method, a selection of
  columns used in the same way.}

  \item{subset}{For the \code{\link{OPMS}} method, a
  character scalar passed to the \code{\link{OPMS}} method
  of \code{\link{extract}}. The data-frame method uses this
  to select the columns; currently only the default makes
  sense. For the matrix method, a selection of columns to
  be plotted.}

  \item{sep}{Character scalar determining how to join row
  names. See \code{\link{extract}} for details.}

  \item{extract.args}{Optional list of arguments passed to
  the \code{\link{OPMS}} method of \code{\link{extract}}.
  Currently ignored by the other methods.}

  \item{rp.type}{Character vector. These and the following
  arguments are passed to \code{plotrix::radial.plot}. See
  there for details.}

  \item{radlab}{Logical scalar.}

  \item{show.centroid}{Logical scalar.}

  \item{show.grid.labels}{Logical scalar.}

  \item{lwd}{Numeric scalar.}

  \item{mar}{Numeric vector of length 4.}

  \item{line.col}{Character or numeric vector.}

  \item{point.symbols}{Passed to \code{radial.plot} from
  the \pkg{plotrix} package. See there for details.
  Explicitly provided here to silence some
  \code{radial.plot} warnings occurring as of \R 3.0.0.}

  \item{point.col}{Also passed to that function.}

  \item{poly.col}{Also passed to that function.}

  \item{main}{Also passed to that function.}

  \item{...}{Optional other arguments passed to that
  function.}

  \item{draw.legend}{Logical scalar. Whether to draw a
  legend. Ignored unless \code{object} has row names
  (because these are used to generate the description).}

  \item{x}{Legend position, passed to \code{legend} from
  the \pkg{graphics} package. Ignored unless
  \code{draw.legend} is \code{TRUE}.}

  \item{y}{Optional Second legend coordinate. Also passed
  to that function.}

  \item{xpd}{Logical scalar. Also passed to that function.}

  \item{pch}{Integer scalar. Also passed to that function.}

  \item{legend.args}{List of optional other arguments
  passed to that function.}
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
}
\details{
  The default positioning of the legend is not necessarily
  very useful, but suitable combinations of \code{margin},
  \code{x} and \code{y} can be found for given data sizes.
  Plotting entire plates usually makes not much sense (see
  the examples).

  The data frame and \code{\link{OPMS}} methods extract a
  numeric matrix from a given data frame or
  \code{\link{OPMS}} object and pass the result to the
  matrix method.
}
\examples{
data("vaas_4")

## 'OPMS' method
# note that we have to use only a subset of the wells for plotting
(y <- radial_plot(vaas_4[, , 1:5], as.labels = list("Species", "Strain"),
  main = "Test"))

## Data-frame method (rarely needed)
x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
(yy <- radial_plot(x[, 1:8], as.labels = c("Species", "Strain"),
  main = "Test"))
stopifnot(identical(y, yy)) # should also yield the same picture than above
stopifnot(is.character(yy), names(yy) == paste(x$Species, x$Strain))

## Matrix method (rarely needed)
x <- extract(vaas_4, as.labels = list("Species", "Strain"))
(yy <- radial_plot(x[, 1:5], main = "Test"))
stopifnot(identical(y, yy)) # should also yield the same picture than above
stopifnot(is.character(yy), names(yy) == rownames(x))
}
\seealso{
  plotrix::radial.plot graphics::legend

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{summary}}, \code{\link{xy_plot}}
}
\keyword{hplot}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{radial_plot-methods}
\alias{radial_plot,OPMS-method}
\alias{radial_plot,data.frame-method}
\alias{radial_plot,matrix-method}
\usage{
  \S4method{radial_plot}{OPMS}(object, as.labels,
    subset = opm_opt("curve.param"), sep = " ", extract.args = list(), ...) 
  \S4method{radial_plot}{data.frame}(object, as.labels,
    subset = "numeric", sep = " ", extract.args = list(), ...) 
  \S4method{radial_plot}{matrix}(object, as.labels = NULL,
    subset = TRUE, sep = " ", extract.args = list(), rp.type = "p",
    radlab = FALSE, show.centroid = TRUE, show.grid.labels = 1, lwd = 3,
    mar = c(2, 2, 2, 2), line.col = opm_opt("colors"), draw.legend = TRUE,
    x = "bottom", y = NULL, xpd = TRUE, pch = 15, legend.args = list(),
    point.symbols = NA, point.col = NA, poly.col = NA,
    main = paste0(as.labels, sep = sep), ...) 
}
%% END INSERTION BY repair_S4_docu.rb
