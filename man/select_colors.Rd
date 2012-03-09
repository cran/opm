\name{select_colors}
\alias{select_colors}
\title{Select colors}
\usage{
  select_colors(set = c("w3c", "w3c.i", "nora", "nora.i", "roseobacter", "roseobacter.i"))
}
\arguments{
  \item{set}{Character scalar. Name of the color vector to
  use. Color vectors have been optimized for maximum
  contrast between adjacent colors, either manually or
  using \code{\link{max_rgb_contrast}}. Names ending in
  \sQuote{.i} indicate vectors in inverse order (compared
  to the vector with the same name except \sQuote{.i}).}
}
\value{
  Character vector (names of colors).
}
\description{
  Select a set of colors for plotting. See
  \code{\link{xy_plot}} for usage example. This is not
  normally directly called by an \pkg{opm} user but could
  be used for testing before doing some serious plotting.
}
\examples{
(x <- select_colors("nora"))
(y <- select_colors("nora.i"))
stopifnot(is.character(x), length(x) > 0L, identical(x, rev(y)))
}
\references{
  \url{http://www.colorbrewer.org}
}
\seealso{
  grDevices::colors grDevices::rainbow grDevices::grey

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{max_rgb_contrast}}, \code{\link{xy_plot}}
}
\keyword{color}

