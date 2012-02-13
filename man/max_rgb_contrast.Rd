\name{max_rgb_contrast}
\alias{max_rgb_contrast}
\title{Sort colours}
\usage{
  max_rgb_contrast(col)
}
\arguments{
  \item{col}{Vector. Names or hexadecimal codes of the
  colors to be sorted.  Might also be an integer vector,
  see \code{col2rgb} from the \pkg{grDevices} package for
  details. Duplicate RGB coordinates and unknown names will
  cause an error.}
}
\value{
  Character vector (rearranged input names).
}
\description{
  A helper function for methods such as
  \code{\link{xy_plot,OPMS}}. Arrange colours to achieve
  that neighboring colours are most distinct with respect
  to their RGB coordinates. This is done as follows: (1)
  euclidean distances between the RGB coordinates of the
  input colours are calculated; (2) the distances are
  inversed; (3) a principal-coordinate analysis is
  conducted on these inversed distances; (4) the input
  colours are sorted according to the first principal
  coordinate.
}
\note{
  The resulting vector could as well be used in reverse
  order.
}
\examples{
(x <- max_rgb_contrast(c("darkred", "darkblue", "blue", "red")))
y <- c("darkblue", "red", "blue", "darkred")
stopifnot(identical(x, y) || identical(x, rev(y)))
}
\seealso{
  col2rgb

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{ci_plot,OPMS}}, \code{\link{heat_map}},
  \code{\link{heat_map,matrix}}, \code{\link{level_plot}},
  \code{\link{level_plot,OPMS}},
  \code{\link{select_colors}}, \code{\link{xy_plot}},
  \code{\link{xy_plot,OPMS}}
}
\keyword{color}

