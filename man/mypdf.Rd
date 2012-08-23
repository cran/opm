\name{mypdf}
\alias{mypdf}
\title{Create PDF file}
\usage{
  mypdf(file, paper = "a4r", prop = 0.9, ...)
}
\arguments{
  \item{file}{See \code{grDevices::pdf}.}

  \item{paper}{Like \code{grDevices::pdf}, but its
  \code{\link{paper_size}} is used to determine
  \sQuote{width} and \sQuote{height} of the plotting
  region.}

  \item{prop}{Numeric vector. Its values should be between
  0 and 1. Its first element is multiplied with the width
  of \code{paper} to yield the width of the plotting
  region. Its last element is multiplied with the height of
  \code{paper} to yield the height of the plotting region.}

  \item{...}{Optional arguments passed to
  \code{grDevices::pdf}.}
}
\value{
  \code{NULL}. As a side effect, \code{file} is opened.
}
\description{
  A slightly adapted wrapper for \code{pdf} from the
  \pkg{grDevices} package.
}
\examples{
\dontrun{
  mypdf("example.pdf")
  ## create some plots...
  dev.off()
}
}
\seealso{
  grDevices::pdf

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{max_rgb_contrast}}, \code{\link{paper_size}},
  \code{\link{radial_plot}}, \code{\link{select_colors}},
  \code{\link{xy_plot}}
}
\keyword{IO}

