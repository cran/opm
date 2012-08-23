\name{paper_size}
\alias{paper_size}
\title{Paper size}
\arguments{
  \item{object}{Numeric vector, character vector, or
  missing. If missing, the default \sQuote{papersize} entry
  is queried using \code{getOption}, and the result passed
  to the other methods. If a numeric vector, specifying the
  paper size in the \sQuote{DIN} series. If a character
  vecotr,}

  \item{inches}{Logical scalar. If \code{TRUE}, output unit
  is inches, otherwise millimeters.}

  \item{series}{Character scalar indicating the
  \sQuote{DIN} series to assume.}

  \item{landscape}{Logical vector. Where \code{FALSE},
  \sQuote{portrait} paper orientation is assumed. For the
  character method, this has only an effect for paper size
  specifiers such as \sQuote{letter} that do not belong to
  the \sQuote{DIN} series. For the \sQuote{DIN} series,
  append \sQuote{R} to the specifier to obtain
  \sQuote{landscape} orientation.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Numeric matrix with columns \sQuote{width} and
  \sQuote{height} and \code{object} as row names (if ot was
  a character vector).
}
\description{
  Determine the size for a given standard paper format.
}
\examples{
query <- c("A4", "Letter", "unknown")
(x <- paper_size(query))
stopifnot(is.matrix(x), is.numeric(x), rownames(x) == query)
stopifnot(colnames(x) == c("height", "width"), is.na(x["unknown", ]))
(y <- paper_size(4))
stopifnot(identical(y, x[1L, , drop = FALSE]))
}
\references{
  \url{http://en.wikipedia.org/wiki/Paper_size}
}
\seealso{
  base::getOption

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{max_rgb_contrast}}, \code{\link{mypdf}},
  \code{\link{radial_plot}}, \code{\link{select_colors}},
  \code{\link{xy_plot}}
}
\keyword{dplot}

\docType{methods}
\alias{paper_size-methods}
\alias{paper_size,missing-method}
\alias{paper_size,numeric-method}
\alias{paper_size,character-method}
\usage{
  \S4method{paper_size}{missing}(...) 

  \S4method{paper_size}{numeric}(object, series = c("A", "B", "C"), 
    landscape = FALSE, ...) 

  \S4method{paper_size}{character}(object, inches = FALSE, 
    landscape = FALSE) 

}
