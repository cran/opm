\name{heat_map}
\alias{heat_map}
\title{Heatmap}
\arguments{
  \item{object}{Dataframe. Must contain at least one column
  with numeric data.}

  \item{as.labels}{Character, numeric or logical vector
  indicating the positions of the columns to be joined and
  used as row labels. If \code{NULL} or empty, the row
  names of \code{object} are used.}

  \item{as.groups}{Character, numeric or logical vector
  indicating the positions of the columns to be joined and
  used as group indicators. If \code{NULL} or empty, groups
  are ignored.}

  \item{sep}{Character scalar determining how to join row
  and group names.}

  \item{...}{Arguments passed to
  \code{\link{heat_map,matrix}}.}
}
\value{
  See \code{\link{heat_map,matrix}}.
}
\description{
  Extracts a numeric matrix from a given dataframe and
  passes the result to \code{\link{heat_map,matrix}}.
}
\examples{
data("vaas_4")
x <- extract(vaas_4, as.labels = list("Species", "Strain"), dataframe = TRUE)
hm <- heat_map(x, as.labels = "Strain", as.groups = "Species")
stopifnot(identical(metadata(vaas_4, "Species"), names(hm$rowColMap)))
# Compare this with the matrix example
}
\seealso{
  heatmap gplots::heatmap.2

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{ci_plot,OPMS}},
  \code{\link{heat_map,matrix}}, \code{\link{level_plot}},
  \code{\link{level_plot,OPMS}},
  \code{\link{max_rgb_contrast}},
  \code{\link{select_colors}}, \code{\link{xy_plot}},
  \code{\link{xy_plot,OPMS}}
}
\keyword{hplot}

\docType{methods}
\alias{heat_map-methods}
\alias{heat_map,data.frame-method}
\usage{
  \S4method{heat_map}{data.frame}(object, as.labels, 
    as.groups = NULL, sep = " ", ...) 
}
