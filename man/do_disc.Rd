\name{do_disc}
\alias{do_disc}
\title{Discretize curve parameters}
\arguments{
  \item{object}{\code{\link{OPMA}} or \code{\link{OPMS}}
  object.}

  \item{cutoff}{If non-empty, passed as \code{range}
  argument to \code{discrete} (with \code{gap} set to
  \code{TRUE}). If \code{NULL}, a cutoff is determined
  using \code{\link{best_cutoff}}, which is only possible
  for \code{\link{OPMS}} objects.}

  \item{groups}{List or character vector passed to
  \code{\link{extract}}, logical scalar or \code{NULL}. If
  \code{TRUE}, groups are automatically created with one
  plate per group. If \code{FALSE}, grouping is not used.
  behaviour differs depending on \code{cutoff}; if that is
  empty, too, an error is raised since
  \code{\link{best_cutoff}} needs groups with more than a
  single element. Otherwise, if \code{combined} is
  \code{FALSE}, groups are automatically created with one
  plate per group.}

  \item{plain}{Logical scalar indicating whether or not an
  \code{\link{OPMD}} or \code{\link{OPMS}} object should be
  created.}

  \item{...}{Optional arguments passed to
  \code{\link{extract}}. Only relevant for certain settings
  of \code{groups}, see above.}
}
\value{
  If \code{plain} is \code{FALSE}, an \code{\link{OPMD}} or
  \code{\link{OPMS}} object. Otherwise a logical vector
  whose length corresponds to the number of wells in
  \code{object} with an additional \sQuote{settings}
  attribute describing the run. The vector and its
  attribute would correspond to the
  \code{\link{discretized}} and \code{\link{disc_settings}}
  entries of a resulting \code{\link{OPMD}} object,
  respectively.
}
\description{
  Discretize the aggregated kinetic data, i.e. infer
  discrete values from the curve parameters stored in an
  \code{\link{OPMA}} or \code{\link{OPMS}} object. Here,
  only discretization into positive, negative and ambiguous
  reactions is supported, and only based on the
  \sQuote{maximum height} curve parameter (which is
  biologically reasonable though).
}
\examples{
## OPMA method
data(vaas_1)

# arbitrary threshold, no ambiguity
summary(x <- do_disc(vaas_1, cutoff = 100))
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
y <- list(program = "direct", options = list(cutoffs = 100, datasets = 1L))
stopifnot(identical(disc_settings(x), y))

# arbitrary thresholds, allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = c(75, 125)))
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(is.na(discretized(x))))
y <- list(program = "direct", options = list(cutoffs = c(75, 125),
  datasets = 1L))
stopifnot(identical(disc_settings(x), y))

# using k-means, no ambiguity
summary(x <- do_disc(vaas_1, cutoff = FALSE))
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
stopifnot(disc_settings(x)$program == "kmeans")
stopifnot(length(disc_settings(x)$options$cutoffs) == 1)

# using k-means, allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = TRUE))
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(discretized(x)))
stopifnot(disc_settings(x)$program == "kmeans")
stopifnot(length(disc_settings(x)$options$cutoffs) == 2)

# OPMS method
data(vaas_4)

# arbitrary threshold, no ambiguity, no groups
summary(x <- do_disc(vaas_4, cutoff = 100))
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
y <- list(program = "direct", options = list(cutoffs = 100, datasets = 4L))
stopifnot(identical(disc_settings(x)[[1]], y))

# arbitrary threshold, no ambiguity, with groups, 1 plate per group
summary(x <- do_disc(vaas_4, cutoff = 100, groups = TRUE))
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
y <- list(program = "direct", options = list(cutoffs = 100, datasets = 1L))
y$options$group <- "1" # the plate numbers yield the group names
stopifnot(identical(disc_settings(x)[[1]], y))

# arbitrary threshold, no ambiguity, with specified groups
x <- do_disc(vaas_4, cutoff = 100, groups = "Species")
summary(x)
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
y <- list(program = "direct", options = list(cutoffs = 100, datasets = 2L))
y$options$group <- "Escherichia coli" # groups are from the metadata
stopifnot(identical(disc_settings(x)[[1]], y))

# using k-means, no ambiguity, with specified groups
x <- do_disc(vaas_4, cutoff = TRUE, groups = "Species")
summary(x)
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
stopifnot(identical(disc_settings(x)[[1]]$program, "kmeans"))
stopifnot(names(disc_settings(x)[[1]]$options) ==
  c("cutoffs", "datasets", "group"))

# using best_cutoff()
x <- do_disc(vaas_4, cutoff = NULL, groups = "Species")
summary(x)
disc_settings(x)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
stopifnot(identical(disc_settings(x)[[1]]$program, "best-cutoff"))
stopifnot(names(disc_settings(x)[[1]]$options) ==
  c("cutoffs", "score", "datasets", "group"))
}
\seealso{
  Other discretization-functions:
  \code{\link{best_cutoff}}, \code{\link{discrete}}
}
\keyword{category}
\keyword{character}

\docType{methods}
\alias{do_disc-methods}
\alias{do_disc,OPMA-method}
\alias{do_disc,OPMS-method}
\usage{
  \S4method{do_disc}{OPMA}(object, cutoff, plain = FALSE) 

  \S4method{do_disc}{OPMS}(object, cutoff = TRUE, groups = FALSE,
    plain = FALSE, ...) 

}
