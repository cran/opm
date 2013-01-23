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
\note{
  The discretized values can be queried for using
  \code{\link{has_disc}} and received using
  \code{\link{discretized}}.
}
\examples{
mustbe <- function(a, b) stopifnot(identical(a, b))

## OPMA method
data(vaas_1)

# arbitrary threshold, no ambiguity
summary(x <- do_disc(vaas_1, cutoff = 100))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
# the settings used  have been stored in the resulting object
(y <- disc_settings(x))
mustbe(y$method, "direct")
mustbe(y$options, list(cutoffs = 100, datasets = 1L))

# arbitrary thresholds, allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = c(75, 125)))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(is.na(discretized(x))))
(y <- disc_settings(x))
mustbe(y$method, "direct")
mustbe(y$options, list(cutoffs = c(75, 125), datasets = 1L))

# using k-means, no ambiguity
summary(x <- do_disc(vaas_1, cutoff = FALSE))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
(y <- disc_settings(x))
mustbe(y$method, "kmeans")
mustbe(length(y$options$cutoffs), 1L)

# using k-means, allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = TRUE))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(discretized(x)))
(y <- disc_settings(x))
mustbe(y$method, "kmeans")
mustbe(length(y$options$cutoffs), 2L) # now 2 cutoff values

# OPMS method
data(vaas_4)

# arbitrary threshold, no ambiguity, no groups
summary(x <- do_disc(vaas_4, cutoff = 100))
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
# the settings used  have been stored in the resulting object
(y <- disc_settings(x)[[1]])
mustbe(y$method, "direct")
mustbe(y$options, list(cutoffs = 100, datasets = 4L))

# arbitrary threshold, no ambiguity, with groups, 1 plate per group
summary(x <- do_disc(vaas_4, cutoff = 100, groups = TRUE))
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]])
mustbe(y$method, "direct")
# here, the plate numbers yield the group names
mustbe(y$options, list(cutoffs = 100, datasets = 1L, group = "1"))

# arbitrary threshold, no ambiguity, with specified groups
summary(x <- do_disc(vaas_4, cutoff = 100, groups = "Species"))
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]])
mustbe(y$method, "direct")
# now, groups are from the metadata (but played no role)
mustbe(y$options,
  list(cutoffs = 100, datasets = 2L, group = "Escherichia coli"))

# using k-means, no ambiguity, with specified groups
summary(x <- do_disc(vaas_4, cutoff = TRUE, groups = "Species"))
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
(y <- disc_settings(x)[[1]])
mustbe(y$method, "kmeans")
# grouping by species, discretized separately
mustbe(y$options$group, "Escherichia coli")
mustbe(y$options$datasets, 2L)
mustbe(length(y$options$cutoffs), 2L)

# using best_cutoff()
summary(x <- do_disc(vaas_4, cutoff = NULL, groups = "Species"))
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
(y <- disc_settings(x)[[1]])
mustbe(y$method, "best-cutoff")
# groups as above
mustbe(y$options$group, "Escherichia coli")
mustbe(y$options$datasets, 2L)
# ...but some additional entries:
stopifnot(c("cutoffs", "score") \%in\% names(y$options))
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
