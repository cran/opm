\name{do_disc}
\alias{do_disc}
\title{Discretize curve parameters}
\arguments{
  \item{object}{\code{\link{OPMA}} or \code{\link{OPMS}}
  object.}

  \item{cutoff}{Determines the discretization approach. If
  non-empty, passed as \code{range} argument to
  \code{discrete} (with \code{gap} set to \code{TRUE}),
  thus triggering discretization using either k-means
  partitioning or one or two predefined thresholds. If
  empty (e.g., \code{NULL}), a discretization cutoff is
  determined using \code{\link{best_cutoff}}, which is only
  possible for \code{\link{OPMS}} objects.}

  \item{groups}{List, \code{NULL} or character vector
  passed as \sQuote{as.labels} argument to
  \code{\link{extract}}, or logical scalar.  In that case,
  if \code{TRUE}, groups are automatically created with one
  plate per group. If \code{FALSE}, grouping is not used,
  i.e. there is only a single group containing all plates.

  Note that if \code{cutoff} is empty and \code{groups} is
  \code{TRUE}, an error is raised since
  \code{\link{best_cutoff}} needs groups with more than a
  single element.}

  \item{plain}{Logical scalar indicating whether or not an
  \code{\link{OPMD}} or \code{\link{OPMS}} object should be
  created.}

  \item{subset}{Character scalar passed to
  \code{\link{extract}}. It is recommended to use the
  maximum height (currently called \sQuote{A}).}

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
# helper function
mustbe <- function(a, b) stopifnot(identical(a, b))


## OPMA method
data(vaas_1)

# arbitrary threshold, no ambiguity
summary(x <- do_disc(vaas_1, cutoff = 100))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
(y <- disc_settings(x))  # stored discretization settings
mustbe(y$method, "direct")
mustbe(y$options, list(cutoffs = 100, datasets = 1L, parameter = "A"))

# arbitrary thresholds, allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = c(75, 125)))
# the intermediate reactions are coded as NA
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(is.na(discretized(x))))
(y <- disc_settings(x)) # stored discretization settings
mustbe(y$method, "direct")
mustbe(y$options, list(cutoffs = c(75, 125), datasets = 1L, parameter = "A"))

# using k-means, two categories, no intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = FALSE))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
(y <- disc_settings(x)) # stored discretization settings
mustbe(y$method, "kmeans")
mustbe(length(y$options$cutoffs), 1L)

# using k-means, now allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = TRUE))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(discretized(x)))
(y <- disc_settings(x)) # stored discretization settings
mustbe(y$method, "kmeans")
mustbe(length(y$options$cutoffs), 2L) # now 2 cutoff values


## OPMS method
data(vaas_4)

# arbitrary threshold, no ambiguity, no groups
x <- do_disc(vaas_4, cutoff = 100)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]]) # stored discretization settings
mustbe(y$method, "direct")
mustbe(y$options, list(cutoffs = 100, datasets = 4L, parameter = "A"))

# arbitrary threshold, no ambiguity, with groups, 1 plate per group
x <- do_disc(vaas_4, cutoff = 100, groups = TRUE)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]]) # stored discretization settings
mustbe(y$method, "direct")
mustbe(y$options, # here, the plate numbers yield the group names
  list(cutoffs = 100, datasets = 1L, group = "1", parameter = "A"))

# arbitrary threshold, no ambiguity, with specified groups
x <- do_disc(vaas_4, cutoff = 100, groups = "Species")
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]]) # stored discretization settings
mustbe(y$method, "direct")
mustbe(y$options, # now, groups are from the metadata (but played no role)
  list(cutoffs = 100, datasets = 2L, group = "Escherichia coli",
    parameter = "A"))

# using k-means, no ambiguity, with specified groups
x <- do_disc(vaas_4, cutoff = TRUE, groups = "Species")
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
(y <- disc_settings(x)[[1]]) # stored discretization settings
mustbe(y$method, "kmeans")
mustbe(y$options$group, "Escherichia coli") # grouping by species
mustbe(y$options$datasets, 2L) # discretized separately
mustbe(length(y$options$cutoffs), 2L)

# using best_cutoff(), groups defined by species affiliation (makes not
# much sense)
x <- do_disc(vaas_4, cutoff = NULL, groups = "Species")
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
(y <- disc_settings(x)[[1]]) # stored discretization settings
mustbe(y$method, "best-cutoff")
mustbe(y$options$group, "Escherichia coli") # groups as above
mustbe(y$options$datasets, 2L) # 2 strains per species
# ...but some additional entries:
stopifnot(c("cutoffs", "score") \%in\% names(y$options))

# using best_cutoff(), single group for all plates (makes even less sense)
x <- do_disc(vaas_4, cutoff = NULL, groups = FALSE)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
(y <- disc_settings(x)[[1]]) # stored discretization settings
mustbe(y$method, "best-cutoff")
mustbe(y$options$group, NULL) # no subgroups
mustbe(y$options$datasets, 4L) # all 4 datasets in one group
# ...and the some additional entries:
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
  \S4method{do_disc}{OPMA}(object, cutoff, plain = FALSE,
    subset = opm_opt("disc.param")) 

  \S4method{do_disc}{OPMS}(object, cutoff = TRUE, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), ...) 

}
