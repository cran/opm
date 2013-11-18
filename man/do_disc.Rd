\name{do_disc}
\alias{do_disc}
\title{Discretise curve parameters}
\arguments{
  \item{object}{\code{\link{OPMA}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{cutoff}{Determines the discretisation approach. If
  non-empty, passed as \code{range} argument to
  \code{discrete} (with \code{gap} set to \code{TRUE}),
  thus triggering discretisation using either k-means
  partitioning or one or two predefined thresholds. If
  empty (e.g., \code{NULL}), a discretisation cutoff is
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
  single element.

  The \code{groups} argument has no effect on
  \code{\link{OPMA}} objects.}

  \item{plain}{Logical scalar indicating whether or not an
  \code{\link{OPMD}} or \code{\link{OPMS}} object should be
  created.}

  \item{subset}{Character scalar passed to
  \code{\link{extract}}. It is recommended to use the
  maximum height (currently called \sQuote{A}).}

  \item{unify}{Logical or numeric scalar indicating whether
  results should be unified per group. This works by
  choosing the most frequent value (mode) if its frequency
  is above a given threshold and \code{NA} otherwise. (The
  same approach is used by \code{\link{listing}} and
  \code{\link{phylo_data}}.)

  If \code{unify} is a logical scalar, \code{NA} triggers
  unification using 1 as threshold, i.e. all ambiguities
  are codes as \code{NA}. Using \code{TRUE} turns on
  unification with the default threshold given by
  \code{opm_opt("min.mode")}, whereas \code{FALSE} turns
  unification off.

  If \code{unify} is a numeric scalar, values below or
  equal to zero turn unification off. Values above zero are
  directly used as unification threshold, thus values above
  1 or numeric \code{NA} make no sense (cause an error).

  See \sQuote{Details} below on the potential consequences
  of unification. In the \code{\link{disc_settings}}
  entries, an according \sQuote{unified} entry will report
  the threshold used, with -1 indicating no unification.

  The \code{unify} argument has no effect on
  \code{\link{OPMA}} objects (because they represent a
  single group with a single member).}

  \item{...}{Optional arguments passed between the methods
  or to \code{\link{extract}}. The latter is only relevant
  for certain settings of \code{groups}, see above.}
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
  Discretise the aggregated kinetic data, i.e. infer
  discrete values from the curve parameters stored in an
  \code{\link{OPMA}} or \code{\link{OPMS}} object. Here,
  only discretisation into positive, negative and ambiguous
  reactions is supported, and by default based on the
  \sQuote{maximum height} curve parameter (which is
  biologically reasonable though).
}
\details{
  If \code{unify} is set to \code{FALSE}, the
  discretisation results are always consistent (in the
  sense described for the \code{\link{OPMD}} class) with
  the discretised parameter. If \code{unify} is set to
  \code{TRUE} this cannot be guaranteed any more. To
  enforce consistency, use \code{opm_opt(strict.OPMD =
  TRUE)}.

  The discretised values can be queried for using
  \code{\link{has_disc}} and received using
  \code{\link{discretized}}.
}
\examples{
## OPMA method

# arbitrary threshold, no ambiguity
summary(x <- do_disc(vaas_1, cutoff = 100))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
(y <- disc_settings(x))  # stored discretisation settings
stopifnot(identical(y$method, "direct"))
stopifnot(is.list(y), is.list(y$options)) # named lists

# arbitrary thresholds, allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = c(75, 125)))
# the intermediate reactions are coded as NA
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(is.na(discretized(x))))
(y <- disc_settings(x)) # stored discretisation settings
stopifnot(identical(y$method, "direct"))
stopifnot(is.list(y), is.list(y$options)) # named lists

# using k-means, two categories, no intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = FALSE))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), !is.na(discretized(x)))
(y <- disc_settings(x)) # stored discretisation settings
stopifnot(identical(y$method, "kmeans"))
stopifnot(is.list(y), is.list(y$options)) # named lists

# using k-means, now allowing intermediate ('weak') reactions
summary(x <- do_disc(vaas_1, cutoff = TRUE))
stopifnot(has_disc(x), dim(x) == dim(vaas_1), any(discretized(x)))
(y <- disc_settings(x)) # stored discretisation settings
stopifnot(identical(y$method, "kmeans"))
stopifnot(is.list(y), is.list(y$options)) # named lists


## OPMS method

# arbitrary threshold, no ambiguity, no groups
x <- do_disc(vaas_4, cutoff = 100)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "direct"))
stopifnot(is.list(y), is.list(y$options)) # named lists

# arbitrary threshold, no ambiguity, no groups, with unification
x <- do_disc(vaas_4, cutoff = 100, unify = TRUE)
stopifnot(has_disc(x), dim(x) == dim(vaas_4))
stopifnot(any(is.na(discretized(x)))) # NAs caused by unification
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "direct"))
stopifnot(is.list(y), is.list(y$options)) # named lists
# all plates made uniform (makes not much sense)

# arbitrary threshold, no ambiguity, with groups, 1 plate per group
x <- do_disc(vaas_4, cutoff = 100, groups = TRUE)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "direct"))
stopifnot(is.list(y), is.list(y$options)) # named lists
# here, the plate numbers yield the group names

# arbitrary threshold, no ambiguity, with specified groups
x <- do_disc(vaas_4, cutoff = 100, groups = "Species")
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "direct"))
stopifnot(is.list(y), is.list(y$options)) # named lists
# now, groups are from the metadata (but played no role)

# using k-means, no ambiguity, with specified groups
x <- do_disc(vaas_4, cutoff = FALSE, groups = "Species")
stopifnot(has_disc(x), dim(x) == dim(vaas_4), !is.na(discretized(x)))
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "kmeans"))
stopifnot(is.list(y), is.list(y$options)) # named lists
# grouping by species, discretised separately

# same, with unification
x <- do_disc(vaas_4, cutoff = FALSE, groups = "Species", unify = TRUE)
stopifnot(has_disc(x), dim(x) == dim(vaas_4))
stopifnot(any(is.na(discretized(x)))) # NAs caused by unification
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "kmeans"))
stopifnot(is.list(y), is.list(y$options)) # named lists
# grouping by species, discretised separately, then made uniform

# using best_cutoff(), groups defined by species affiliation (makes not
# much sense and by default yields warnings with these data)
x <- do_disc(vaas_4, cutoff = NULL, groups = "Species")
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "best-cutoff"))
stopifnot(is.list(y), is.list(y$options)) # named lists
# groups as above, 2 strains per species, but some additional entries

# using best_cutoff(), single group for all plates (makes even less sense
# and by default also yields warnings with these data)
x <- do_disc(vaas_4, cutoff = NULL, groups = FALSE)
stopifnot(has_disc(x), dim(x) == dim(vaas_4), any(is.na(discretized(x))))
(y <- disc_settings(x)[[1]]) # stored discretisation settings
stopifnot(identical(y$method, "best-cutoff"))
stopifnot(is.list(y), is.list(y$options)) # named lists
# no subgroups, all 4 data sets in one group, and some additional entries
}
\seealso{
  Other discretization-functions:
  \code{\link{best_cutoff}}, \code{\link{discrete}}
}
\keyword{category}
\keyword{character}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{do_disc-methods}
\alias{do_disc,MOPMX-method}
\alias{do_disc,OPMA-method}
\alias{do_disc,OPMS-method}
\usage{
  \S4method{do_disc}{MOPMX}(object, ...) 
  \S4method{do_disc}{OPMA}(object, cutoff, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), unify = FALSE) 
  \S4method{do_disc}{OPMS}(object, cutoff = TRUE, groups = FALSE,
    plain = FALSE, subset = opm_opt("disc.param"), unify = !length(cutoff),
    ...) 
}
%% END INSERTION BY repair_S4_docu.rb
