\name{prepare_class_names}
\alias{prepare_class_names}
\alias{prepare_class_names.character}
\title{Prepare class names}
\usage{
  prepare_class_names(x)

  \method{prepare_class_names}{character} (x)
}
\arguments{
  \item{x}{Character vector.}
}
\value{
  Character vector.
}
\description{
  Ensure that a vector of class names contains only unique
  values and \sQuote{character}. Reduce it to \sQuote{ANY}
  if \sQuote{ANY} is contained. See
  \code{\link{map_values}} for a use.
}
\keyword{internal}

