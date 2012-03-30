\name{prepare_class_names}
\alias{prepare_class_names}
\title{Prepare class names}
\arguments{
  \item{object}{Character vector.}
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

\docType{methods}
\alias{prepare_class_names-methods}
\alias{prepare_class_names,character-method}
\usage{
  \S4method{prepare_class_names}{character}(object) 

}
