\name{try_select_colors}
\alias{try_select_colors}
\title{Safely select colors.}
\usage{
  try_select_colors(set)
}
\arguments{
  \item{set}{Character vector passed to
  \code{\link{select_colors}}.}
}
\value{
  Character vector of colour codes.
}
\description{
  Call \code{\link{select_colors}} and if this does not
  work return the input argument as-is.
}
\keyword{internal}

