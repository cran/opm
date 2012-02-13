\name{repair_names}
\alias{repair_names}
\title{Repair names}
\arguments{
  \item{object}{List.}

  \item{fill}{Logical scalar. If \code{TRUE}, fill
  empty-string names by list positions coerced to character
  mode. If \code{FALSE}, delete all names if at least one
  of them is the empty string.}
}
\value{
  List.
}
\description{
  Recursively repair all names of a list, i.e. ensure that
  \code{names} applied to the list and its sublists, if
  any, returns either \code{NULL} or a character vector not
  containing any empty string. This is useful when
  outputting lists in YAML format because otherwise maps
  with empty keys might be created, resulting in invalidly
  formatted, unparseable data.
}
\keyword{internal}

\docType{methods}
\alias{repair_names-methods}
\alias{repair_names,list-method}
\usage{
  \S4method{repair_names}{list}(object, fill = TRUE) 
}
