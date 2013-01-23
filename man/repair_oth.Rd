\name{repair_oth}
\alias{repair_oth}
\title{Repair OTH plates}
\arguments{
  \item{x}{Numeric matrix.}
}
\value{
  Matrix. Either equal to \code{x} or conatining a single
  row only.
}
\description{
  Plates run in generation-III mode, if converted to
  \acronym{CSV} by the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  software, comprise duplicated rows with only two sets of
  distinct values, the first set containing only \code{0}.
  This function checks for such a matrix and, if
  identified, reduces it to its single informative row.
}
\keyword{internal}

\docType{methods}
\alias{repair_oth-methods}
\alias{repair_oth,matrix-method}
\usage{
  \S4method{repair_oth}{matrix}(x) 

}
