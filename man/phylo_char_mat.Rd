\name{phylo_char_mat}
\alias{phylo_char_mat}
\title{Phylogenetic character matrix}
\arguments{
  \item{object}{Matrix. See \code{\link{phylo_data}}.}

  \item{format}{Output format (character scalar). See
  \code{\link{phylo_data}}.}

  \item{enclose}{Logical scalar. See
  \code{\link{phylo_data}}.}
}
\value{
  Character vector.
}
\description{
  Create proper character matrix (without header and
  footer) in a file format suitable for exporting
  phylogenetic data.
}
\keyword{internal}

\docType{methods}
\alias{phylo_char_mat-methods}
\alias{phylo_char_mat,matrix-method}
\usage{
  \S4method{phylo_char_mat}{matrix}(object, format, enclose = TRUE) 
}
