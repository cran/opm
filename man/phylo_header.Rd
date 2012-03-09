\name{phylo_header}
\alias{phylo_header}
\title{Header for phylogenetic data}
\arguments{
  \item{object}{Matrix.}

  \item{format}{Output format (character scalar). See
  \code{\link{phylo_data}}.}

  \item{enclose}{Logical scalar. See
  \code{\link{phylo_data}}.}

  \item{indent}{Integer. See \code{\link{phylo_data}}.}
}
\value{
  Character vector.
}
\description{
  Create header of file formats for exporting phylogenetic
  data.
}
\keyword{internal}

\docType{methods}
\alias{phylo_header-methods}
\alias{phylo_header,matrix-method}
\usage{
  \S4method{phylo_header}{matrix}(object, format, enclose = TRUE,
    indent = 3L) 

}
