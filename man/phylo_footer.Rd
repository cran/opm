\name{phylo_footer}
\alias{phylo_footer}
\title{Footer for phylogenetic data}
\usage{
  phylo_footer(format, indent = 3L, paup.block = FALSE)
}
\arguments{
  \item{format}{Character scalar. See
  \code{\link{phylo_data}} for the possible values.}

  \item{indent}{Integer scalar. See
  \code{\link{phylo_data}}.}

  \item{paup.block}{Logical scalar. Append a PAUP* block
  with selected default values?}
}
\value{
  Character vector or \code{NULL}.
}
\description{
  Create footer of file formats for exporting phylogenetic
  data.
}
\keyword{internal}

