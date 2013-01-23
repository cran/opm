\name{format}
\alias{format}
\title{Format phylogenetic data}
\arguments{
  \item{x}{Object of class \sQuote{CMAT}.}

  \item{how}{Character scalar. See \code{\link{phylo_data}}
  for the possible values (argument \sQuote{format}).}

  \item{enclose}{Logical scalar. See
  \code{\link{phylo_data}}.}

  \item{digits}{Integer scalar. See
  \code{\link{phylo_data}}.}

  \item{indent}{Integer scalar. See
  \code{\link{phylo_data}}.}

  \item{paup.block}{Logical scalar. See
  \code{\link{phylo_data}}.}

  \item{comments}{Character vector. See
  \code{\link{phylo_data}}.}

  \item{html.args}{List. See \code{\link{phylo_data}}.}

  \item{...}{Optional arguments. See
  \code{\link{phylo_data}}.}
}
\value{
  Character vector or \code{NULL}.
}
\description{
  This is the helper function used by
  \code{\link{phylo_data}} for converting the data to a
  character vector.
}
\keyword{internal}

\docType{methods}
\alias{format-methods}
\alias{format,CMAT-method}
\usage{
  \S4method{format}{CMAT}(x, how, enclose, digits, indent,
    paup.block, comments, html.args, ...) 

}
