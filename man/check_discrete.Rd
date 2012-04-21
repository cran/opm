\name{check_discrete}
\alias{check_discrete}
\title{Check phylogenetic characters}
\arguments{
  \item{object}{Character or numeric vector.}

  \item{joined}{Logical scalar. Expect the data to be
  joined beforehand?}

  \item{from.numeric}{Logical scalar. Ignored unless
  \code{joined} is \code{TRUE}.}
}
\value{
  \code{NULL}.
}
\description{
  Check discrete characters. Raise an error if the check
  fails.
}
\keyword{internal}

\docType{methods}
\alias{check_discrete-methods}
\alias{check_discrete,character-method}
\alias{check_discrete,numeric-method}
\alias{check_discrete,matrix-method}
\alias{check_discrete,ANY-method}
\usage{
  \S4method{check_discrete}{character}(object, joined,
    from.numeric = FALSE) 

  \S4method{check_discrete}{numeric}(object, joined,
    from.numeric = FALSE) 

  \S4method{check_discrete}{matrix}(object, joined,
    from.numeric = FALSE) 

  \S4method{check_discrete}{ANY}(object, joined,
    from.numeric = FALSE) 

}
