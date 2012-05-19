\name{case}
\alias{case}
\title{Modified switch function}
\arguments{
  \item{EXPR}{A scalar based on which a decision is made.
  If of mode \sQuote{logical}, exactly two other elements
  must be passed, the first of which is chosen if
  \code{EXPR} is \code{TRUE}, the second if it is
  \code{FALSE}. If \code{EXPR} is a character scalar, the
  behaviour is like the one of \code{switch} with the
  exception that unmatched values within \code{...} cause
  an error. If \code{EXPR} is of mode \sQuote{numeric}, the
  behaviour is like \code{switch} but counting starts at 0
  and values larger than the number of elements within
  \code{...} select the last element. It is an error if
  \code{EXPR} is negative.}

  \item{x}{Chosen if \code{EXPR} is \code{TRUE}.}

  \item{y}{Chosen if \code{EXPR} is \code{FALSE}.}

  \item{...}{Additional arguments from which to select an
  alternative.}
}
\value{
  Selected value of \code{...}, \code{x} or \code{y}.
}
\description{
  An altered switch statement for flow control.
}
\seealso{
  base::switch
}
\keyword{internal}

\docType{methods}
\alias{case-methods}
\alias{case,logical-method}
\alias{case,numeric-method}
\alias{case,character-method}
\usage{
  \S4method{case}{logical}(EXPR, x, y) 

  \S4method{case}{numeric}(EXPR, ...) 

  \S4method{case}{character}(EXPR, ...) 

}
