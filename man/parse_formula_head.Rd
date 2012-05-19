\name{parse_formula_head}
\alias{parse_formula_head}
\title{Turn the head of a formula into a vector}
\arguments{
  \item{object}{An object of class \sQuote{call},
  \sQuote{name} or \sQuote{vector} (S4-based).}
}
\value{
  Vector.
}
\description{
  If a formula has length 3, the second element represents
  the left part (the first element is the tilde). Once
  extracted using \code{[[}, the left part can be a call, a
  name or a vector. These methods convert it to a vector,
  aiming at generating a valid key for indexing a list.
}
\keyword{internal}

\docType{methods}
\alias{parse_formula_head-methods}
\alias{parse_formula_head,vector-method}
\alias{parse_formula_head,name-method}
\alias{parse_formula_head,call-method}
\usage{
  \S4method{parse_formula_head}{vector}(object) 

  \S4method{parse_formula_head}{name}(object) 

  \S4method{parse_formula_head}{call}(object) 

}
