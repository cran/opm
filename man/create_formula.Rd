\name{create_formula}
\alias{create_formula}
\title{Create formula}
\usage{
  create_formula(fmt, ..., .env = parent.frame())
}
\arguments{
  \item{fmt}{Character scalar. The format of the formula;
  omitting \sQuote{~} yields an error.}

  \item{...}{Passed to \code{sprintf} after joining. It is
  an error to not pass enough arguments.}

  \item{env}{Passed to \code{formula} as \sQuote{env}
  argument.}
}
\value{
  Formula.
}
\description{
  Construct a formula from a template.
}
\keyword{internal}

