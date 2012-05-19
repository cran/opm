\name{taste}
\alias{taste}
\title{Convert errors to their messages}
\usage{
  taste(expr, ...)
}
\arguments{
  \item{expr}{R expression to evaluate.}

  \item{...}{Optional further arguments to
  \code{tryCatch}.}
}
\value{
  The result of \code{expr} if no error occurs, otherwise
  the error message as character scalar.
}
\description{
  If an error occurs, return its message. The expression
  used has to ensure that its normal result can be
  distinguished from the error message.
}
\keyword{internal}

