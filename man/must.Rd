\name{must}
\alias{must}
\title{Convert warnings to errors}
\usage{
  must(expr, ...)
}
\arguments{
  \item{expr}{R expression to evaluate.}

  \item{...}{Optional further arguments to
  \code{tryCatch}.}
}
\value{
  The result of \code{expr} (if no error occurs).
}
\description{
  Raise an error if a warning occurs. Useful for making
  certain tests more strict.
}
\keyword{internal}

