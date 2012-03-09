\name{traverse}
\alias{traverse}
\title{Traverse a list with a function}
\arguments{
  \item{object}{List.}

  \item{func}{Function to apply to each element of the
  list.}

  \item{cores}{Integer scalar. Number of cores to use. If
  more than one, a warning is issued if the \pkg{multicore}
  package is not available, and the number of cores is set
  back to 1.}

  \item{...}{Optional arguments to \code{lapply} or
  \code{mclapply} (can be arguments passed to
  \code{func}).}
}
\value{
  List.
}
\description{
  Apply a function to all list elements in turn, optionally
  in parallel using the \pkg{multicore} package.
}
\keyword{internal}

\docType{methods}
\alias{traverse-methods}
\alias{traverse,list,function-method}
\usage{
  \S4method{traverse}{list,function}(object, func, cores, 
    ...) 

}
