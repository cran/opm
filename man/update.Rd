\name{update}
\alias{update}
\title{Modify a CMAT object}
\arguments{
  \item{object}{An object of class \sQuote{CMAT}.}

  \item{how}{Characater scalar indicating how \code{object}
  should be modified.}

  \item{digits}{Integer scalar indicating the number of
  decimal points to consider when comparing \sQuote{double}
  values.}

  \item{na.rm}{Logical scalar passed to
  \code{\link{is_constant}} (if applicable).}
}
\value{
  Object of the same class than \code{object}.
}
\description{
  Modify a CMAT object in a specified way.
}
\keyword{internal}

\docType{methods}
\alias{update-methods}
\alias{update,CMAT-method}
\usage{
  \S4method{update}{CMAT}(object,
    how = c("NA2int", "delete.uninf", "delete.constant", "delete.ambig"),
    digits = opm_opt("digits"), na.rm = TRUE) 

}
