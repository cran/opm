\name{tidy}
\alias{tidy}
\title{Check HTML using the Tidy program}
\arguments{
  \item{object}{Query character vector, or list of such
  vectors, or missing.  If missing, the location of the
  tidy executable is returned}

  \item{check}{Logical scalar. If \code{TRUE}, the Tidy
  checking results, potentially including warnings and
  error messages, are captured in a character vector.
  Otherwise the converted HTML is returned.}

  \item{args}{Character vector with arguments passed to
  HTML Tidy. Is is currently an error to set any of its
  \sQuote{File manipulation} options.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Character vector, or list of such vectors. If
  \code{object} is missing, the method returns the location
  of the HTML Tidy executable but \code{NULL} if it cannot
  be found.
}
\description{
  Run the HTML Tidy program for check or converting HTML
  character vectors.
}
\keyword{internal}

\docType{methods}
\alias{tidy-methods}
\alias{tidy,missing-method}
\alias{tidy,character-method}
\alias{tidy,list-method}
\usage{
  \S4method{tidy}{missing}() 

  \S4method{tidy}{character}(object, check = TRUE,
    args = c("-u", "-i")) 

  \S4method{tidy}{list}(object, ...) 

}
