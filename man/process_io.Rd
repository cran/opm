\name{process_io}
\alias{process_io}
\title{Conversion between two files}
\usage{
  process_io(files, io.fun, fun.args = list(),
    overwrite = c("no", "older", "yes"), verbose = TRUE)
}
\arguments{
  \item{files}{Two-element character vector containing the
  input and output files.}

  \item{io.fun}{Conversion function. Should accept
  \code{infile} and \code{outfile} as the first two
  arguments.}

  \item{fun.args}{Optional list of further arguments of
  \code{io.fun}.}

  \item{overwrite}{Character scalar. If \sQuote{yes},
  conversion is always tried if \code{infile} exists and is
  not empty. If \sQuote{no}, conversion is not tried if
  \code{outfile} exists and is not empty. If
  \sQuote{older}, conversion is tried if \code{outfile}
  does not exist or is empty or is older than \code{infile}
  (with respect to the modification time).}

  \item{verbose}{Logical scalar. Print conversion and
  success/failure information?}
}
\value{
  Character vector corresponding to one of the rows of the
  result of \code{\link{batch_process}}.
}
\description{
  Convert data from an input file to data in an output
  file.
}
\keyword{internal}

