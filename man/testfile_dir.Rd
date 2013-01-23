\name{testfile_dir}
\alias{testfile_dir}
\title{Test file directory}
\usage{
  testfile_dir(files = NULL)
}
\arguments{
  \item{files}{Character vector. Optional list of of
  filenames to append to the directory name. They would be
  removed again unless all of them did in fact exist.}
}
\value{
  Name of the directory in which the input files for
  testing reside.
}
\description{
  Name of directory with test files. The purpose of this
  function is to guarantee to always find these files
  irrespective of whether the package was loaded using
  \code{library()} (e.g., during \acronym{CRAN} checking)
  or using \code{source()} (optionally used during
  development).
}
\keyword{internal}

