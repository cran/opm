\name{opm_files}
\alias{opm_files}
\title{Opm files}
\usage{
  opm_files(what = c("scripts", "testdata"))
}
\arguments{
  \item{what}{Character scalar indicating the subdirectory
  to search in.  Currently the following ones are included:
  \describe{ \item{scripts}{R script files for
  non-interactive uses of the \pkg{opm} package,
  particularly for the batch processing of many files. When
  called without input arguments or with the \sQuote{-h}
  switch, the scripts output usage information.}
  \item{testdata}{Files as output by the OmniLog(R) device
  for testing data input and metadata management.} }}
}
\value{
  Character vector of filenames.
}
\description{
  Get list of files from the \pkg{opm} package of interest
  for the user.
}
\note{
  This might fail with unusual installations of the
  \pkg{opm} package.
}
\examples{
(x <- opm_files("scripts"))
(y <- opm_files("testdata"))
if (length(y) > 0) {
  stopifnot(grepl("\\\\.R$", x, ignore.case = TRUE))
  stopifnot(!grepl("\\\\.R$", y, ignore.case = TRUE))
}

# On UNIX systems you should be able to do this if Rscript and the optparse
# package are properly installed:
# invisible(sapply(opm_files(), function(f) system(paste("Rscript", f))))
# ...and get the usage messages of all scripts.
}
\seealso{
  base::list.files

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{phylo_data}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}}
}
\keyword{utilities}

