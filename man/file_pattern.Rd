\name{file_pattern}
\alias{file_pattern}
\title{File pattern}
\usage{
  file_pattern(type = c("both", "csv", "yaml", "json", "any", "empty"),
    compressed = TRUE, literally = inherits(type, "AsIs"))
}
\arguments{
  \item{type}{Character scalar indicating the file types to
  be matched by extension. Alternaticely, directly the
  extension or extensions, or a list of file names (not
  \code{NA}).}

  \item{compressed}{Logical scalar. Shall compressed files
  also be matched?  This affects the returned pattern as
  well as the pattern used for extracting file extensions
  from complete file names (if \code{literally} is
  \code{TRUE}).}

  \item{literally}{Logical scalar. Interpret \code{type}
  literally? This also allows for vectors with more than a
  single element, as well as the extraction of file
  extensions from file names.}
}
\value{
  Character scalar, holding a regular expression.
}
\description{
  Create a regular expression matching certain file
  extensions. This is not normally directly called by an
  \pkg{opm} user but by the other IO functions of the
  package.
}
\examples{
(x <- file_pattern())
(y <- file_pattern(type = "csv", compressed = FALSE))
stopifnot(nchar(x) > nchar(y))
# constructing pattern from existing files
(files <- list.files(pattern = "[.]"))
(x <- file_pattern(I(files))) # I() causes 'literally' to be TRUE
stopifnot(grepl(x, files, ignore.case = TRUE))
}
\seealso{
  tools::file_ext

  Other io-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm}}, \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{opm_files}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{utilities}

