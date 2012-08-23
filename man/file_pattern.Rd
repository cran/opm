\name{file_pattern}
\alias{file_pattern}
\title{File pattern}
\usage{
  file_pattern(type = c("both", "csv", "yaml", "any", "empty"),
    compressed = TRUE, literally = FALSE)
}
\arguments{
  \item{type}{Character scalar indicating the file types to
  be matched by extension.}

  \item{compressed}{Logical scalar. Shall compressed files
  also be matched?}

  \item{literally}{Logical scalar. Interpret \code{type}
  literally? Also allow vectors with more than a single
  element.}
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
}
\seealso{
  tools::file_ext

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{opm_files}},
  \code{\link{read_opm}}, \code{\link{read_single_opm}},
  \code{\link{split_files}}, \code{\link{to_metadata}}
}
\keyword{utilities}

