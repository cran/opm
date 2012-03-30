\name{to_yaml}
\alias{to_yaml}
\title{Convert to YAML}
\arguments{
  \item{object}{Object of one of the classes belonging to
  \code{\link{YAML_VIA_LIST-class}}.}

  \item{sep}{Logical scalar. Prepend YAML document
  separator \verb{---}?}

  \item{line.sep}{Character scalar used as output line
  separator.}

  \item{...}{Optional other arguments passed to
  \code{as.yaml} from the \pkg{yaml} package.}
}
\value{
  Character scalar (YAML string).
}
\description{
  Convert some R object to YAML. If the package \pkg{yaml}
  is not installed, a call of this function will result in
  an error.
}
\note{
  Many PM datasets can be batch-converted into YAML format
  using \code{\link{batch_opm_to_yaml}}. The output format
  for the child classes is described in detail there.
}
\examples{
\dontrun{

# Let 'x' be a any convertible object
# Store the data in file 'out.yml' in YAML format.
write(to_yaml(x), "out.yml")
}
}
\references{
  \url{http://www.yaml.org/}
}
\seealso{
  yaml::as.yaml yaml::yaml.load_file

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{merge}}, \code{\link{oapply}},
  \code{\link{plates}}
}
\keyword{IO}
\keyword{character}

\docType{methods}
\alias{to_yaml-methods}
\alias{to_yaml,YAML_VIA_LIST-method}
\usage{
  \S4method{to_yaml}{YAML_VIA_LIST}(object, sep = TRUE,
    line.sep = "\n", ...) 

}
