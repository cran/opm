\name{to_yaml}
\alias{to_yaml}
\title{Convert to YAML}
\arguments{
  \item{object}{Object of one of the classes belonging to
  \code{\link{YAML_VIA_LIST}}.}

  \item{sep}{Logical scalar. Prepend \acronym{YAML}
  document separator \verb{---}?}

  \item{line.sep}{Character scalar used as output line
  separator.}

  \item{json}{Logical scalar. Create \acronym{JSON} instead
  of \acronym{YAML}?  If so, \code{sep}, \code{line.sep}
  and \code{...} are ignored.}

  \item{listify}{Logical scalar indicating whether after
  conversion to a list its non-list elements should be
  converted to lists if they have names.  (Names of named
  vector are \strong{not} conserved by default in output
  \acronym{YAML}).}

  \item{...}{Optional other arguments passed to
  \code{as.yaml} from the \pkg{yaml} package.}
}
\value{
  Character scalar (\acronym{YAML} string).
}
\description{
  Convert some \R object to \acronym{YAML}. If the package
  \pkg{yaml} is not installed, a call of this function will
  result in an error.
}
\details{
  \acronym{YAML} is a useful data-serialization standard
  that is understood by many programming languages. It is
  particularly more human readable than \acronym{XML}, and
  vector-like data structures (such as Phenotype MicroArray
  measurements) can be much more compactly encoded.

  Many PM datasets at once can be batch-converted into
  \acronym{YAML} format using \code{\link{batch_opm}}. The
  output format for the child classes is described in
  detail there, as well as other aspects relevant in
  practice.

  \acronym{JSON} is a subset of \acronym{YAML} and (in most
  cases) can also be parsed by a \acronym{YAML} parser. For
  generating \acronym{JSON}, the \code{toJSON} function
  from the \pkg{rjson} package would be used.
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

  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{extract}}, \code{\link{extract_columns}},
  \code{\link{flatten}}, \code{\link{gen_iii}},
  \code{\link{merge}}, \code{\link{oapply}},
  \code{\link{plates}}, \code{\link{rep}},
  \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{unique}}
}
\keyword{IO}
\keyword{character}

\docType{methods}
\alias{to_yaml-methods}
\alias{to_yaml,YAML_VIA_LIST-method}
\usage{
  \S4method{to_yaml}{YAML_VIA_LIST}(object, sep = TRUE,
    line.sep = "\n", json = FALSE, listify = FALSE, ...) 

}
