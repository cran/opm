\docType{data}
\name{vaas_1}
\alias{vaas_1}
\title{Example dataset from Vaas et al. (2012), a single selected plate}
\format{\code{\link{OPMD}} object with the dimensions 384 x 96, i.e. a single
  plate with 384 time points and 96 wells.}
\description{
  This \code{\link{OPMA}} object contains measurements from
  a single selected plate from the study by Vaas et al.
  (2012). Metadata have been added to fully describe the
  conducted PM experiments: this plate is the 6th technical
  replicate from the first biological replicate for the
  strain \emph{Escherichia coli} DSM30083T (yes, the type
  strain of \emph{E. coli}). This is a a subset of
  \code{\link{vaas_4}}. The complete data set is available
  as \code{vaas_et_al} in the \pkg{opmdata} package.
}
\examples{
\dontrun{

# Calling this yielded a variable vaas_1 containing the data. The opm
# package must be loaded beforehand using library().
data(vaas_1)
}
}
\references{
  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
}
\keyword{datasets}
\keyword{datasets}

