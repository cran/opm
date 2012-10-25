\docType{data}
\name{vaas_4}
\alias{vaas_4}
\title{Example dataset from Vaas et al. (2012), four selected plates}
\format{\code{\link{OPMS}} object with the dimensions 4 x 384 x 96, i.e.
  4 plates with 384 time points and 96 wells per plate.}
\description{
  This \code{\link{OPMS}} object contains measurements from
  four selected plates from the study by Vaas et al.
  (2012). Metadata have been added to fully describe the
  conducted PM experiments: these plates are the 6th
  technical replicate from the first biological replicate
  for the four bacterial strains considered in the study.
  The complete data set is available as \code{vaas_et_al}
  in the \pkg{opmdata} package.
}
\examples{
\dontrun{

# Calling this yielded a variable vaas_4 containing the data. The opm
# package must be loaded beforehand using library().
data(vaas_4)
}
}
\references{
  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.

  Selezska, K., Kazmierczak, M., Muesken, M., Garbe, J.,
  Schobert, M., Haeussler, S., Wiehlmann, L., Rohde, C.,
  Sikorski, J. 2012 \emph{Pseudomonas aeruginosa}
  population structure revisited under environmental focus:
  impact of water quality and phage pressure.
  \emph{Environmental Microbiology} \strong{14},
  1952--1967.

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
}
\keyword{datasets}
\keyword{datasets}

