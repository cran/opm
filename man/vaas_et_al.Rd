\docType{data}
\name{vaas_et_al}
\alias{vaas_et_al}
\title{Example dataset from Vaas et al. (2012)}
\format{\code{\link{OPMS}} object with the dimensions 114 x 384 x 96, i.e.
  114 plates with 384 time points and 96 wells per plate. (10 plates have
  364, 365, 368 or 371 time points, respectively; the remaining 74 plates
  have 384 time points).}
\description{
  This \code{\link{OPMS}} object contains all measurements
  from the study by Vaas et al. (2012). Metadata have been
  added to fully describe the conducted PM experiments. The
  plate type is \sQuote{Generation III}, but the running
  mode was as for PM plates. Four bacterial strains from
  two species were considered in the study. For the three
  publicly accessible ones, the URLs of their DSMZ catalog
  entries are given below.
}
\examples{
\dontrun{

# Calling this yielded a variable vaas_et_al containing the data. The
# opm package must be loaded beforehand using library().
data(vaas_et_al)
}
}
\references{
  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
}
\keyword{dataset}

