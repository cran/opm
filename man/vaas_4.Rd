\docType{data}
\name{vaas_4}
\alias{vaas_1}
\alias{vaas_4}
\title{Example data sets from Vaas et al. (2012)}
\format{\code{vaas_4} is an \code{\link{OPMS}} object with the dimensions 4 x
  384 x 96, i.e. 4 plates with 384 time points and 96 wells per plate.
  \code{vaas_1} is an \code{\link{OPMD}} object with the dimensions 384 x 96,
  i.e. a single plate with 384 time points and 96 wells.}
\description{
  Two literature example data sets are provided with the
  \pkg{opm} package, containing one or four plates,
  respectively.
}
\details{
  The \code{\link{OPMS}} object \code{vaas_4} contains
  measurements from four selected plates from the study by
  Vaas et al. (2012). Metadata have been added to fully
  describe the conducted \acronym{PM} experiments: these
  plates are the sixth technical replicate from the first
  biological replicate for the four bacterial strains
  considered in the study.

  The \code{\link{OPMD}} object \code{vaas_1} contains
  measurements from a single selected plate from the study
  by Vaas et al. (2012). Metadata have been added to fully
  describe the conducted \acronym{PM} experiments: this
  plate is the sixth technical replicate from the first
  biological replicate for the strain \emph{Escherichia
  coli} \acronym{DSM} 30083T (yes, the type strain of
  \emph{E. coli}). This is a subset of \code{vaas_4}.

  The complete data set is available as \code{vaas_et_al}
  in the \pkg{opmdata} package.
}
\examples{
\dontrun{

# Calling this yielded a variable vaas_4 containing the data. The opm
# package must be loaded beforehand using library().
data(vaas_4)

# Calling this yielded a variable vaas_1 containing the data. The opm
# package must be loaded beforehand using library().
data(vaas_1)
}
}
\references{
  Bochner, B.R., Savageau, M.A. 1977. Generalized indicator
  plate for genetic, metabolic, and taxonomic studies with
  microorganisms. \emph{ Applied and Environmental
  Microbiology} \strong{33}, 434--444.

  Selezska, K., Kazmierczak, M., Muesken, M., Garbe, J.,
  Schobert, M., Haeussler, S., Wiehlmann, L., Rohde, C.,
  Sikorski, J. 2012 \emph{Pseudomonas aeruginosa}
  population structure revisited under environmental focus:
  impact of water quality and phage pressure.
  \emph{Environmental Microbiology} \strong{14},
  1952--1967.

  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-1707.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-18039.html}

  \url{http://www.dsmz.de/catalogues/details/culture/DSM-30083.html}
}
\keyword{datasets}
\keyword{datasets}

