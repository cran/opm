\docType{package}
\name{opm-package}
\alias{opm-package}
\title{The opm package}
\description{
  Package for analysing OmniLog(R) phenotype microarray
  data.
}
\note{
  As a brief guideline for using this manual, including the
  definition of frequently used terms, consider the
  following: \describe{ \item{families}{All functions and
  methods are assigned to one or several families of
  functions and methods with similar purposes. The
  respective other family members are listed under each
  \sQuote{See Also} entry.} \item{classes}{To make sense of
  this package, one usually has to create at least one
  object of one of the classes \code{\link{OPM}},
  \code{\link{OPMA}} or \code{\link{OPMS}}. The
  documentation entry for each class contains a complete
  list of its user-level methods.  Conceptually, all these
  classes store PM data; they differ in whether they also
  contain aggregated values (OPMA), and whether they
  contain more than a single plate (OPMS). Note that
  objects of the class OPMS may also contain aggregated
  values.} \item{input}{Most \pkg{opm} users will start
  with data input using either
  \code{\link{read_single_opm}} or \code{\link{read_opm}},
  which create the appropriate objects. OmniLog(R)
  phenotype microarray data are structured in
  \strong{plates}. Each plate has 12 x 8 \strong{well}
  layout, and each well contains the respiration
  measurements on one substrate or inhibitor, or
  combination of substrates or inhibitors.}
  \item{undocumented methods}{Some \code{\link{OPMS}}
  methods with the same use as the eponymous
  \code{\link{OPM}} method are only briefly listed in the
  documentation of the \code{\link{OPMS}} class.
  \code{\link{OPMA}} inherits from \code{\link{OPM}} and
  has all its methods, even though they are not listed as
  \code{\link{OPMA}} methods. \code{\link{OPM}} itself
  inherits from \code{\link{WMD}} and has all its methods.
  Objects of the \code{\link{OPMS}} class act as containers
  for \code{\link{OPM}} and/or \code{\link{OPMA}} objects,
  and its methods usually traverse the contained objects in
  some manner.} \item{coercion functions}{Coercion
  functions for the three classes are only briefly listed
  under each class documentation entry. For details, see
  the documentation of \code{as} from the \pkg{methods}
  package.} \item{scalars}{This documentation uses the term
  \sQuote{scalar} for single-element vectors, even though
  technically these are, well, vectors.} \item{YAML}{Input
  and output of YAML files needs the \pkg{yaml} package.
  Because this package is not required for the installation
  of \pkg{opm}, warnings or error messages will not occur
  before any of the YAML-dependent functions are called. We
  recommend installing one of the newer versions of
  \pkg{yaml} (>= v2.1.3) which are based on libyaml as
  parser instead of Syck, are faster and contain some bug
  fixes. The YAML-related functions of \pkg{opm} are
  \code{\link{to_yaml}} and
  \code{\link{batch_opm_to_yaml}}.} \item{running
  time}{Computations on such high-dimensional data may take
  some time. The limiting steps are aggregating
  (curve-parameter estimation) and plotting many curves
  together. The former step can be conducted in parallel if
  the \pkg{multicore} package is available. It is not
  required for the installation of \pkg{opm}. There is also
  fast estimation method for the parameters \sQuote{area
  under the curve} and \sQuote{maximum height}. See
  \code{\link{do_aggr}} and the methods it refers to for
  details.} \item{advanced plotting}{The \pkg{gplots}
  package is also not required for the installation of
  \pkg{opm} but can be used to draw more advanced heatmaps.
  See \code{\link{heat_map}} and its accompanying methods
  for details.} }
}
\references{
  \url{http://www.biolog.com/}

  Bochner BR, Gadzinski P, Panomitros E. Phenotype
  MicroArrays for high throughput phenotypic testing and
  assay of gene function. Genome Research 2001; 11:
  1246-1255.

  Bochner BR. Global phenotypic characterization of
  bacteria. FEMS Microbiological Reviews 2009; 33:191-205.

  \url{http://www.dsmz.de/}

  Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P.
  Visualization and curve parameter estimation strategies
  for efficient exploration of Phenotype Microarray
  kinetics. PLoS ONE 2012; in press.

  \url{http://www.yaml.org/}
}
\keyword{package}

