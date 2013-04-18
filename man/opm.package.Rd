\docType{package}
\name{opm.package}
\alias{opm.package}
\alias{opm.package-package}
\title{The opm package}
\description{
  Package for analysing OmniLog Phenotype MicroArray data.
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
  \code{\link{OPMA}}, \code{\link{OPMD}} or
  \code{\link{OPMS}}. The documentation entry for each
  class contains a complete list of its user-level methods.
  Conceptually, all these classes store PM data; they
  differ in whether they also contain aggregated values
  (OPMA) or aggregated and discretized values (OPMD), and
  whether they contain more than a single plate (OPMS).
  Note that objects of the class OPMS may also contain
  aggregated or both aggregated and discretized values.}
  \item{methods}{The class \code{\link{OPMA}} inherits from
  the class \code{\link{OPM}} and, hence, has all its
  methods, even though they are not listed as
  \code{\link{OPMA}} methods. \code{\link{OPM}} itself
  inherits from \code{\link{WMD}} and has all its methods.
  Objects of the \code{\link{OPMS}} class act as containers
  for \code{\link{OPM}} and/or \code{\link{OPMA}} objects,
  and its methods usually traverse the contained objects in
  some manner.} \item{input}{Most \pkg{opm} users will
  start with data input using either
  \code{\link{read_single_opm}} or \code{\link{read_opm}},
  which create the appropriate objects.
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray data are structured in
  \strong{plates}. Each plate has 12 x 8 \strong{well}
  layout, and each well contains the respiration
  measurements on one substrate or inhibitor, or
  combination of substrates or inhibitors. For input
  example files, see \code{\link{opm_files}}.} \item{global
  options}{Options affecting the default parameters of a
  number of \pkg{opm} functions can be set and queried for
  using \code{\link{opm_opt}}.} \item{coercion
  functions}{Coercion functions for the three classes are
  only briefly listed under each class documentation entry.
  For details, see the documentation of \code{as} from the
  \pkg{methods} package.} \item{scalars}{This documentation
  uses the term \sQuote{scalar} for single-element vectors,
  even though technically these are, well, vectors.}
  \item{YAML}{Input and output of \acronym{YAML} files is
  based on the \pkg{yaml} package. Up to \pkg{opm} version
  0.7, this package was not required for the installation
  of \pkg{opm}. It is now mandatory to install one of the
  newer versions of \pkg{yaml} (>= v2.1.5). These are based
  on libyaml as parser instead of Syck, are faster and
  contain some bug fixes. The \acronym{YAML}-related
  functions of \pkg{opm} are \code{\link{to_yaml}} and
  \code{\link{batch_opm}}.} \item{running
  time}{Computations on such high-dimensional data may take
  some time. The limiting steps are aggregating
  (curve-parameter estimation) and plotting many curves
  together. The former step can be conducted in parallel if
  \code{mclapply} from the \pkg{parallel} package can be
  run with more than 1 core (basically anywhere except for
  Windows).  There is also a fast estimation method for the
  parameters \sQuote{area under the curve} and
  \sQuote{maximum height}. See \code{\link{do_aggr}} and
  the methods it refers to for details.} \item{advanced
  plotting}{The \pkg{gplots} package is also not required
  for the installation of \pkg{opm} but can be used to draw
  more advanced heatmaps. See \code{\link{heat_map}} and
  its accompanying methods for details.} }
}
\references{
  \url{http://www.biolog.com/}

  Bochner, B. R., Gadzinski, P., Panomitros, E. 2001
  Phenotype MicroArrays for high throughput phenotypic
  testing and assay of gene function. \emph{Genome
  Research} \strong{11}, 1246--1255.

  Bochner, B. R. 2009 Global phenotypic characterization of
  bacteria. \emph{FEMS Microbiological Reviews}
  \strong{33}, 191--205.

  \url{http://opm.dsmz.de/}

  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.

  \url{http://www.yaml.org/}
}
\keyword{package}

