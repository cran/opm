\name{phylo_data}
\alias{phylo_data}
\title{Export phylogenetic data}
\arguments{
  \item{object}{Matrix. Currently only \sQuote{integer},
  \sQuote{logical}, \sQuote{numeric} and \sQuote{character}
  content is supported.}

  \item{format}{Character scalar, either \sQuote{epf}
  (Extended Phylip Format), \sQuote{nexus} or
  \sQuote{phylip}. The main difference between \sQuote{epf}
  and \sQuote{phylip} is that the former can use labels
  with more than ten characters, but its labels must not
  contain whitespace. If \sQuote{nexus} format is chosen, a
  non-empty \code{comment} attribute will be output
  together with the data (and appropriately escaped).}

  \item{outfile}{Character scalar. If a non-empty character
  scalar, resulting lines are directly written to this
  file. Otherwise, they are returned.}

  \item{enclose}{Logical scalar. Shall labels be enclosed
  in single quotes?  Ignored unless \code{format} is
  \sQuote{nexus}.}

  \item{indent}{Integer scalar. Indentation of subcommands
  in NEXUS format.  Ignored unless \code{format} is
  \sQuote{nexus} (and a matter of taste anyway).}

  \item{paup.block}{Logical scalar. Append a PAUP* block
  with selected default values?}
}
\value{
  Character vector, each element representing a line in a
  potential output file, returned invisibly if
  \code{outfile} is given.
}
\description{
  Create entire character matrix (include header and
  footer) in a file format suitable for exporting
  phylogenetic data. Return it or write it to a file.
}
\note{
  For exporting NEXUS format, the matrix should normally be
  converted beforehand by applying
  \code{\link{discrete,matrix}}.
}
\examples{
x <- matrix(c(0:9, letters[1:22]), nrow = 2)
colnames(x) <- LETTERS[1:16]
rownames(x) <- c("Ahoernchen", "Behoernchen") # Chip and Dale in German

(y.epf <- phylo_data(x, format = "epf"))
stopifnot(is.character(y.epf), length(y.epf) == 3)

(y.phylip <- phylo_data(x, format = "phylip"))
stopifnot((y.epf == y.phylip) == c(TRUE, TRUE, FALSE))

(y.nexus <- phylo_data(x, format = "nexus"))
nexus.len.1 <- length(y.nexus)
stopifnot(is.character(y.nexus), nexus.len.1 > 10)

comment(x) <- c("This", "is", "a", "test")
(y.nexus <- phylo_data(x, format = "nexus"))
stopifnot(identical(length(y.nexus), nexus.len.1 + 7L))
}
\references{
  Berger SA, Stamatakis A. Accuracy of morphology-based
  phylogenetic fossil placement under maximum likelihood.
  2010; 8th ACS/IEEE International Conference on Computer
  Systems and Applications (AICCSA-10), Hammamet, Tunisia
  [analysis of phenotypic data wih RAxML].

  Felsenstein J. PHYLIP (Phylogeny Inference Package)
  version 3.6.  Distributed by the author. 2005; Department
  of Genome Sciences, University of Washington, Seattle
  [the PHYLIP program].

  Maddison DR, Swofford DL, Maddison WP. Nexus: An
  extensible file format for systematic information. Syst
  Biol 1997; 46:590-621 [the NEXUS format].

  Stamatakis A. RAxML-VI-HPC: Maximum likelihood-based
  phylogenetic analyses with thousands of taxa and mixed
  models Bioinformatics 2006; 22:2688-2690. [the RAxML
  program].

  Swofford DL. PAUP*: Phylogenetic Analysis Using Parsimony
  (*and Other Methods), Version 4.0 b10. 2002; Sinauer
  Associates, Sunderland [the PAUP* program].
}
\seealso{
  comment write

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}},
  \code{\link{to_metadata,character}}

  Other phylogeny-functions: \code{\link{discrete}},
  \code{\link{discrete,matrix}}, \code{\link{safe_labels}}
}
\keyword{IO}
\keyword{character}
\keyword{cluster}

\docType{methods}
\alias{phylo_data-methods}
\alias{phylo_data,matrix-method}
\usage{
  \S4method{phylo_data}{matrix}(object, format = "epf",
    outfile = "", enclose = TRUE, indent = 3L, paup.block = FALSE) 
}
