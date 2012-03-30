\name{phylo_data}
\alias{phylo_data}
\title{Export phylogenetic data}
\arguments{
  \item{object}{Matrix. Currently only \sQuote{integer},
  \sQuote{logical}, \sQuote{numeric} and \sQuote{character}
  content is supported.}

  \item{format}{Character scalar, either \sQuote{epf}
  (Extended Phylip Format), \sQuote{nexus}, \sQuote{phylip}
  or \sQuote{html}. The main difference between
  \sQuote{epf} and \sQuote{phylip} is that the former can
  use labels with more than ten characters, but its labels
  must not contain whitespace.  (These adaptations are done
  automatically with \code{\link{safe_labels}}.)  If
  \sQuote{nexus} format is chosen, a non-empty
  \code{comment} attribute will be output together with the
  data (and appropriately escaped).}

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

  \item{remove.constant}{Logical scalar. Remove substrates
  with constant results? This is currently ignored for
  formats other than \sQuote{html}.}

  \item{remove.ambig}{Logical scalar. Remove substrates
  with ambiguous results? This is currently ignored for
  formats other than \sQuote{html}.}

  \item{join}{Logical scalar. Join rows of \code{object}
  together according to \code{groups}? This can be used to
  deal with measurements repetitions for the same organism
  or treatment.}

  \item{groups}{Vector. Only used if \code{join} is
  \code{TRUE}; see there for details.}
}
\value{
  Character vector, each element representing a line in a
  potential output file, returned invisibly if
  \code{outfile} is given.
}
\description{
  Create entire character matrix (include header and
  footer) in a file format suitable for exporting
  phylogenetic data. Return it or write it to a file. This
  function can also produce HTML tables suitable for
  displaying PM data in taxonomic journals such as IJSEM.
}
\note{
  For exporting NEXUS format, the matrix should normally be
  converted beforehand by applying \code{\link{discrete}}.
  Even stricter is the \sQuote{html} setting, which
  requires the data to be discretized with \code{gap} set
  to \code{TRUE}.
}
\examples{
x <- matrix(c(0:9, letters[1:22]), nrow = 2)
colnames(x) <- LETTERS[1:16]
rownames(x) <- c("Ahoernchen", "Behoernchen") # Chip and Dale in German

(y.epf <- phylo_data(x, format = "epf"))
stopifnot(is.character(y.epf), length(y.epf) == 3)

(y.phylip <- phylo_data(x, format = "phylip"))
stopifnot((y.epf == y.phylip) == c(TRUE, FALSE, FALSE))

(y.nexus <- phylo_data(x, format = "nexus"))
nexus.len.1 <- length(y.nexus)
stopifnot(is.character(y.nexus), nexus.len.1 > 10)

comment(x) <- c("This", "is", "a", "test")
(y.nexus <- phylo_data(x, format = "nexus"))
stopifnot(identical(length(y.nexus), nexus.len.1 + 7L))

# Example with real data; see discrete() for the conversion
data(vaas_4)
x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  in.parens = FALSE)
x <- discrete(x, range = TRUE, gap = TRUE)
message(y <- phylo_data(x, format = "html"))
stopifnot(is.character(y), length(y) == 10, y[1:2] == c("<html>", "<body>"))

# Example with real data, joining the results per species
x <- extract(vaas_4, as.labels = list("Species"), in.parens = FALSE)
x <- discrete(x, range = TRUE, gap = TRUE)
message(y <- phylo_data(x, format = "html", join = TRUE))
stopifnot(is.character(y), length(y) == 10, y[1:2] == c("<html>", "<body>"))
}
\references{
  Berger, S. A., Stamatakis, A. 2010 Accuracy of
  morphology-based phylogenetic fossil placement under
  maximum likelihood. \emph{8th ACS/IEEE International
  Conference on Computer Systems and Applications
  (AICCSA-10).} Hammamet, Tunisia [analysis of phenotypic
  data wih RAxML].

  Felsenstein, J. 2005 PHYLIP (Phylogeny Inference Package)
  version 3.6. Distributed by the author. Seattle:
  University of Washington, Department of Genome Sciences
  [the PHYLIP program].

  Maddison, D. R., Swofford, D. L., Maddison, W. P. 1997
  Nexus: An extensible file format for systematic
  information. \emph{Syst Biol} \strong{46}, 590--621 [the
  NEXUS format].

  Stamatakis, A. 2006 RAxML-VI-HPC: Maximum
  likelihood-based phylogenetic analyses with thousands of
  taxa and mixed models \emph{Bioinformatics} \strong{22},
  2688--2690. [the RAxML program].

  Swofford, D. L. 2002 PAUP*: Phylogenetic Analysis Using
  Parsimony (*and Other Methods), Version 4.0 b10.
  Sunderland, Mass.: Sinauer Associates, [the PAUP*
  program].

  \url{http://ijs.sgmjournals.org/} [IJSEM journal]
}
\seealso{
  base::comment base::write

  Other IO-functions: \code{\link{batch_collect}},
  \code{\link{batch_opm_to_yaml}},
  \code{\link{batch_process}},
  \code{\link{clean_filenames}},
  \code{\link{collect_template}},
  \code{\link{explode_dir}}, \code{\link{file_pattern}},
  \code{\link{opm_files}}, \code{\link{read_opm}},
  \code{\link{read_single_opm}}, \code{\link{split_files}},
  \code{\link{to_metadata}}

  Other phylogeny-functions: \code{\link{discrete}},
  \code{\link{safe_labels}}
}
\keyword{IO}
\keyword{character}
\keyword{cluster}

\docType{methods}
\alias{phylo_data-methods}
\alias{phylo_data,matrix-method}
\usage{
  \S4method{phylo_data}{matrix}(object, format = "epf",
    outfile = "", enclose = TRUE, indent = 3L, paup.block = FALSE, 
    remove.constant = TRUE, remove.ambig = TRUE, join = FALSE, 
    groups = rownames(object)) 

}
