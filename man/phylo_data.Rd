\name{phylo_data}
\alias{phylo_data}
\title{Export phylogenetic data}
\arguments{
  \item{object}{Data frame, numeric matrix or \sQuote{OPMS}
  object (with aggregated values). Currently only
  \sQuote{integer}, \sQuote{logical}, \sQuote{numeric} and
  \sQuote{character} matrix content is supported. The
  data-frame and \sQuote{OPMS} methods first call
  \code{\link{extract}} and then the matrix method.}

  \item{format}{Character scalar, either \sQuote{epf}
  (Extended Phylip Format), \sQuote{nexus},
  \sQuote{phylip}, \sQuote{hennig} or \sQuote{html}. If
  \sQuote{nexus} or \sQuote{hennig} format are chosen, a
  non-empty \code{comment} attribute will be output
  together with the data (and appropriately escaped). In
  case of \sQuote{html} format, a non-empty \code{comment}
  yields the title of the HTML document. The main
  difference between \sQuote{epf} and \sQuote{phylip} is
  that the former can use labels with more than ten
  characters, but its labels must not contain whitespace.
  (These adaptations are done automatically with
  \code{\link{safe_labels}}.)}

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

  \item{delete}{Character scalar, one of \sQuote{uninf},
  \sQuote{ambig}, \sQuote{constant} or \sQuote{none}. If
  \sQuote{uninf}, columns are removed which are either
  constant (in the strict sense) or are columns in which
  some fields contain polymorphisms, and no pairs of fields
  share no character states. If \sQuote{ambig}, columns
  with ambiguities are removed. If \sQuote{constant},
  columns which are constant in the strict sense are
  removed. \code{delete} is currently ignored for formats
  other than \sQuote{html}, and note that columns become
  rows in the final HTML output.}

  \item{join}{Logical scalar, vector or factor. Unless
  \code{FALSE}, rows of \code{object} are joined together,
  either according to the row names (if \code{join} is
  \code{TRUE}), or directly according to \code{join}.  This
  can be used to deal with measurements repetitions for the
  same organism or treatment.}

  \item{digits}{Numeric scalar. Used for rounding, and thus
  ignored unless \code{object} is of mode
  \sQuote{numeric}.}

  \item{prefer.char}{Logical scalar. When dealing with
  \sQuote{logical} or \sQuote{integer} input, convert it to
  character instead of numeric data?  This fails for
  integer values > 9 or < 0.}

  \item{run.tidy}{Logical scalar. Filter the resulting HTML
  through the HTML Tidy program? Ignored unless
  \code{format} is \sQuote{html}. Otherwise, if
  \code{TRUE}, it is an error if the Tidy executable is not
  found.}

  \item{as.labels}{Vector of data-frame indices or
  \sQuote{OPMS} metadata entries. See
  \code{\link{extract}}.}

  \item{what}{Character scalar. See \code{\link{extract}}.}

  \item{sep}{Character scalar. See \code{\link{extract}}.}

  \item{subset}{Character scalar passed to the
  \sQuote{OPMS} method of \code{\link{extract}}.}

  \item{extract.args}{Optional list of arguments passed to
  that method.}

  \item{discrete.args}{Optional list of arguments passed
  from the \sQuote{OPMS} method to \code{\link{discrete}}.
  If set to \code{NULL}, discretization is turned off.}

  \item{...}{Optional arguments passed between the methods
  (i.e., from the other methods to the matrix method).}
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
  \itemize{ \item Exporting PM data in such formats allows
  one to either infer trees from the data under the
  maximum-likelihood and/or the maximum-parsimony
  criterion, or to reconstruct the evolution of PM
  characters on given phylogenetic trees.  \item For
  exporting NEXUS format, the matrix should normally be
  converted beforehand by applying \code{\link{discrete}}.
  Even stricter is the \sQuote{html} setting, which
  requires the data to be discretized with \code{gap} set
  to \code{TRUE}. The \sQuote{hennig} (Hennig86) format is
  the one used by TNT; it allows continuous characters to
  be analysed as such.  \item \sQuote{epf} or
  \sQuote{extended PHYLIP} is sometimes called
  \sQuote{relaxed PHYLIP}.  \item The generated HTML is
  guaranteed to produce neither errors nor warnings if
  checked using HTML Tidy. }
}
\examples{
x <- matrix(c(0:9, letters[1:22]), nrow = 2)
colnames(x) <- LETTERS[1:16]
rownames(x) <- c("Ahoernchen", "Behoernchen") # Chip and Dale in German

(y.epf <- phylo_data(x, format = "epf"))
stopifnot(is.character(y.epf), length(y.epf) == 3)
stopifnot(identical(y.epf, phylo_data(as.data.frame(x), what = "factor",
  format = "epf")))

(y.phylip <- phylo_data(x, format = "phylip"))
stopifnot((y.epf == y.phylip) == c(TRUE, FALSE, FALSE))

(y.nexus <- phylo_data(x, format = "nexus"))
nexus.len.1 <- length(y.nexus)
stopifnot(is.character(y.nexus), nexus.len.1 > 10)

comment(x) <- c("This", "is", "a", "test")
(y.nexus <- phylo_data(x, format = "nexus"))
stopifnot(identical(length(y.nexus), nexus.len.1 + 3L))

(y.hennig <- phylo_data(x, format = "hennig"))
hennig.len.1 <- length(y.hennig)
stopifnot(is.character(y.hennig), hennig.len.1 > 10)

comment(x) <- NULL
(y.hennig <- phylo_data(x, format = "hennig"))
stopifnot(identical(length(y.hennig), hennig.len.1 - 3L))

# Example with real data; see discrete() for the conversion
data(vaas_4)
x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  in.parens = FALSE)
x <- discrete(x, range = TRUE, gap = TRUE)
(y <- phylo_data(x, format = "html"))
stopifnot(is.character(y), length(y) == 16)
stopifnot(c("</html>", "</head>", "</body>", "") \%in\% y)

# Example with real data, joining the results per species
x <- extract(vaas_4, as.labels = list("Species"), in.parens = FALSE)
x <- discrete(x, range = TRUE, gap = TRUE)
(y <- phylo_data(x, format = "html", join = TRUE))
stopifnot(is.character(y), length(y) == 16)
stopifnot(c("</html>", "</head>", "</body>", "") \%in\% y)

# 'OPMS' method
(yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
  join = TRUE, extract.args = list(in.parens = FALSE)))
stopifnot(identical(y, yy))

# Effect of deletion
(y <- phylo_data(x, "html", delete = "none", join = FALSE))
(y.noambig <- phylo_data(x, "html", delete = "ambig", join = FALSE))
stopifnot(identical(y, y.noambig))
# ambiguities are created by joining only
longer <- function(x, y) {
  any(nchar(x) > nchar(y)) && !any(nchar(x) < nchar(y))
}
(y <- phylo_data(x, "html", delete = "none", join = TRUE))
(y.noambig <- phylo_data(x, "html", delete = "ambig", join = TRUE))
stopifnot(longer(y, y.noambig))
(y.nouninf <- phylo_data(x, "html", delete = "uninf", join = TRUE))
stopifnot(longer(y.noambig, y.nouninf))
(y.noconst <- phylo_data(x, "html", delete = "const", join = TRUE))
stopifnot(longer(y.noconst, y.nouninf))
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

  Goloboff, P.A., Farris, J.S., Nixon, K.C. 2008 TNT, a
  free program for phylogenetic analysis. \emph{Cladistics}
  \strong{24}, 774--786 [the TNT program].

  Goloboff, P.A., Mattoni, C., Quinteros, S. 2005
  Continuous characters analyzed as such. \emph{Cladistics}
  \strong{22}, 589--601.

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

  \url{http://tidy.sourceforge.net/} [HTML Tidy]
}
\seealso{
  base::comment base::write

  Other phylogeny-functions: \code{\link{best_cutoff}},
  \code{\link{discrete}}, \code{\link{safe_labels}}
}
\keyword{IO}
\keyword{character}
\keyword{cluster}

\docType{methods}
\alias{phylo_data-methods}
\alias{phylo_data,matrix-method}
\alias{phylo_data,data.frame-method}
\alias{phylo_data,OPMS-method}
\usage{
  \S4method{phylo_data}{matrix}(object, 
    format = opm_opt("phylo.fmt"), outfile = "", enclose = TRUE, indent = 3L,
    paup.block = FALSE, delete = "uninf", join = FALSE, 
    digits = opm_opt("digits"), prefer.char = FALSE, run.tidy = FALSE) 

  \S4method{phylo_data}{data.frame}(object, as.labels = NULL, 
    what = "numeric", sep = " ", ...) 

  \S4method{phylo_data}{OPMS}(object, as.labels, subset = "A",
    sep = " ", extract.args = list(), 
    discrete.args = list(range = TRUE, gap = TRUE), ...) 

}
