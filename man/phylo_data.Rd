\name{phylo_data}
\alias{phylo_data}
\title{Export phylogenetic data}
\arguments{
  \item{object}{Data frame, numeric matrix or \sQuote{OPMS}
  object (with aggregated values). Currently only
  \sQuote{integer}, \sQuote{logical}, \sQuote{double} and
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

  \item{paup.block}{Logical scalar. Append a
  \acronym{PAUP*} block with selected (recommended) default
  values? Has no effect unless \sQuote{nexus} is selected
  as \sQuote{format}.}

  \item{delete}{Character scalar. If \sQuote{uninf},
  columns are removed which are either constant (in the
  strict sense) or are columns in which some fields contain
  polymorphisms, and no pairs of fields share no character
  states. If \sQuote{ambig}, columns with ambiguities
  (multiple states in at least one single field) are
  removed. If \sQuote{constant}, columns which are constant
  in the strict sense are removed. \code{delete} is
  currently ignored for formats other than \sQuote{html},
  and note that columns become rows in the final
  \acronym{HTML} output.}

  \item{join}{Logical scalar, vector or factor. Unless
  \code{FALSE}, rows of \code{object} are joined together,
  either according to the row names (if \code{join} is
  \code{TRUE}), or directly according to \code{join}. This
  can be used to deal with measurements repetitions for the
  same organism or treatment.}

  \item{cutoff}{Numeric scalar. If joining results in
  multi-state characters, they can be filtered by removing
  all entries with a relative frequency less than
  \sQuote{cutoff}. Makes not much sense for non-integer
  numeric data.}

  \item{digits}{Numeric scalar. Used for rounding, and thus
  ignored unless \code{object} is of mode
  \sQuote{numeric}.}

  \item{comments}{Character vector. Comments to be added to
  the output (as title if \acronym{HTML} is chosen).
  Ignored if the output format does not allow for comments.
  If empty, a default comment is chosen.}

  \item{html.args}{List of arguments used to modify the
  generated \acronym{HTML}. See \code{\link{html_args}} for
  the supported list elements and their meaning.}

  \item{prefer.char}{Logical scalar indicating whether or
  not to use \code{NA} as intermediary character state. Has
  only an effect for \sQuote{logical} and \sQuote{integer}
  characters. A warning is issued if integers are not
  within the necessary range, i.e. either \code{0} or
  \code{1}.}

  \item{run.tidy}{Logical scalar. Filter the resulting
  \acronym{HTML} through the Tidy program? Ignored unless
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
  If set to \code{NULL}, discretization is turned off.
  Ignored if precomputed discretized values are chosen by
  setting \code{subset} to \sQuote{disc}.}

  \item{...}{Optional arguments passed between the methods
  (i.e., from the other methods to the matrix method) or to
  \code{hwrite} from the \pkg{hwriter} package. Note that
  \sQuote{table.summary} is set via \code{html.args} and
  that \sQuote{page}, \sQuote{x} and \sQuote{div} cannot be
  used.}
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
  function can also produce \acronym{HTML} tables suitable
  for displaying PM data in taxonomic journals such as
  \acronym{IJSEM}.
}
\details{
  \itemize{ \item Exporting PM data in such formats allows
  one to either infer trees from the data under the
  maximum-likelihood and/or the maximum-parsimony
  criterion, or to reconstruct the evolution of PM
  characters on given phylogenetic trees, or to nicely
  display the data in \acronym{HTML} format.  \item For
  exporting NEXUS format, the matrix should normally be
  converted beforehand by applying \code{\link{discrete}}.
  Exporting \acronym{HTML} is optimized for data
  discretized with \code{gap} set to \code{TRUE}. For other
  data, the \code{character.states} argument should be
  modified, see \code{\link{html_args}}. The
  \sQuote{hennig} (Hennig86) format is the one used by
  \acronym{TNT}; it allows continuous characters to be
  analysed as such.  \item \sQuote{epf} or \sQuote{extended
  PHYLIP} is sometimes called \sQuote{relaxed PHYLIP}.
  \item The generated \acronym{HTML} is guaranteed to
  produce neither errors nor warnings if checked using the
  Tidy program. It deliberately contains no formatting
  instructions but a rich annotation with \sQuote{class}
  attributes which allows for \acronym{CSS}-based
  formatting. This annotation includes the naming of all
  sections and all kinds of textual content. Whether the
  characters show differences between at least one organism
  and the others is also indicated. For the \acronym{CSS}
  files that come with the package, see the examples below
  and \code{\link{opm_files}}. }
}
\note{
  Regarding the meaning of \sQuote{character} as used here,
  see the \sQuote{details} section of
  \code{\link{discrete}}.
}
\examples{
# simple helper functions
echo <- function(x) write(x, file = "")
is_html <- function(x) is.character(x) &&
  c("<html>", "<head>", "<body>", "</html>", "</head>", "</body>") \%in\% x
longer <- function(x, y) any(nchar(x) > nchar(y)) &&
  !any(nchar(x) < nchar(y))

# dummy data set
x <- matrix(c(0:9, letters[1:22]), nrow = 2)
colnames(x) <- LETTERS[1:16]
rownames(x) <- c("Ahoernchen", "Behoernchen") # Chip and Dale in German

# EPF is a comparatively restricted format
echo(y.epf <- phylo_data(x, format = "epf"))
stopifnot(is.character(y.epf), length(y.epf) == 3)
stopifnot(identical(y.epf, phylo_data(as.data.frame(x), what = "factor",
  format = "epf")))

# PHYLIP is even more restricted (shorter labels!)
echo(y.phylip <- phylo_data(x, format = "phylip"))
stopifnot((y.epf == y.phylip) == c(TRUE, FALSE, FALSE))

# NEXUS allows for more content; note the comment and the character labels
echo(y.nexus <- phylo_data(x, format = "nexus"))
nexus.len.1 <- length(y.nexus)
stopifnot(is.character(y.nexus), nexus.len.1 > 10)

# adding a PAUP* block with (hopefully useful) default settings
echo(y.nexus <- phylo_data(x, format = "nexus", paup.block = TRUE))
stopifnot(is.character(y.nexus), length(y.nexus) > nexus.len.1)

# adding our own comment
comment(x) <- c("This is", "a test") # yields two lines
echo(y.nexus <- phylo_data(x, format = "nexus"))
stopifnot(identical(length(y.nexus), nexus.len.1 + 1L))

# Hennig86/TNT also includes the comment
echo(y.hennig <- phylo_data(x, format = "hennig"))
hennig.len.1 <- length(y.hennig)
stopifnot(is.character(y.hennig), hennig.len.1 > 10)

# without an explicit comment, the default one will be used
comment(x) <- NULL
echo(y.hennig <- phylo_data(x, format = "hennig"))
stopifnot(identical(length(y.hennig), hennig.len.1 - 1L))

# examples with real data and HTML
data(vaas_4)

# setting the CSS file that comes with opm as default
opm_opt(css.file = grep("[.]css$", opm_files("auxiliary"), value = TRUE))

# see discrete() for the conversion and note the OPMS example below: one
# could also get the results directly from OPMS objects
x <- extract(vaas_4, as.labels = list("Species", "Strain"),
  in.parens = FALSE)
x <- discrete(x, range = TRUE, gap = TRUE)
echo(y <- phylo_data(x, format = "html",
  html.args = html_args(organisms.start = "Strains: ")))
# this yields HTML with the usual tags, a table legend, and the table itself
# in a single line; the default 'organisms.start' could also be used
stopifnot(is_html(y))

# now with joining of the results per species (and changing the organism
# description accordingly)
x <- extract(vaas_4, as.labels = list("Species"), in.parens = FALSE)
x <- discrete(x, range = TRUE, gap = TRUE)
echo(y <- phylo_data(x, format = "html", join = TRUE,
  html.args = html_args(organisms.start = "Species: ")))
stopifnot(is_html(y))
# Here and in the following examples note the highlighting of the variable
# (uninformative or informative) characters. The uninformative ones are those
# that are not constant but show overlap regarding the sets of character
# states between all organisms. The informative ones are those that are fully
# distinct between all organisms.

# 'OPMS' method, yielding the same results than above but directly
echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
  join = TRUE, extract.args = list(in.parens = FALSE),
  html.args = html_args(organisms.start = "Species: ")))
# the timestamps might differ, but otherwise the result is as above
stopifnot(length(y) == length(yy) && length(which(y != yy)) < 2)

# appending user-defined sections
echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
  join = TRUE, extract.args = list(in.parens = FALSE),
  html.args = html_args(organisms.start = "Species: ",
  append = list(section.1 = "additional text", section.2 = "more text"))))
stopifnot(length(y) < length(yy), length(which(!y \%in\% yy)) < 2)
# note the position -- there are also 'prepend' and 'insert' arguments

# effect of deletion
echo(y <- phylo_data(x, "html", delete = "none", join = FALSE))
echo(y.noambig <- phylo_data(x, "html", delete = "ambig", join = FALSE))
stopifnot(length(which(y != y.noambig)) < 2) # timestamps might differ
# ambiguities are created only by joining
echo(y <- phylo_data(x, "html", delete = "none", join = TRUE))
echo(y.noambig <- phylo_data(x, "html", delete = "ambig", join = TRUE))
stopifnot(longer(y, y.noambig))
echo(y.nouninf <- phylo_data(x, "html", delete = "uninf", join = TRUE))
stopifnot(longer(y, y.nouninf))
echo(y.noconst <- phylo_data(x, "html", delete = "const", join = TRUE))
stopifnot(longer(y.noconst, y.nouninf))

# getting real numbers, not discretized ones
echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
  join = TRUE, extract.args = list(in.parens = FALSE), subset = "A",
  discrete.args = NULL,
  html.args = html_args(organisms.start = "Species: ")))
stopifnot(is_html(yy), length(yy) == length(y) - 1) # no symbols list
# the highlighting is also used here, based on the following heuristic:
# if mean+/-2*sd does not overlap, the character is informative; else
# if mean+/-sd does not overlap, the character is uninformative; otherwise
# is is constant

# this can also be used for formats other than HTML (but not all make sense)
echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "hennig",
  join = TRUE, extract.args = list(in.parens = FALSE), subset = "A",
  discrete.args = NULL))
stopifnot(is.character(yy), length(yy) > 10)
}
\references{
  Berger, S. A., Stamatakis, A. 2010 Accuracy of
  morphology-based phylogenetic fossil placement under
  maximum likelihood. \emph{8th ACS/IEEE International
  Conference on Computer Systems and Applications
  (AICCSA-10).} Hammamet, Tunisia [analysis of phenotypic
  data with RAxML].

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

  Swofford, D. L. 2002 \emph{PAUP*: Phylogenetic Analysis
  Using Parsimony (*and Other Methods), Version 4.0 b10}.
  Sunderland, Mass.: Sinauer Associates [the PAUP*
  program].

  \url{http://ijs.sgmjournals.org/} [IJSEM journal]

  \url{http://tidy.sourceforge.net/} [HTML Tidy]
}
\seealso{
  base::comment base::write hwriter::hwrite

  Other phylogeny-functions: \code{\link{html_args}},
  \code{\link{safe_labels}}
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
    paup.block = FALSE, delete = c("none", "uninf", "constant", "ambig"),
    join = FALSE, cutoff = 0, digits = opm_opt("digits"),
    comments = comment(object), html.args = html_args(),
    prefer.char = format == "html", run.tidy = FALSE, ...) 

  \S4method{phylo_data}{data.frame}(object, as.labels = NULL,
    what = "numeric", sep = " ", ...) 

  \S4method{phylo_data}{OPMS}(object, as.labels, subset = "disc",
    sep = " ", extract.args = list(), join = TRUE,
    discrete.args = list(range = TRUE, gap = TRUE), ...) 

}
