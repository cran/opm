\name{html_args}
\alias{html_args}
\alias{safe_labels}
\title{\acronym{HTML} formatting and output label generation}
\usage{
  html_args(character.states = c(`negative reaction` = "-", `weak reaction` = "w",
    `positive reaction` = "+"),
    multiple.sep = "/", organisms.start = "Organisms: ",
    states.start = "Symbols: ", legend.dot = TRUE,
    legend.sep.1 = ", ", legend.sep.2 = "; ",
    table.summary = "character matrix",
    greek.letters = TRUE, css.file = opm_opt("css.file"),
    ...)

  safe_labels(x, format, enclose = TRUE, pad = FALSE,
    comment = FALSE)
}
\arguments{
  \item{character.states}{Character vector used for mapping
  integers to the elements in the corresponding position.
  It is also used in conjunction with its names to create
  the table legend. The default value is useful for data of
  mode \sQuote{logical}, mapping \code{FALSE}, \code{NA}
  and \code{TRUE}, in this order. Data of this kind are by
  default internally converted to an according integer
  vector.}

  \item{multiple.sep}{Character scalar used for joining
  multiple-state characters together.}

  \item{organisms.start}{Character scalar prepended to the
  organism part of the table legend. Ignored if empty.}

  \item{states.start}{Character scalar prepended to the
  character-states part of the table legend. Ignored if
  empty.}

  \item{legend.dot}{Logical scalar indicating whether or
  not a dot shall be appended to the table-legend entries.}

  \item{legend.sep.1}{Character scalar used for the first
  pass of joining the table-legend entries together.}

  \item{legend.sep.2}{Character scalar used for the second
  pass of joining the table-legend entries together.}

  \item{table.summary}{Character scalar inserted as
  \sQuote{summary} attribute of the resulting \code{HTML}
  table.}

  \item{greek.letters}{Logical scalar indicating whether or
  not letters between \sQuote{a} and \sQuote{e} within
  substrate names should be converted to the corresponding
  Greek letters.}

  \item{css.file}{Character vector indicating the name of
  one to several \acronym{CSS} files to link. Empty strings
  and empty vectors are ignored.  It is no error if the
  file does not exist, but the page will then probably not
  be displayed as intended.

  Under Windows it is recommended to convert a file name
  \code{f} beforehand using \code{normalizePath(f, winslash
  = "/")}.}

  \item{...}{Optional other arguments available for
  inserting user-defined \acronym{HTML} content. Currently
  the following ones (in their order of insertion) are not
  ignored, and can even be provided several times:
  \describe{ \item{meta}{Used as (additional) \sQuote{meta}
  entries within the \acronym{HTML} head.}
  \item{headline}{Override the use of the \code{title}
  argument as headline (placed above the table legend). An
  empty argument would turn it off.} \item{prepend}{List or
  character vector to be inserted before the table legend.
  Lists are converted recursively. List names will be
  converted to \sQuote{title} and \sQuote{class} attributes
  (if missing, names are inferred from the nesting level;
  see \code{\link{opm_opt}}, entry \code{html.class}).
  Names of other vectors, if any, are converted to
  \sQuote{title} and \sQuote{span} attributes. Character
  vectors are converted using \code{\link{safe_labels}}
  unless they inherit from \code{AsIs} (see \code{I} from
  the \pkg{base} package).} \item{insert}{As above, but
  inserted between the legend and the table.}
  \item{append}{As above, but inserted after the table.} }}

  \item{x}{Character vector or convertible to such.}

  \item{format}{Character scalar. See
  \code{\link{phylo_data}}.}

  \item{enclose}{Logical scalar. See
  \code{\link{phylo_data}} and the description of
  \code{comment}.}

  \item{pad}{Logical scalar. Bring labels to the same
  number of characters by appending spaces? Has no effect
  for \acronym{PHYLIP} and \acronym{HTML} output format.}

  \item{comment}{Logical scalar. If \code{TRUE}, comments
  as used in the respective format will be produced.
  \acronym{PHYLIP} and \acronym{EPF} do not accept comments
  and will yield an error. If \code{enclose} is
  \code{TRUE}, the comment-enclosing characters are
  appended and prepended to the vector, otherwise to each
  string separately.}
}
\value{
  List of \acronym{HTML} arguments or character vector with
  modified labels.
}
\description{
  These are helper functions for \code{\link{phylo_data}}
  allowing for either the easy fine-tuning of the generated
  \acronym{HTML} output or for the conversions of strings
  to safe phylogenetic taxon labels.
}
\details{
  These functions are not normally called directly by an
  \pkg{opm} user but by \code{\link{phylo_data}}; see there
  for their usual application. The \code{\link{phylo_data}}
  methods for \code{OPMD_Listing} and \code{OPMS_Listing}
  objects do not support all \acronym{HTML} formatting
  options.

  Label cleaning invokes either the replacement of
  disallowed characters or the enclosing of all labels in
  single quotes and the doubling of already existing single
  quotes, if any.
}
\examples{
# Some animals you might know
x <- c("Elephas maximus", "Loxodonta africana", "Giraffa camelopardalis")

(y <- safe_labels(x, "phylip"))
stopifnot(nchar(y) == 10L) # truncation

(y <- safe_labels(x, "epf"))
stopifnot(nchar(y) == nchar(x)) # changes in length unnecessary
(y <- safe_labels(x, "epf", pad = TRUE))
stopifnot(nchar(y) == 22) # padded to uniform length

(y <- safe_labels(x, "nexus", enclose = TRUE))
stopifnot(grepl("^'.*'$", y)) # all strings enclosed in sinqle quotes
}
\seealso{
  base::normalizePath base::I base::gsub

  Other phylogeny-functions: \code{\link{phylo_data}}
}
\keyword{IO}
\keyword{character}
\keyword{cluster}

