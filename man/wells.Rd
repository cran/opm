\name{wells}
\alias{listing}
\alias{wells}
\title{Listing of well names}
\arguments{
  \item{object}{\code{\link{OPM}} object,
  \code{\link{OPMS}} or \code{\link{MOPMX}} object or well
  name or index. If missing, defaults to the selection of
  all possible wells (for the default plate type, see
  below).}

  \item{full}{Logical scalar. Return the full names of the
  wells (if available) or just their coordinates on the
  plate? The following arguments have no effect if
  \code{full} is \code{FALSE}.}

  \item{in.parens}{Logical scalar. If \code{TRUE}, add the
  full name of the substrate in parentheses (or brackets)
  after the original name. If \code{FALSE}, replace by the
  full substrate name. Note that adding in parentheses (or
  brackets) is only done if the trimmed substrate names are
  not empty.}

  \item{max}{Numeric scalar. Maximum number of characters
  allowed in the names.  Longer names are truncated and the
  truncation is indicated by appending a dot.}

  \item{brackets}{Logical scalar. Use brackets instead of
  parentheses?}

  \item{clean}{Logical scalar. If \code{TRUE}, clean
  trimmed end of full substrate name from non-word
  characters; use an empty string if only the dot
  remained.}

  \item{word.wise}{Logical scalar. If \code{TRUE},
  abbreviation works by truncating each word separately,
  and removing vowels first.}

  \item{paren.sep}{Character scalar. What to insert before
  the opening parenthesis (or bracket). Currently only zero
  to many whitespace characters are allowed. The ability to
  insert a line break is the main purpose of this argument.
  Using the \sQuote{at sign} as value is the only
  alternative and also special, as it causes the plate name
  itself to be appended to the well coordinate (after an
  \sQuote{at sign}, without parentheses or brackets). So
  mapping is not actually done in that case but the
  resulting names are understood by certain other \pkg{opm}
  methods which can conduct the mapping at a later stage.}

  \item{downcase}{Logical scalar indicating whether full
  names should be (carefully) converted to lower case. This
  uses \code{\link{substrate_info}} in \kbd{downcase} mode;
  see there for details.}

  \item{rm.num}{Logical scalar indicating whether numbering
  (used in the case of replicated substrates per plate)
  should be stripped from the end of the full well names.}

  \item{plate}{Name of the plate type. Several ones can be
  given unless \code{object} is of class \code{\link{OPM}}
  or \code{\link{OPMS}}.  Normalisation as in
  \code{\link{plate_type}} is applied before searching for
  the substrate names but otherwise the match must be
  exact.}

  \item{simplify}{Logical scalar indicating whether the
  result should be simplified to a vector. This will never
  be done if more than a single column is contained, i.e.
  if data for more than a single plate type are queried
  for.}

  \item{...}{Optional arguments passed between the
  methods.}

  \item{x}{\code{\link{OPMD}}, \code{\link{OPMS}} or
  \code{well_coords_map} object.}

  \item{as.groups}{Key suitable for querying the metadata,
  or \code{NULL}. If non-empty, passed as eponymous
  argument to \code{\link{extract}}. Thus \code{TRUE} and
  \code{FALSE} can be used, creating either a single group
  or one per plate. The extracted metadata define groups
  for which the discretised data are aggregated.

  If \code{x} is an \code{\link{OPMD}} object and
  \code{as.groups} is not empty, it is used to create the
  row name of the single row of the resulting
  \code{OPMS_Listing} object. Otherwise an
  \code{OPMD_Listing} object is produced.}

  \item{cutoff}{Numeric scalar used if \sQuote{as.groups}
  is non-empty. If the relative frequency of the most
  frequent entry within the discretised values to be joined
  is below that cutoff, \code{NA} is used. Ignored if
  \code{x} is an \code{\link{OPMD}} object but added to the
  result if \code{as.groups} is non-empty.}

  \item{html}{Logical scalar. Convert to \acronym{HTML}?
  This involves Greek letters and paragraph (\sQuote{div})
  tags.}

  \item{sep}{Character scalar used for joining the
  \sQuote{as.groups} entries (if any).}

  \item{exact}{Logical scalar passed to
  \code{\link{metadata}}.}

  \item{strict}{Logical scalar also passed to
  \code{\link{metadata}}.}
}
\value{
  The \code{wells} methods return a named character vector
  or a named matrix of the S3 class \code{well_coords_map},
  depending on \code{simplify} and \code{plate}.

  The return value of the \code{listing} methods for
  \code{\link{OPMX}} objects is a character vector or
  matrix with additional class attribute
  \code{OPMD_Listing} or \code{OPMS_Listing}.

  The \code{well_coords_map} method creates a nested list
  of the class \code{well_coords_listing} which can be used
  in conjunction with \code{\link{to_yaml}} or
  \code{saveRDS} for externally storing well maps.  See the
  examples for details.
}
\description{
  Get the names of the wells contained in an
  \code{\link{OPMX}} object. Optionally the full substrate
  names can be added in parentheses or brackets or used
  instead of the coordinate, and trimmed to a given length.
  The \code{listing} methods create a textual listing of
  the discretised values. (See \code{\link{do_disc}} for
  generating discretised data.) This is useful to describe
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  phenotype microarray results in a scientific manuscript.
}
\details{
  Do not confuse \code{wells} this with \code{\link{well}}.
  The purpose of the \code{\link{OPM}} and
  \code{\link{OPMS}} methods for \code{wells} should be
  obvious. The default method is intended for providing a
  quick overview of the substrates contained in one to
  several plates if \code{full} is \code{TRUE}. If
  \code{full} is \code{FALSE}, it can be used to study the
  effect of the well-index translation and well-name
  normalisation approaches as used by \pkg{opm},
  particularly by the sub-creation methods (see
  \code{\link{[}}).
}
\examples{
## wells() 'OPM' method
(x <- wells(vaas_1, full = FALSE))[1:10]
(y <- wells(vaas_1, full = TRUE))[1:10]
(z <- wells(vaas_1, full = TRUE, in.parens = FALSE))[1:10]
# string lengths differ depending on selection
stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))

## wells() 'OPM' method
(xx <- wells(vaas_4, full = FALSE))[1:10]
# wells are guaranteed to be uniform within OPMS objects
stopifnot(identical(x, xx))

## wells() default method
x <- c("A01", "B10")
(y <- wells(x, plate = "PM1"))
stopifnot(nchar(y) > nchar(x))
(z <- wells(x, plate = "PM1", in.parens = TRUE))
stopifnot(nchar(z) > nchar(y))
# formula yields same result
stopifnot(y == wells(~ c(A01, B10), plate = "PM1"))
# querying for several plate types at once
(y <- wells(~ c(A01, B10), plate = c("PM2", "PM3", "PM10")))
stopifnot(dim(y) == c(2, 3))
(z <- listing(y)) # create a printable nested list
stopifnot(is.list(z), sapply(z, is.list), names(z) == colnames(y))
# using a sequence of well coordinates
stopifnot(nrow(wells(~ C02:C06)) == 5) # well sequence
stopifnot(nrow(wells(plate = "PM1")) == 96) # all wells by default

## listing() 'OPMD' method

# this yields one sentence for each kind of reaction:
(x <- listing(vaas_1, NULL))
stopifnot(inherits(x, "OPMD_Listing"), is.character(x), length(x) == 3,
  !is.null(names(x)))

# create an 'OPMS_Listing' object
(y <- listing(vaas_1, ~ Species + Strain))
stopifnot(inherits(y, "OPMS_Listing"), is.matrix(y), dim(y) == c(1, 3),
  y == x, colnames(y) == names(x), !is.null(rownames(y)))

# including HTML tags
(y <- listing(vaas_1, NULL, html = TRUE))
stopifnot(inherits(y, "OPMD_Listing"), is.character(x), nchar(y) > nchar(x),
  !is.null(names(x)))

## listing() 'OPMS' method

# no grouping, no names (numbering used instead for row names)
(x <- listing(vaas_4[1:2], as.groups = NULL))
stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
(y <- listing(vaas_4[1:2], as.groups = FALSE)) # alternative
stopifnot(identical(x, y))

# in effect no grouping, but names
(x <- listing(vaas_4[1:2], as.groups = list("Species", "Strain")))
stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))

# only single group for all plates
(y <- listing(vaas_4[1:2], as.groups = TRUE))
stopifnot(inherits(y, "OPMS_Listing"), is.matrix(y), dim(y) == c(1, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))

# two groups
(x <- listing(vaas_4, as.groups = list("Species")))
stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
}
\seealso{
  base::strtrim base::abbreviate

  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}}, \code{\link{gen_iii}},
  \code{\link{opm_files}}, \code{\link{param_names}},
  \code{\link{plate_type}}, \code{\link{register_plate}},
  \code{\link{select_colors}}, \code{\link{substrate_info}}
}
\keyword{attribute}
\keyword{category}
\keyword{character}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{listing-methods}
\alias{listing,OPMD-method}
\alias{listing,XOPMX-method}
\alias{listing,well_coords_map-method}
\alias{wells-methods}
\alias{wells,ANY-method}
\alias{wells,MOPMX-method}
\alias{wells,OPM-method}
\alias{wells,OPMS-method}
\alias{wells,missing-method}
\usage{
  \S4method{listing}{OPMD}(x, as.groups,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) 
  \S4method{listing}{XOPMX}(x, as.groups, cutoff = opm_opt("min.mode"),
    downcase = TRUE, full = TRUE, in.parens = FALSE, html = FALSE, sep = " ",
    ..., exact = TRUE, strict = TRUE) 
  \S4method{listing}{well_coords_map}(x) 

  \S4method{wells}{ANY}(object, full = TRUE, in.parens = FALSE,
    max = opm_opt("max.chars"), brackets = FALSE, clean = TRUE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    plate = "PM01", simplify = FALSE) 
  \S4method{wells}{MOPMX}(object, ...) 
  \S4method{wells}{OPM}(object, full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), brackets = FALSE, clean = TRUE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    plate = plate_type(object), simplify = TRUE) 
  \S4method{wells}{OPMS}(object, ...) 
  \S4method{wells}{missing}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
