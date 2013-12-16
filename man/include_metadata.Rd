\name{include_metadata}
\alias{edit}
\alias{include_metadata}
\alias{map_metadata}
\title{Add or map metadata or edit them by hand}
\arguments{
  \item{object}{\code{\link{OPM}} (\code{\link{WMD}}),
  \code{\link{OPMS}} or \code{\link{MOPMX}} object.}

  \item{name}{Like \code{object}, but for the \code{edit}
  method.}

  \item{md}{Data frame containing keys as column names, or
  name of file from which to read the data frame. Handled
  by \code{\link{to_metadata}}.}

  \item{keys}{Character vector.}

  \item{replace}{Logical scalar indicating whether the
  previous metadata, if any, shall be replaced by the novel
  ones, or whether these shall be appended.}

  \item{skip.failure}{Logical scalar. Do not stop with an
  error message if (unambiguous) selection is impossible
  but raise a warning only?}

  \item{remove.keys}{Logical scalar. When including
  \code{md} in the metadata, discard the \code{keys}
  columns?}

  \item{mapping}{In most cases passed to
  \code{\link{map_values}}. \itemize{ \item If a function,
  this is just a wrapper for \code{rapply}, with \code{how}
  set to \sQuote{replace}, if \code{values} is \code{TRUE}.
  It is applied to all non-list elements of
  \code{\link{metadata}}, which is traversed recursively.
  \item Alternatively, a character vector.
  \code{\link{metadata_chars}} can be used to create a
  template for such a vector.  \item \code{mapping} can
  also be a formula; in that case, \code{\link{metadata}}
  is replaced by the according method of
  \code{\link{map_values}}. If the left side of the formula
  is missing, the entire metadata are replaced by the
  result, which is an error if the result is not a list.
  \item If \code{mapping} is missing, the behaviour is
  special; see the next two arguments. }}

  \item{values}{Mostly a logical scalar. \itemize{ \item
  For the function and character-vector methods, if
  \code{FALSE}, metadata names, not values, are mapped, and
  \code{classes} is ignored (names are always of class
  \sQuote{character}).  \item For the formula method,
  \code{values} is the enclosing environment used.  \item
  If \code{mapping} is missing, setting \code{values} to
  \code{TRUE} causes all non-list entries that only
  comprise \code{NA} values to be removed.  }}

  \item{classes}{Character vector or (for the character
  vector-based mapping) \code{TRUE}. For the mapping with a
  function or vector, this specifies the classes in
  addition to \sQuote{character} that are mapped (after
  converting to \sQuote{character} mode).

  If \code{classes} is \code{TRUE}, \code{mapping} is
  treated as a mapping between class names, and the
  according conversions are applied. See the \code{coerce}
  argument of \code{\link{map_values}} for details.

  If \code{mapping} is missing, \code{classes} specifies
  classes that are converted to character vectors.}

  \item{...}{Optional arguments passed to \code{mapping} if
  it is a function, and from the \code{\link{OPMS}} method
  to the \code{\link{WMD}} method, or from
  \code{include_metadata} to \code{\link{to_metadata}}, or
  as additional arguments to \code{edit} from the
  \pkg{utils} package.}
}
\value{
  Novel \code{\link{WMD}} or \code{\link{OPMS}} object with
  modified metadata.
}
\description{
  Either include metadata by mapping \acronym{CSV} data and
  column names in a data frame (optionally read from file),
  or modify meta-information stored together with the
  measurements by using a function or other kinds of
  mappings and return the objects otherwise unchanged, or
  invoke \code{edit} from the \pkg{utils} package for
  editing the metadata by hand.
}
\details{
  The \code{\link{OPMS}} method applies the inclusion and
  mapping routines to all plates in turn and returns an
  \code{\link{OPMS}} object with accordingly modified
  metadata.

  Calling \code{edit} will only work if
  \code{\link{to_metadata}} yields a data frame suitable
  for the \code{edit} method from the \pkg{utils} package.
  This usually means that the \code{\link{metadata}} must
  be rectangular, even though this is not enforced by the
  implementation of the \code{\link{OPMX}} classes. Entries
  missing in some elements of \code{name} should not
  present a problem, however. Values that remained
  \code{NA} would be removed before returning the result.
}
\examples{
## include_metadata()

(x <- collect_template(vaas_1, add.cols = "Location")) # generate data frame
x[1, "Location"] <- "Braunschweig" # insert additional information
copy <- include_metadata(vaas_1, x) # include the data in new OPM object
stopifnot(is.null(metadata(vaas_1, "Location")))
stopifnot(identical(metadata(copy, "Location"), "Braunschweig"))

## map_metadata()

# WMD methods

# WMD+function method
copy <- map_metadata(vaas_1, identity)
stopifnot(identical(copy, vaas_1))
copy <- map_metadata(vaas_1, identity, values = FALSE)
stopifnot(identical(copy, vaas_1))
copy <- map_metadata(vaas_1, function(x) paste(x, "!"), values = FALSE)
(x <- metadata_chars(vaas_1, values = FALSE))
(y <- metadata_chars(copy, values = FALSE))
stopifnot(identical(as.character(y), paste(x, "!")))

# WMD+character method: mapping a value
map <- metadata_chars(vaas_1)
map["First replicate"] <- "Rep. 1"
copy <- map_metadata(vaas_1, map)
stopifnot(identical(names(metadata(copy)), names(metadata(vaas_1))))
stopifnot(!identical(metadata(copy, "Experiment"),
  metadata(vaas_1, "Experiment")))

# WMD+character method: mapping a name
map <- metadata_chars(vaas_1, values = FALSE)
map["Plate number"] <- "Plate no."
copy <- map_metadata(vaas_1, map, values = FALSE)
stopifnot(!identical(names(metadata(copy)), names(metadata(vaas_1))))

# WMD+formula method
copy <- map_metadata(vaas_1, Organism ~ paste(Species, Strain))
(x <- setdiff(metadata_chars(copy), metadata_chars(vaas_1)))
stopifnot(length(x) == 1, x == "Escherichia coli DSM30083T")
stopifnot(identical(copy, # same result with expression
  map_metadata(vaas_1, expression(Organism <- paste(Species, Strain)))))

# WMD+missing method
(x <- metadata(map_metadata(vaas_1)))
stopifnot(identical(x, metadata(vaas_1))) # nothing to modify in that case

# OPMS method

# using a function
copy <- map_metadata(vaas_4, identity)
stopifnot(identical(copy, vaas_4))
copy <- map_metadata(vaas_4, identity, values = FALSE)
stopifnot(identical(copy, vaas_4))
copy <- map_metadata(vaas_4, function(x) paste(x, "!"), values = FALSE)
(x <- metadata_chars(vaas_4, values = FALSE))
(y <- metadata_chars(copy, values = FALSE))
stopifnot(identical(as.character(y), paste(x, "!")))

# using a character vector
map <- metadata_chars(vaas_4)
map["First replicate"] <- "Rep. 1"
copy <- map_metadata(vaas_4, map)
x <- metadata(vaas_4, "Experiment")
stopifnot(x == "First replicate")
y <- metadata(copy, "Experiment")
stopifnot(y == "Rep. 1")

# using a formula
copy <- map_metadata(vaas_4, Organism ~ paste(Species, Strain))
(x <- setdiff(metadata_chars(copy), metadata_chars(vaas_4)))
stopifnot(length(x) == 4) # one entry per plate

# 'mapping' missing
(x <- metadata(map_metadata(vaas_4)))
stopifnot(identical(x, metadata(vaas_4))) # nothing to modify in that case

\dontrun{ ## edit metadata by hand
  x <- edit(vaas_4) # this would create a new object
  x <- edit(x) # overwrite x in 2nd editing step
  ## This will not necessarily work if the metadata are nested!
}
}
\seealso{
  utils::edit

  Other metadata-functions: \code{\link{metadata}},
                                    \code{\link{metadata_chars}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{edit-methods}
\alias{edit,OPMX-method}
\alias{include_metadata-methods}
\alias{include_metadata,MOPMX-method}
\alias{include_metadata,OPM-method}
\alias{include_metadata,OPMS-method}
\alias{map_metadata-methods}
\alias{map_metadata,MOPMX,ANY-method}
\alias{map_metadata,MOPMX,missing-method}
\alias{map_metadata,OPMS,ANY-method}
\alias{map_metadata,OPMS,missing-method}
\alias{map_metadata,WMD,FOE-method}
\alias{map_metadata,WMD,character-method}
\alias{map_metadata,WMD,function-method}
\alias{map_metadata,WMD,missing-method}
\usage{
  \S4method{edit}{OPMX}(name, ...) 

  \S4method{include_metadata}{MOPMX}(object, ...) 
  \S4method{include_metadata}{OPM}(object, md,
    keys = opm_opt("csv.keys"), replace = FALSE, skip.failure = FALSE,
    remove.keys = TRUE, ...) 
  \S4method{include_metadata}{OPMS}(object, ...) 

  \S4method{map_metadata}{MOPMX,ANY}(object, mapping, ...) 
  \S4method{map_metadata}{MOPMX,missing}(object, mapping,
    values = TRUE, classes = "factor") 
  \S4method{map_metadata}{OPMS,ANY}(object, mapping, ...) 
  \S4method{map_metadata}{OPMS,missing}(object, mapping,
    values = TRUE, classes = "factor") 
  \S4method{map_metadata}{WMD,FOE}(object, mapping,
    values = parent.frame(), classes = NULL) 
  \S4method{map_metadata}{WMD,character}(object, mapping,
    values = TRUE, classes = "factor") 
  \S4method{map_metadata}{WMD,function}(object, mapping,
    values = TRUE, classes = "ANY", ...) 
  \S4method{map_metadata}{WMD,missing}(object, mapping,
    values = TRUE, classes = "factor") 
}
%% END INSERTION BY repair_S4_docu.rb
