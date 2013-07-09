\name{map_metadata}
\alias{map_metadata}
\title{Map metadata}
\arguments{
  \item{object}{\code{\link{WMD}} object or
  \code{\link{OPMS}} object.}

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
  result, which is an error if the result is not a list. }}

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

  \item{...}{Optional argument passed to \code{mapping} if
  it is a function, and from the \code{\link{OPMS}} method
  to the \code{\link{WMD}} method.}
}
\value{
  \code{\link{WMD}} or \code{\link{OPMS}} object with
  modified metadata.
}
\description{
  Modify meta-information stored together with the
  measurements by using a function or other kinds of
  mappings and return the objects otherwise unchanged. The
  \code{\link{OPMS}} method applies this to all plates in
  turn and returns an \code{\link{OPMS}} object with
  accordingly modified metadata.
}
\examples{
# WMD methods
data(vaas_1)

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
data(vaas_4)

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
}
\seealso{
  Other metadata-functions: \code{\link{include_metadata}},
  \code{\link{metadata.set}},                               \code{\link{metadata_chars}}
}
\keyword{manip}

\docType{methods}
\alias{map_metadata-methods}
\alias{map_metadata,WMD,function-method}
\alias{map_metadata,WMD,character-method}
\alias{map_metadata,WMD,FOE-method}
\alias{map_metadata,WMD,missing-method}
\alias{map_metadata,OPMS,missing-method}
\alias{map_metadata,OPMS,ANY-method}
\usage{
  \S4method{map_metadata}{WMD,function}(object, mapping,
    values = TRUE, classes = "ANY", ...) 

  \S4method{map_metadata}{WMD,character}(object, mapping,
    values = TRUE, classes = "factor") 

  \S4method{map_metadata}{WMD,FOE}(object, mapping,
    values = parent.frame()) 

  \S4method{map_metadata}{WMD,missing}(object, mapping,
    values = TRUE, classes = "factor") 

  \S4method{map_metadata}{OPMS,missing}(object, mapping,
    values = TRUE, classes = "factor") 

  \S4method{map_metadata}{OPMS,ANY}(object, mapping, ...) 

}
