\name{metadata.set}
\alias{metadata.set}
\alias{metadata<-}
\title{Replace metadata}
\arguments{
  \item{object}{\code{\link{WMD}}, \code{\link{WMDS}} or
  \code{\link{MOPMX}} object.}

  \item{key}{Missing, numeric scalar, character vector,
  factor, or list. \itemize{ \item If missing, replace all
  metadata by \code{value} (unless \code{value} is a
  formula that specifies the key to replace).  \item If a
  numeric scalar, then if positive, prepend \code{value} to
  old metadata. If negative, append \code{value} to old
  metadata. If zero, replace old metadata entirely by
  \code{value}.  \item If a list, treated as list of keys;
  expect \code{value} to be a list of corresponding
  metadata values to be set. Names are replaced by the
  values of either list if they are missing.  \item If a
  character vector, used as key for setting/replacing this
  metadata entry to/by \code{value}. It is an error if
  \code{key} has zero length. If it contains more than one
  entry, a nested query is done. See \code{[[} from the
  \pkg{base} package for details.  \item The factor method
  calls the character method after converting \code{key} to
  mode \sQuote{character}. }}

  \item{value}{Character vector, list, data frame, formula,
  \code{\link{WMD}} or \code{\link{WMDS}} object.
  \itemize{ \item If \code{key} is a character vector, this
  can be arbitrary value(s) to be included in the metadata
  (if \code{NULL}, this metadata entry is deleted).  \item
  If \code{key} is otherwise, \code{value} must be list of
  values to be prepended, appended or set as metadata,
  either entirely or specifically, depending on \code{key}.
  \item Formulae can also be used as \code{value}. In that
  case, the formula can specify the key to be replaced. See
  the examples below and \code{\link{map_values}} for
  details.  \item If \code{object} is of class
  \code{\link{WMDS}}, \code{value} can be a data frame
  whose number of rows must be equal to the number of
  plates.  Metadata to be set will then be selected from
  each individual row in turn and in input order. This
  works analogously if \code{value} is an
  \code{\link{WMDS}} object. The lengths of both objects
  must match. If \code{value} is a \code{\link{WMD}}
  object, its metadata entries will be recycled. \item If
  \code{object} is of class \code{\link{WMD}}, \code{value}
  cannot be of class \code{\link{WMD}}.  }}
}
\value{
  \code{value}.
}
\description{
  Set the meta-information stored together with the data.
  For most kinds of arguments the \code{\link{WMDS}} and
  \code{\link{MOPMX}} methods set the meta-information
  stored together with the measurements for all plates at
  once. But they can address the plates individually if
  \code{value} is a data frame, and they can address
  metadata keys individually if \code{value} is a formula.
}
\details{
  This method can easily be used to copy (selected parts
  of) the \code{\link{csv_data}} to the metadata; see there
  for details.

  \code{\link{map_metadata}} can also be used to modify
  metadata but it will return a novel object. See
  \code{\link{edit}} for manually modifying metadata.
}
\examples{
## WMD methods

# WMD/missing/list method
copy <- vaas_1
new.md <- list(Species = "Thermomicrobium roseum")
metadata(copy) <- new.md
stopifnot(identical(metadata(copy), new.md))

# WMD/missing/formula method (operates on previous entries!)
copy <- vaas_1
metadata(copy) <- Organism ~ paste(Species, Strain)
(x <- metadata(copy, "Organism"))
stopifnot(is.null(metadata(vaas_1, "Organism")), !is.null(x))

# WMD/numeric/list method
copy <- vaas_1
metadata(copy, 1) <- list(Authors = "Vaas et al.")
stopifnot(length(metadata(copy)) > length(metadata(vaas_1)))

# WMD/list/list method
copy <- vaas_1
stopifnot(identical(metadata(copy, "Species"), "Escherichia coli"))

# You can use this to translate the keys on-the-fly...
metadata(copy, list(Organism = "Species")) <- list(
  Organism = "Bacillus subtilis")
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(identical(metadata(copy, "Species"), "Bacillus subtilis"))
stopifnot(is.null(metadata(copy, "Organism"))) # this was not set!

# ...but you need not
metadata(copy, list("Species")) <- list(Species = "Yersinia pestis")
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(identical(metadata(copy, "Species"), "Yersinia pestis"))

# Names need not be duplicated
metadata(copy, list("Species")) <- list("Gen. sp.")
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(identical(metadata(copy, "Species"), "Gen. sp."))

# ...but this would delete the entry because nothing would be found in
# 'value'
metadata(copy, list("Species")) <- list(Organism = "E. coli")
stopifnot(length(metadata(copy)) < length(metadata(vaas_1)))
stopifnot(is.null(metadata(copy, "Species")))

# ...this yields a general mechanism for metadata deletion by providing an
# empty list as 'value'.

# WMD/character/any method
copy <- vaas_1
metadata(copy, "Strain") <- "08/15"
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(metadata(copy, "Strain") != metadata(vaas_1, "Strain"))

# WMD/factor/any method
metadata(copy, as.factor("Strain")) <- metadata(vaas_1, "Strain")
stopifnot(metadata(copy, "Strain") == metadata(vaas_1, "Strain"))

## WMDS methods

# WMDS/missing/list method
copy <- vaas_4
(metadata(copy) <- list(x = -99)) # will replace all of them
stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))
metadata(copy[2]) <- list(x = 1) # will replace those of 2nd plate
stopifnot(identical(unique(metadata(copy)),
  list(list(x = -99), list(x = 1))))

# WMDS/missing/WMD method
(metadata(copy) <- vaas_1) # will also replace all of them
stopifnot(identical(unique(metadata(copy)), list(metadata(vaas_1))))

# WMDS/missing/formula method
copy <- vaas_4
metadata(copy) <- Organism ~ paste(Species, Strain)
(x <- metadata(copy, "Organism"))
stopifnot(length(x) == length(metadata(vaas_4, "Organism")) + 4)

# WMDS/ANY/ANY method
copy <- vaas_4
(metadata(copy, "Species") <- "Bacillus subtilis") # will set all of them
stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))
stopifnot(!identical(metadata(copy), metadata(vaas_4)))
metadata(copy) <- vaas_4 # reset
metadata(copy)
stopifnot(identical(metadata(copy), metadata(vaas_4)))
(metadata(copy) <- vaas_1) # set everything to metadata of vaas_1
stopifnot(identical(unique(metadata(copy)), list(metadata(vaas_1))))

# WMDS/character/data frame method
copy <- vaas_4
(x <- data.frame(Type = grepl("T$", metadata(vaas_4, "Strain"))))
metadata(copy, "Type") <- x
# one-column data frames are simplified
stopifnot(identical(metadata(copy, "Type"), x$Type))
# if keys match, a partial selection of the data frame is used
(x <- cbind(x, Notype = !x$Type))
metadata(copy, "Type") <- x
stopifnot(identical(metadata(copy, "Type"), x$Type))
# if keys do not match, the entire data-frame rows are included
metadata(copy, "Type2") <- x
stopifnot(!identical(metadata(copy, "Type2"), x$Type))
}
\seealso{
  Other metadata-functions: \code{\link{edit}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}}, \code{\link{map_values}},
  \code{\link{metadata}}, \code{\link{metadata_chars}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{metadata.set-methods}
\alias{metadata.set,MOPMX,ANY,ANY-method}
\alias{metadata.set,MOPMX-method}
\alias{metadata<-,MOPMX,ANY,ANY-method}
\alias{metadata<-,MOPMX-method}
\alias{metadata.set,MOPMX,ANY,data.frame-method}
\alias{metadata<-,MOPMX,ANY,data.frame-method}
\alias{metadata.set,MOPMX,missing,ANY-method}
\alias{metadata.set,MOPMX,missing-method}
\alias{metadata<-,MOPMX,missing,ANY-method}
\alias{metadata<-,MOPMX,missing-method}
\alias{metadata.set,MOPMX,missing,data.frame-method}
\alias{metadata<-,MOPMX,missing,data.frame-method}
\alias{metadata.set,WMD,ANY,ANY-method}
\alias{metadata.set,WMD-method}
\alias{metadata<-,WMD,ANY,ANY-method}
\alias{metadata<-,WMD-method}
\alias{metadata.set,WMD,character,ANY-method}
\alias{metadata.set,WMD,character-method}
\alias{metadata<-,WMD,character,ANY-method}
\alias{metadata<-,WMD,character-method}
\alias{metadata.set,WMD,character,WMD-method}
\alias{metadata<-,WMD,character,WMD-method}
\alias{metadata.set,WMD,character,WMDS-method}
\alias{metadata<-,WMD,character,WMDS-method}
\alias{metadata.set,WMD,character,data.frame-method}
\alias{metadata<-,WMD,character,data.frame-method}
\alias{metadata.set,WMD,list,WMD-method}
\alias{metadata<-,WMD,list,WMD-method}
\alias{metadata.set,WMD,list,WMDS-method}
\alias{metadata<-,WMD,list,WMDS-method}
\alias{metadata.set,WMD,list,data.frame-method}
\alias{metadata<-,WMD,list,data.frame-method}
\alias{metadata.set,WMD,list,list-method}
\alias{metadata<-,WMD,list,list-method}
\alias{metadata.set,WMD,missing,FOE-method}
\alias{metadata<-,WMD,missing,FOE-method}
\alias{metadata.set,WMD,missing,WMD-method}
\alias{metadata<-,WMD,missing,WMD-method}
\alias{metadata.set,WMD,missing,WMDS-method}
\alias{metadata<-,WMD,missing,WMDS-method}
\alias{metadata.set,WMD,missing,data.frame-method}
\alias{metadata<-,WMD,missing,data.frame-method}
\alias{metadata.set,WMD,missing,list-method}
\alias{metadata<-,WMD,missing,list-method}
\alias{metadata.set,WMD,numeric,WMD-method}
\alias{metadata<-,WMD,numeric,WMD-method}
\alias{metadata.set,WMD,numeric,WMDS-method}
\alias{metadata<-,WMD,numeric,WMDS-method}
\alias{metadata.set,WMD,numeric,data.frame-method}
\alias{metadata<-,WMD,numeric,data.frame-method}
\alias{metadata.set,WMD,numeric,list-method}
\alias{metadata<-,WMD,numeric,list-method}
\alias{metadata.set,WMDS,ANY,ANY-method}
\alias{metadata.set,WMDS-method}
\alias{metadata<-,WMDS,ANY,ANY-method}
\alias{metadata<-,WMDS-method}
\alias{metadata.set,WMDS,ANY,WMD-method}
\alias{metadata<-,WMDS,ANY,WMD-method}
\alias{metadata.set,WMDS,ANY,WMDS-method}
\alias{metadata<-,WMDS,ANY,WMDS-method}
\alias{metadata.set,WMDS,ANY,data.frame-method}
\alias{metadata<-,WMDS,ANY,data.frame-method}
\alias{metadata.set,WMDS,character,WMDS-method}
\alias{metadata<-,WMDS,character,WMDS-method}
\alias{metadata.set,WMDS,character,data.frame-method}
\alias{metadata<-,WMDS,character,data.frame-method}
\alias{metadata.set,WMDS,missing,FOE-method}
\alias{metadata<-,WMDS,missing,FOE-method}
\alias{metadata.set,WMDS,missing,WMD-method}
\alias{metadata<-,WMDS,missing,WMD-method}
\alias{metadata.set,WMDS,missing,WMDS-method}
\alias{metadata<-,WMDS,missing,WMDS-method}
\alias{metadata.set,WMDS,missing,data.frame-method}
\alias{metadata<-,WMDS,missing,data.frame-method}
\alias{metadata.set,WMDS,missing,list-method}
\alias{metadata<-,WMDS,missing,list-method}
\usage{
  \S4method{metadata}{MOPMX,ANY,ANY}(object, key) <- value
  \S4method{metadata}{MOPMX,ANY,data.frame}(object, key) <- value
  \S4method{metadata}{MOPMX,missing,ANY}(object, key) <- value
  \S4method{metadata}{MOPMX,missing,data.frame}(object, key) <- value
  \S4method{metadata}{WMD,ANY,ANY}(object, key) <- value
  \S4method{metadata}{WMD,character,ANY}(object, key) <- value
  \S4method{metadata}{WMD,character,WMD}(object, key) <- value
  \S4method{metadata}{WMD,character,WMDS}(object, key) <- value
  \S4method{metadata}{WMD,character,data.frame}(object, key) <- value
  \S4method{metadata}{WMD,list,WMD}(object, key) <- value
  \S4method{metadata}{WMD,list,WMDS}(object, key) <- value
  \S4method{metadata}{WMD,list,data.frame}(object, key) <- value
  \S4method{metadata}{WMD,list,list}(object, key) <- value
  \S4method{metadata}{WMD,missing,FOE}(object, key) <- value
  \S4method{metadata}{WMD,missing,WMD}(object, key) <- value
  \S4method{metadata}{WMD,missing,WMDS}(object, key) <- value
  \S4method{metadata}{WMD,missing,data.frame}(object,
    key) <- value
  \S4method{metadata}{WMD,missing,list}(object, key) <- value
  \S4method{metadata}{WMD,numeric,WMD}(object, key) <- value
  \S4method{metadata}{WMD,numeric,WMDS}(object, key) <- value
  \S4method{metadata}{WMD,numeric,data.frame}(object, key) <- value
  \S4method{metadata}{WMD,numeric,list}(object, key) <- value
  \S4method{metadata}{WMDS,ANY,ANY}(object, key) <- value
  \S4method{metadata}{WMDS,ANY,WMD}(object, key) <- value
  \S4method{metadata}{WMDS,ANY,WMDS}(object, key) <- value
  \S4method{metadata}{WMDS,ANY,data.frame}(object, key) <- value
  \S4method{metadata}{WMDS,character,WMDS}(object, key) <- value
  \S4method{metadata}{WMDS,character,data.frame}(object,
    key) <- value
  \S4method{metadata}{WMDS,missing,FOE}(object, key) <- value
  \S4method{metadata}{WMDS,missing,WMD}(object, key) <- value
  \S4method{metadata}{WMDS,missing,WMDS}(object, key) <- value
  \S4method{metadata}{WMDS,missing,data.frame}(object,
    key) <- value
  \S4method{metadata}{WMDS,missing,list}(object, key) <- value
}
%% END INSERTION BY repair_S4_docu.rb
