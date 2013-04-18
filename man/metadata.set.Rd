\name{metadata.set}
\alias{metadata.set}
\alias{metadata<-}
\title{Replace metadata}
\arguments{
  \item{object}{\code{\link{WMD}} or \code{\link{OPMS}}
  object.}

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
  \code{\link{WMD}} or \code{\link{OPMS}} object.
  \itemize{ \item If \code{key} is a character vector, this
  can be arbitrary value(s) to be included in the metadata
  (if \code{NULL}, this metadata entry is deleted).  \item
  If \code{key} is otherwise, \code{value} must be list of
  values to be prepended, appended or set as metadata,
  either entirely or specifically, depending on \code{key}.
  \item Formulas can also be used as \code{value}. In that
  case, the formula can specify the key to be replaced. See
  the examples below and \code{\link{map_values}} for
  details.  \item If \code{object} is of class
  \sQuote{OPMS}, \code{value} can be a data frame whose
  number of rows must be equal to the number of plates.
  Metadata to be set will then be selected from each
  individual row in turn and in input order. This works
  analogously if \code{value} is an \code{OPMS} object. The
  lengths of both objects must match. If \code{value} is a
  \code{WMD} object, its metadata entries will be recycled.
  \item If \code{object} is of class \sQuote{WMD},
  \code{value} cannot be of class \sQuote{OPMS}.  }}
}
\value{
  \code{value}.
}
\description{
  Set the meta-information stored together with the data.
  The \code{\link{OPMS}} methods set the meta-information
  stored together with the measurements for all plates at
  once (but can address the plates individually if
  \code{value} is a formula or a data frame, see below).
}
\examples{
############################################################
#
# WMD methods
data(vaas_1)

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

############################################################
#
# OPMS methods
data(vaas_4)

# OPMS/missing/list method
copy <- vaas_4
(metadata(copy) <- list(x = -99)) # will replace all of them
stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))
metadata(copy[2]) <- list(x = 1) # will replace those of 2nd plate
stopifnot(identical(unique(metadata(copy)),
  list(list(x = -99), list(x = 1))))

# OPMS/missing/WMD method
(metadata(copy) <- vaas_1) # will also replace all of them
stopifnot(identical(unique(metadata(copy)), list(metadata(vaas_1))))

# OPMS/missing/formula method
copy <- vaas_4
metadata(copy) <- Organism ~ paste(Species, Strain)
(x <- metadata(copy, "Organism"))
stopifnot(length(x) == length(metadata(vaas_4, "Organism")) + 4)

# OPMS/ANY/ANY method
copy <- vaas_4
(metadata(copy, "Species") <- "Bacillus subtilis") # will set all of them
stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))
stopifnot(!identical(metadata(copy), metadata(vaas_4)))
metadata(copy) <- vaas_4 # reset
metadata(copy)
stopifnot(identical(metadata(copy), metadata(vaas_4)))
(metadata(copy) <- vaas_1) # set everything to metadata of vaas_1
stopifnot(identical(unique(metadata(copy)), list(metadata(vaas_1))))

# OPMS/character/data frame method
copy <- vaas_4
(x <- data.frame(Type = grepl("T$", metadata(vaas_4, "Strain"))))
metadata(copy, "Type") <- x
# one-column data frames are simplified
stopifnot(identical(metadata(copy, "Type"), x$Type))
# if keys match, a subselection of the data frame is used
(x <- cbind(x, Notype = !x$Type))
metadata(copy, "Type") <- x
stopifnot(identical(metadata(copy, "Type"), x$Type))
# if keys do not match, the entire data-frame rows are included
metadata(copy, "Type2") <- x
stopifnot(!identical(metadata(copy, "Type2"), x$Type))
}
\seealso{
  Other metadata-functions: \code{\link{include_metadata}},
  \code{\link{map_metadata}}, \code{\link{metadata_chars}}
}
\keyword{manip}

\docType{methods}
\alias{metadata.set-methods}
\alias{metadata.set,WMD,missing,list-method}
\alias{metadata<-,WMD,missing,list-method}
\alias{metadata.set,WMD,missing,FOE-method}
\alias{metadata<-,WMD,missing,FOE-method}
\alias{metadata.set,WMD,numeric,list-method}
\alias{metadata<-,WMD,numeric,list-method}
\alias{metadata.set,WMD,list,list-method}
\alias{metadata<-,WMD,list,list-method}
\alias{metadata.set,WMD,character,ANY-method}
\alias{metadata<-,WMD,character,ANY-method}
\alias{metadata.set,WMD,factor,ANY-method}
\alias{metadata<-,WMD,factor,ANY-method}
\alias{metadata.set,WMD,missing,WMD-method}
\alias{metadata<-,WMD,missing,WMD-method}
\alias{metadata.set,WMD,ANY,WMD-method}
\alias{metadata<-,WMD,ANY,WMD-method}
\alias{metadata.set,WMD,ANY,OPMS-method}
\alias{metadata<-,WMD,ANY,OPMS-method}
\alias{metadata.set,OPMS,missing,WMD-method}
\alias{metadata<-,OPMS,missing,WMD-method}
\alias{metadata.set,OPMS,missing,list-method}
\alias{metadata<-,OPMS,missing,list-method}
\alias{metadata.set,OPMS,missing,FOE-method}
\alias{metadata<-,OPMS,missing,FOE-method}
\alias{metadata.set,OPMS,missing,data.frame-method}
\alias{metadata<-,OPMS,missing,data.frame-method}
\alias{metadata.set,OPMS,missing,OPMS-method}
\alias{metadata<-,OPMS,missing,OPMS-method}
\alias{metadata.set,OPMS,ANY,WMD-method}
\alias{metadata<-,OPMS,ANY,WMD-method}
\alias{metadata.set,OPMS,ANY,OPMS-method}
\alias{metadata<-,OPMS,ANY,OPMS-method}
\alias{metadata.set,OPMS,ANY,ANY-method}
\alias{metadata<-,OPMS,ANY,ANY-method}
\alias{metadata.set,OPMS,ANY,data.frame-method}
\alias{metadata<-,OPMS,ANY,data.frame-method}
\alias{metadata.set,OPMS,character,OPMS-method}
\alias{metadata<-,OPMS,character,OPMS-method}
\alias{metadata.set,OPMS,character,data.frame-method}
\alias{metadata<-,OPMS,character,data.frame-method}
\alias{metadata.set,OPMS,factor,ANY-method}
\alias{metadata<-,OPMS,factor,ANY-method}
\usage{
  \S4method{metadata}{WMD,missing,list}(object, key) <- value

  \S4method{metadata}{WMD,missing,FOE}(object, key) <- value

  \S4method{metadata}{WMD,numeric,list}(object, key) <- value

  \S4method{metadata}{WMD,list,list}(object, key) <- value

  \S4method{metadata}{WMD,character,ANY}(object, key) <- value

  \S4method{metadata}{WMD,factor,ANY}(object, key) <- value

  \S4method{metadata}{WMD,missing,WMD}(object, key) <- value

  \S4method{metadata}{WMD,ANY,WMD}(object, key) <- value

  \S4method{metadata}{WMD,ANY,OPMS}(object, key) <- value

  \S4method{metadata}{OPMS,missing,WMD}(object, key) <- value

  \S4method{metadata}{OPMS,missing,list}(object, key) <- value

  \S4method{metadata}{OPMS,missing,FOE}(object, key) <- value

  \S4method{metadata}{OPMS,missing,data.frame}(object,
    key) <- value

  \S4method{metadata}{OPMS,missing,OPMS}(object, key) <- value

  \S4method{metadata}{OPMS,ANY,WMD}(object, key) <- value

  \S4method{metadata}{OPMS,ANY,OPMS}(object, key) <- value

  \S4method{metadata}{OPMS,ANY,ANY}(object, key) <- value

  \S4method{metadata}{OPMS,ANY,data.frame}(object, key) <- value

  \S4method{metadata}{OPMS,character,OPMS}(
    object, key) <- value

  \S4method{metadata}{OPMS,character,data.frame}(
    object, key) <- value

  \S4method{metadata}{OPMS,factor,ANY}(
    object, key) <- value

}
