\name{metadata.set}
\alias{metadata.set}
\alias{metadata<-}
\title{Replace metadata}
\arguments{
  \item{object}{\code{\link{WMD}} or \code{\link{OPMS}}
  object..}

  \item{key}{Missing, numeric scalar, character vector,
  factor, or list. If missing, replace all metadata by
  \code{value} (unless \code{value} is a formula that
  specifies the key to replace). If a numeric scalar, then
  if positive, prepend \code{value} to old metadata. If
  negative, append \code{value} to old metadata. If zero,
  replace old metadata entirely by \code{value}. If a list,
  treat it as list of keys; expect \code{value} to be a
  list of corresponding metadata values to be set. Names
  are replaced by the values of either list if they are
  missing. If a character vector, use it as key and
  set/replace this metadata entry to/by \code{value}. It is
  an error if \code{key} has zero length. If it contains
  more than one entry, a nested query is done. See
  \code{[[} from the \pkg{base} package for details. The
  factor method calls the character method after converting
  \code{key} to mode \sQuote{character}.}

  \item{value}{If \code{key} is a character vector, this
  can be arbitrary value(s) to be included in the metadata
  (if \code{NULL}, this metadata entry is deleted). If
  \code{key} is otherwise, \code{value} must be list of
  values to be prepended, appended or set as metadata,
  either entirely or specifically, depending on \code{key}.
  Formulas can also be used as \code{value}. In that case,
  the formula can specify the key to be replaced.  See the
  examples below and \code{\link{map_values}} for details.
  If \code{object} is of class \sQuote{OPMS}, \code{value}
  can be a data frame whose number of rows must be equal to
  the number of plates. Metadata to be set will then be
  selected from each individual row in turn and in input
  order.}
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
(metadata(copy) <- list(x = -99))
stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))

# OPMS/missing/formula method
copy <- vaas_4
metadata(copy) <- Organism ~ paste(Species, Strain)
(x <- metadata(copy, "Organism"))
stopifnot(length(x) == length(metadata(vaas_4, "Organism")) + 4)

# OPMS/ANY/ANY method
copy <- vaas_4
(metadata(copy, "Species") <- "Bacillus subtilis")
stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))

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
\alias{metadata.set,WMD,missing,formula-method}
\alias{metadata<-,WMD,missing,formula-method}
\alias{metadata.set,WMD,numeric,list-method}
\alias{metadata<-,WMD,numeric,list-method}
\alias{metadata.set,WMD,list,list-method}
\alias{metadata<-,WMD,list,list-method}
\alias{metadata.set,WMD,character,ANY-method}
\alias{metadata<-,WMD,character,ANY-method}
\alias{metadata.set,WMD,factor,ANY-method}
\alias{metadata<-,WMD,factor,ANY-method}
\alias{metadata.set,OPMS,missing,list-method}
\alias{metadata<-,OPMS,missing,list-method}
\alias{metadata.set,OPMS,missing,formula-method}
\alias{metadata<-,OPMS,missing,formula-method}
\alias{metadata.set,OPMS,missing,data.frame-method}
\alias{metadata<-,OPMS,missing,data.frame-method}
\alias{metadata.set,OPMS,ANY,ANY-method}
\alias{metadata<-,OPMS,ANY,ANY-method}
\alias{metadata.set,OPMS,ANY,data.frame-method}
\alias{metadata<-,OPMS,ANY,data.frame-method}
\alias{metadata.set,OPMS,character,data.frame-method}
\alias{metadata<-,OPMS,character,data.frame-method}
\alias{metadata.set,OPMS,factor,data.frame-method}
\alias{metadata<-,OPMS,factor,data.frame-method}
\usage{
  \S4method{metadata}{WMD,missing,list}(object) <- value

  \S4method{metadata}{WMD,missing,formula}(object) <- value

  \S4method{metadata}{WMD,numeric,list}(object, key) <- value

  \S4method{metadata}{WMD,list,list}(object, key) <- value

  \S4method{metadata}{WMD,character,ANY}(object, key) <- value

  \S4method{metadata}{WMD,factor,ANY}(object, key) <- value

  \S4method{metadata}{OPMS,missing,list}(object) <- value

  \S4method{metadata}{OPMS,missing,formula}(object) <- value

  \S4method{metadata}{OPMS,missing,data.frame}(object) <- value

  \S4method{metadata}{OPMS,ANY,ANY}(object, key) <- value

  \S4method{metadata}{OPMS,ANY,data.frame}(object, key) <- value

  \S4method{metadata}{OPMS,character,data.frame}(
    object, key) <- value

  \S4method{metadata}{OPMS,factor,data.frame}(
    object, key) <- value

}
