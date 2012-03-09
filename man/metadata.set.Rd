\name{metadata.set}
\alias{metadata.set}
\alias{metadata<-}
\title{Replace metadata}
\arguments{
  \item{object}{\code{\link{WMD}} or \code{\link{OPMS}}
  object..}

  \item{key}{Missing, numeric scalar, character vector, or
  list.  If missing, replace all metadata by \code{value}.
  If a numeric scalar, then if positive, prepend
  \code{value} to old metadata. If negative, append
  \code{value} to old metadata. If zero, replace old
  metadata entirely by \code{value}.  If a list, treat it
  as list of keys; expect \code{value} to be a list of
  corresponding metadata values to be set. Names are
  replaced by the values of either list if they are
  missing.  If a character vector, use it as key and
  set/replace this metadata entry to/by \code{value}. It is
  an error if \code{key} has zero length.  If it contains
  more than one entry, a nested query is done. See
  \code{[[} from the \pkg{base} package for details.}

  \item{value}{If \code{key} is a character vector, this
  can be arbitrary value(s) to be included in the metadata
  (if \code{NULL}, this metadata entry is deleted). If
  \code{key} is otherwise, \code{value} must be list of
  values to be prepended, appended or set as metadata,
  either entirely or specifically, depending on
  \code{key}.}
}
\value{
  \code{value}.
}
\description{
  Set the meta-information stored together with the data.
  The \code{\link{OPMS}} methods set the meta-information
  stored together with the measurements for all plates at
  once.
}
\examples{
############################################################
#
# WMD methods
data(vaas_1)

# WMD+missing method
copy <- vaas_1
new.md <- list(Species = "Thermomicrobium roseum")
metadata(copy) <- new.md
stopifnot(identical(metadata(copy), new.md))

# WMD+numeric method
copy <- vaas_1
metadata(copy, 1) <- list(Authors = "Vaas et al.")
stopifnot(length(metadata(copy)) > length(metadata(vaas_1)))

# WMD+list method
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

# WMD+character method
copy <- vaas_1
metadata(copy, "Strain") <- "08/15"
stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
stopifnot(metadata(copy, "Strain") != metadata(vaas_1, "Strain"))

############################################################
#
# OPMS methods
data(vaas_4)

# OPMS+missing method
copy <- vaas_4
(metadata(copy) <- list(x = -99))
stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))

# OPMS+ANY method
copy <- vaas_4
(metadata(copy, "Species") <- "Bacillus subtilis")
stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))
}
\seealso{
  Other metadata-functions: \code{\link{collect_template}},
  \code{\link{extract_columns}},
  \code{\link{include_metadata}},
  \code{\link{map_metadata}}, \code{\link{metadata}},
  \code{\link{metadata_chars}}, \code{\link{to_metadata}}
}
\keyword{manip}

\docType{methods}
\alias{metadata.set-methods}
\alias{metadata.set,OPMS,missing,list-method}
\alias{metadata<-,OPMS,missing,list-method}
\alias{metadata.set,OPMS,ANY,ANY-method}
\alias{metadata<-,OPMS,ANY,ANY-method}
\alias{metadata.set,WMD,missing,list-method}
\alias{metadata<-,WMD,missing,list-method}
\alias{metadata.set,WMD,numeric,list-method}
\alias{metadata<-,WMD,numeric,list-method}
\alias{metadata.set,WMD,list,list-method}
\alias{metadata<-,WMD,list,list-method}
\alias{metadata.set,WMD,character,ANY-method}
\alias{metadata<-,WMD,character,ANY-method}
\usage{
  \S4method{metadata}{OPMS,missing,list}(object) <- value

  \S4method{metadata}{OPMS,ANY,ANY}(object, key) <- value

  \S4method{metadata}{WMD,missing,list}(object) <- value

  \S4method{metadata}{WMD,numeric,list}(object, key) <- value

  \S4method{metadata}{WMD,list,list}(object, key) <- value

  \S4method{metadata}{WMD,character,ANY}(object, key) <- value

}
