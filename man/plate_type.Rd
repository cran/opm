\name{plate_type}
\alias{gen_iii}
\alias{plate_type}
\alias{register_plate}
\title{Plate type displayed or modified, registered or deleted}
\arguments{
  \item{object}{\code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object, or character vector of
  original plate name(s), or factor, or logical scalar. If
  missing, the function displays the plate types \pkg{opm}
  knows about. If a logical scalar, the behaviour is the
  same, but optionally restricted to the user-defined or
  the \pkg{opm}-defined plate types.

  For \code{register_plate}, \code{object} is either
  missing (the easiest way to apply the function), causing
  the arguments within \code{...} (which should be named)
  to be used, if any, a named list of \code{NULL} values or
  character vectors, or a character vector whose elements
  are interpretable as file names. See below for details on
  the input format.}

  \item{full}{Logical scalar. If \code{TRUE}, add (or
  replace by) the full name of the plate type (if
  available); otherwise, return it as-is.}

  \item{in.parens}{Logical scalar. This and the five next
  arguments work like the eponymous ones of
  \code{\link{wells}}, but here are applied to the plate
  name. See there for details.}

  \item{max}{Numeric scalar.}

  \item{clean}{Logical scalar.}

  \item{brackets}{Logical scalar.}

  \item{word.wise}{Logical scalar.}

  \item{paren.sep}{Character scalar.}

  \item{downcase}{Logical scalar.}

  \item{normalize}{Logical scalar. Attempt to normalise the
  plate-type string before interpreting it?}

  \item{subtype}{Logical scalar. Keep the plate subtype
  indicator, if any? Only relevant for the character or
  factor method.}

  \item{to}{Character scalar indicating the plate type.
  User-defined plate types must be given literally. For
  generation-III plates, use \kbd{gen.iii}; for the
  EcoPlate\eqn{\textsuperscript{\texttrademark}}{(TM)}, use
  \kbd{eco}; the remaining allowed values are only
  \kbd{sf.n2}, \kbd{sf.p2}, \kbd{an2}, \kbd{ff} and
  \kbd{yt}, but matching is case-insensitive.

  A character vector of length greater than one can be
  passed to the \code{\link{MOPMX}} method, which recycles
  its \code{to} argument.}

  \item{...}{Optional arguments passed between the methods.
  For \code{register_plate}, named arguments to be used if
  \code{object} is missing.}
}
\value{
  Character scalar in the case of the \code{\link{OPM}} and
  \code{\link{OPMS}} methods of \code{plate_type},
  otherwise a character vector with the same length than
  \code{object}, or a corresponding factor.  If
  \code{object} is not given, a character vector of
  normalised plate-type names.

  \code{gen_iii} returns a novel \code{\link{OPMX}} object.

  \code{register_plate} returns a logical vector whose
  values indicate whether information was registered or
  deleted and whose names are the normalised plate-type
  names.
}
\description{
  Get the type of the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} plate
  used in the measuring, normalise plate-type names,
  display known names, or modify the plate type after
  inputting the plate data. Alternatively, register or
  remove user-defined plate types.
}
\details{
  The \code{\link{OPM}} and \code{\link{OPMS}} methods of
  \code{plate_type} are convenience methods for one of the
  more important entries of \code{\link{csv_data}} with
  additional options useful for creating plot titles.

  The character method normalises the names of
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  \acronym{PM} plates to the internally used naming scheme.
  Unrecognised names are returned unchanged. This needs not
  normally be called by the \pkg{opm} user but might be of
  interest.

  Factors are treated by passing their levels through the
  character method.

  If a logical scalar is given as \code{object} argument,
  \code{TRUE} restricts the output to user-defined plate
  types (an empty set by default), \code{FALSE} to the
  plate types that ship with \pkg{opm}, and \code{NA} shows
  all plates.

  \code{gen_iii} change the plate type of an
  \code{\link{OPM}} object to \sQuote{Generation III} or
  another plate type. This is currently the only function
  to change plate names. It is intended for Generation-III
  or other plates that were not devised for the OmniLog
  instrument but can be run just like \acronym{PM} plates.
  Usually they will be annotated as some \acronym{PM} plate
  by the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  system. In contrast, input ID-mode plates are
  automatically detected (see
  \code{\link{read_single_opm}}). For this reason,
  \code{gen_iii} does not enable changes to \acronym{PM}
  plate types.

  User-defined plate types are allowed but need the
  according prefix (which must currently case-insensitively
  match \sQuote{Custom:}), even though additional
  normalisation is done. It is an error to set a
  user-defined plate type that has not beforehand been
  registered with \code{register_plate}.

  The actual spelling of the plate type used might (in
  theory) differ between distinct versions of \pkg{opm} but
  is internally consistent. It is an error to set one of
  the \acronym{PM} plate types or to assign an unknown
  plate type.

  Two kinds of information can be registered for
  user-defined plates: the full name of the plate and/or
  the full names of the substrates for all well
  coordinates. Both kinds of information can be deleted
  again. In any case, the name of the argument within
  \code{...} or within \code{object} given as list must
  indicate the plate type. Normalisation is done, as well
  as adding the usual prefix for user-defined plates.

  For registering a full plate name, an unnamed character
  scalar must be provided. For registering the mapping from
  well coordinates to substrate names, a named character
  vector must be provided with the names indicating the
  well coordinates and the elements indicating the
  according substrate names. Alternatively, a matrix or
  data frame can be provided that imitates that physical
  structure of the plate. That is, the rows are the plate
  rows (A, B, C, ...) and the columns are the plate columns
  (1, 2, 3, ...). If row or column names are used, they are
  honoured.

  For deleting a user-defined plate, an empty value (such
  as \code{NULL} or an empty vector) must be provided.
  Deletion is done for both full plate names and mappings
  from well coordinates to substrate names. It is ignored
  whether or not this information had been registered
  beforehand.
}
\examples{
## 'OPM' method
(x <- plate_type(vaas_1, full = FALSE))
(y <- plate_type(vaas_1, full = TRUE))
(z <- plate_type(vaas_1, full = TRUE, in.parens = FALSE))
# strings lengths differ depending on the selection
stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))

\dontrun{

# Splitting a list of 'OPM' objects according to the plate type is easy:
x <- split(x), sapply(x, plate_type))
# but see also opms() and read_opm(), which can do this internally
}

## 'OPMS' method
(xx <- plate_type(vaas_4, full = FALSE))
# plate type is guaranteed to be uniform within an OPMS object
stopifnot(identical(x, xx))

## character and factor methods

# Entirely unrecognized strings are returned as-is
(x <- plate_type(letters))
stopifnot(identical(x, letters))

# Something more realistic
(x <- plate_type(y <- c("PM1", "PM-11C", "PMM04-a"), subtype = TRUE))
stopifnot(x != y)

# Factors
(z <- plate_type(as.factor(y), subtype = TRUE))
stopifnot(is.factor(z), z == x) # same result after conversion

## 'missing' method
(x <- plate_type())
stopifnot(is.character(x), plate_type(vaas_1) \%in\% x)

## changing the plate type

# 'OPM' method
plate_type(copy <- gen_iii(vaas_1))
stopifnot(identical(vaas_1, copy)) # the data set already had that plate type
plate_type(copy <- gen_iii(vaas_1, "eco")) # which is wrong, actually
stopifnot(!identical(vaas_1, copy))

# 'OPMS' method
plate_type(copy <- gen_iii(vaas_4))
stopifnot(identical(vaas_4, copy)) # as above
plate_type(copy <- gen_iii(vaas_4, "eco"))
stopifnot(!identical(vaas_4, copy)) # as above

## registering plate types

# well map and full name of a plate can be simultaneously registered
register_plate(myplate = c(A01 = "Glucose", A02 = "Fructose"),
  myplate = "Simple fake test plate")
# note standardization of name
stopifnot("CUSTOM:MYPLATE" \%in\% plate_type(TRUE))
# queries can be done ignoring case differences
listing(wells(plate = "custom:myplate"))

# input/output of plate types
plate.file <- tempfile()
write(to_yaml(listing(wells(plate = "custom:myplate"))), plate.file)
register_plate(plate.file)
unlink(plate.file) # tidying up

# erasing this plate type again; this will delete well map and full name
register_plate(myplate = NULL)
stopifnot(!"CUSTOM:MYPLATE" \%in\% plate_type(TRUE))
}
\seealso{
  base::strtrim base::abbreviate base::gsub

  Other naming-functions: \code{\link{find_positions}},
  \code{\link{find_substrate}}, \code{\link{listing}},
  \code{\link{opm_files}}, \code{\link{param_names}},
  \code{\link{select_colors}},
  \code{\link{substrate_info}}, \code{\link{wells}}
}
\keyword{attribute}
\keyword{character}
\keyword{manip}
\keyword{utilities}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{gen_iii-methods}
\alias{gen_iii,MOPMX-method}
\alias{gen_iii,OPM-method}
\alias{gen_iii,OPMS-method}
\alias{plate_type-methods}
\alias{plate_type,MOPMX-method}
\alias{plate_type,OPM-method}
\alias{plate_type,OPMS-method}
\alias{plate_type,OPM_DB-method}
\alias{plate_type,character-method}
\alias{plate_type,factor-method}
\alias{plate_type,logical-method}
\alias{plate_type,missing-method}
\alias{register_plate-methods}
\alias{register_plate,character-method}
\alias{register_plate,list-method}
\alias{register_plate,missing-method}
\usage{
  \S4method{gen_iii}{MOPMX}(object, ...) 
  \S4method{gen_iii}{OPM}(object, to = "gen.iii") 
  \S4method{gen_iii}{OPMS}(object, ...) 

  \S4method{plate_type}{MOPMX}(object, ..., normalize = FALSE,
    subtype = FALSE) 
  \S4method{plate_type}{OPM}(object, ..., normalize = FALSE,
    subtype = FALSE) 
  \S4method{plate_type}{OPMS}(object, ...) 
  \S4method{plate_type}{OPM_DB}(object, ..., normalize = FALSE,
    subtype = FALSE) 
  \S4method{plate_type}{character}(object, full = FALSE,
    in.parens = TRUE, max = opm_opt("max.chars"), clean = TRUE,
    brackets = FALSE, word.wise = FALSE, paren.sep = " ", downcase = FALSE,
    normalize = TRUE, subtype = FALSE) 
  \S4method{plate_type}{factor}(object, ...) 
  \S4method{plate_type}{logical}(object, ...) 
  \S4method{plate_type}{missing}(object, ...) 

  \S4method{register_plate}{character}(object, ...) 
  \S4method{register_plate}{list}(object, ...) 
  \S4method{register_plate}{missing}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
