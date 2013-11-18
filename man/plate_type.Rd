\name{plate_type}
\alias{gen_iii}
\alias{plate_type}
\title{Plate type displayed or modified}
\arguments{
  \item{object}{\code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object, or character vector of
  original plate name(s), or factor. If missing, the
  function displays the plate types \pkg{opm} knows about.}

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
  \kbd{yt}, but matching is case-insensitive.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Character scalar in the case of the \code{\link{OPM}} and
  \code{\link{OPMS}} methods of \code{plate_type},
  otherwise a character vector with the same length than
  \code{object}, or a corresponding factor.  If
  \code{object} is not given, a character vector of
  normalised plate-type names.

  \code{gen_iii} returns a novel \code{\link{OPMX}} object.
}
\description{
  Get the type of the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} plate
  used in the measuring, normalise plate-type names,
  display known names, or modify the plate type after
  inputting the plate data.
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

  \code{gen_iii} change the plate type of an
  \code{\link{OPM}} object to \sQuote{Generation III} or
  another plate type. This is currently the only function
  to change plate names. It is intended for Generation-III
  plates which were run like \acronym{PM} plates. Usually
  they will be annotated as some \acronym{PM} plate by the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  system. In contrast, input ID-mode plates are
  automatically detected (see
  \code{\link{read_single_opm}}).

  The actual spelling of the plate type used might (in
  theory) differ between distinct versions of \pkg{opm} but
  is internally consistent. It is an error to set one of
  the \acronym{PM} plate types or to assign an unknown
  plate type.
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
\alias{plate_type,character-method}
\alias{plate_type,factor-method}
\alias{plate_type,missing-method}
\usage{
  \S4method{gen_iii}{MOPMX}(object, ...) 
  \S4method{gen_iii}{OPM}(object, to = "gen.iii") 
  \S4method{gen_iii}{OPMS}(object, ...) 

  \S4method{plate_type}{MOPMX}(object, ...) 
  \S4method{plate_type}{OPM}(object, ..., normalize = FALSE,
    subtype = FALSE) 
  \S4method{plate_type}{OPMS}(object, ...) 
  \S4method{plate_type}{character}(object, full = FALSE,
    in.parens = TRUE, max = opm_opt("max.chars"), clean = TRUE,
    brackets = FALSE, word.wise = FALSE, paren.sep = " ", downcase = FALSE,
    normalize = TRUE, subtype = FALSE) 
  \S4method{plate_type}{factor}(object, ...) 
  \S4method{plate_type}{missing}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
