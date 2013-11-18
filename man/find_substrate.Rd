\name{find_substrate}
\alias{find_positions}
\alias{find_substrate}
\title{Identify substrates or positions}
\arguments{
  \item{object}{Query character vector or factor, when
  searching for positions alternatively a list, an S3
  object of class \sQuote{substrate_match}, an
  \code{\link{OPM}} or an \code{\link{OPMS}} object.}

  \item{type}{Ignored if empty or \code{FALSE}. Otherwise,
  passed to \code{\link{plate_type}} for normalisation and
  then used to restrict the positions to those in that
  plate. Changes the output object to a vector; see below
  for details. In the case of \code{\link{OPMX}} objects,
  this can be set to \code{TRUE}, causing the use of the
  plate of \code{object}.}

  \item{search}{Character scalar indicating the search
  mode. \describe{ \item{exact}{Query names must exactly
  match (parts of) the well annotations.} \item{glob}{Shell
  globbing is used. See \code{\link{glob_to_regex}} for a
  description of globbing patterns.}
  \item{approx}{Approximate matching is used; the number or
  proportion of errors allowed is set using \code{max.dev},
  and neither globbing or regular-expression matching is
  done in that case.} \item{regex}{Regular-expression
  matching is used.} \item{pmatch}{Uses \code{pmatch} from
  the \pkg{base} package.} } All matching is
  case-insensitive except for the \kbd{exact} and
  \kbd{pmatch} search modes.}

  \item{max.dev}{Numeric scalar indicating the maximum
  allowed deviation. If < 1, the proportion of characters
  that might deviate, otherwise their absolute number. It
  can also be a list; see the \sQuote{max.distance}
  argument of \code{agrep} in the \pkg{base} package for
  details. Has an effect only if \sQuote{approx} is chosen
  as search mode (see the \code{search} argument).}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  \code{find_substrate} returns an S3 object of class
  \sQuote{substrate_match}; basically a list of character
  vectors (empty if nothing was found), with duplicates
  removed and the rest sorted. The names of the list
  correspond to \code{names}.

  The \code{find_positions} character method returns a list
  of character matrices (empty if nothing was found), with
  one row per position found, the plate name in the first
  column and the well name in the second. The names of this
  list correspond to \code{names}. The \code{\link{OPM}}
  and \code{\link{OPMS}} methods do the same, using their
  own substrates. The list and \sQuote{substrate_match}
  methods return lists of such lists.
}
\description{
  Identify the names of substrates as used in the stored
  plate annotations, or identify the positions of
  substrates, i.e. the plate(s) and well(s) in which they
  occur. Exact or error-tolerant matching can be used, as
  well as globbing and regular-expression matching.
}
\details{
  When searching for positions, the query names must be
  written exactly as used in the stored plate annotations.
  To determine their spelling, use \code{find_substrate}.
  This spelling is not guaranteed to be stable between
  distinct \pkg{opm} releases.
}
\examples{
## find_substrate()

# Note that 'exact' search matches parts of the names, whereas globbing
# matches entire strings if there are no wildcards (which wouldn't make much
# sense)
(x <- find_substrate("D-Glucose", search = "exact"))
(y <- find_substrate("D-Glucose", search = "glob"))
stopifnot(length(x[[1]]) > length(y[[1]]))

# 'pmatch' matching matches partially at the beginning and returns at most
# one match (the first one)
(y <- find_substrate("D-Glucose", search = "pmatch"))
stopifnot(length(x[[1]]) > length(y[[1]]))

# Now allowing mismatches
(z <- find_substrate("D-Glucose", search = "approx"))
stopifnot(length(z[[1]]) > length(x[[1]]))

# Factor method
(zz <- find_substrate(as.factor("D-Glucose"), search = "approx"))
stopifnot(identical(z, zz))

## find_positions()

# Character method; compare correct and misspelled substrate name
(x <- find_positions(c("D-Glucose", "D-Gloucose")))
stopifnot(length(x[[1]]) > length(x[[2]]))

# Factor method
(y <- find_positions(as.factor(c("D-Glucose", "D-Gloucose"))))
stopifnot(identical(y, x))

# Restrict to  a certain plate
(x <- find_positions(c("D-Glucose", "D-Gloucose"), type = "Gen III"))
stopifnot(is.character(x), any(is.na(x)), !all(is.na(x)))

# List method
x <- find_positions(find_substrate(c("D-Glucose", "D-Gloucose")))
x[[1]][1:3]
x[[2]]
stopifnot(length(x[[1]]) > length(x[[2]]))
}
\seealso{
  base::grep base::agrep

  Other naming-functions: \code{\link{gen_iii}},
  \code{\link{listing}}, \code{\link{opm_files}},
  \code{\link{param_names}}, \code{\link{plate_type}},
  \code{\link{select_colors}},
  \code{\link{substrate_info}}, \code{\link{wells}}
}
\keyword{character}
\keyword{utilities}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{find_positions-methods}
\alias{find_positions,OPM-method}
\alias{find_positions,OPMS-method}
\alias{find_positions,character-method}
\alias{find_positions,factor-method}
\alias{find_positions,list-method}
\alias{find_positions,substrate_match-method}
\alias{find_substrate-methods}
\alias{find_substrate,character-method}
\alias{find_substrate,factor-method}
\usage{
  \S4method{find_positions}{OPM}(object, type = NULL, ...) 
  \S4method{find_positions}{OPMS}(object, ...) 
  \S4method{find_positions}{character}(object, type = NULL, ...) 
  \S4method{find_positions}{factor}(object, ...) 
  \S4method{find_positions}{list}(object, ...) 
  \S4method{find_positions}{substrate_match}(object, ...) 

  \S4method{find_substrate}{character}(object,
    search = c("exact", "glob", "approx", "regex", "pmatch"), max.dev = 0.2) 
  \S4method{find_substrate}{factor}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
