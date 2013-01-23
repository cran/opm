\name{glob_to_regex}
\alias{glob_to_regex}
\title{Convert wildcard to regular expression}
\arguments{
  \item{object}{Character vector or factor.}
}
\value{
  Character vector (or factor).
}
\description{
  Change a shell globbing wildcard into a regular
  expression. This is just a slightly extended version of
  \code{glob2rx} from the \pkg{utils} package, but more
  conversion steps might need to be added here in the
  future.
}
\details{
  The here used globbing search patterns contain only two
  special characters, \sQuote{?} and \sQuote{*}, and are
  thus more easy to master than regular expressions.
  \sQuote{?} matches a single arbitrary character, whereas
  \sQuote{*} matches zero to an arbitrary number of
  arbitrary characters. Some examples: \describe{
  \item{a?c}{Matches \sQuote{abc}, \sQuote{axc}, \sQuote{a
  c} etc. but not \sQuote{abbc}, \sQuote{abbbc},
  \sQuote{ac} etc.} \item{a*c}{Matches \sQuote{abc},
  \sQuote{abbc}, \sQuote{ac} etc. but not \sQuote{abd}
  etc.} \item{ab*}{Matches \sQuote{abc}, \sQuote{abcdefg},
  \sQuote{abXYZ} etc.  but not \sQuote{acdefg} etc.}
  \item{?bc}{Matches \sQuote{abc}, \sQuote{Xbc}, \sQuote{
  bc} etc. but not \sQuote{aabc}, \sQuote{abbc},
  \sQuote{bc} etc.} } Despite their simplicity, globbing
  patterns are often sufficient for selecting filenames.
}
\note{
  This is not normally directly called by an \pkg{opm} user
  because particularly \code{\link{explode_dir}} and the IO
  functions calling that function internally use
  \code{glob_to_regex} anyway.
}
\examples{
x <- "*what glob2rx() can't handle because a '+' is included*"
(y <- glob_to_regex(x))
(z <- glob2rx(x))
stopifnot(!identical(y, z))
# Factor method
(z <- glob_to_regex(as.factor(x)))
stopifnot(identical(as.factor(y), z))
}
\seealso{
  utils::glob2rx base::regex

  Other auxiliary-functions: \code{\link{contains}},
  \code{\link{kubrick}}, \code{\link{map_names}},
  \code{\link{map_values}}, \code{\link{opm_opt}},
  \code{\link{separate}}
}
\keyword{character}

\docType{methods}
\alias{glob_to_regex-methods}
\alias{glob_to_regex,character-method}
\alias{glob_to_regex,factor-method}
\usage{
  \S4method{glob_to_regex}{character}(object) 

  \S4method{glob_to_regex}{factor}(object) 

}
