\name{gen_iii}
\alias{gen_iii}
\title{Change to Generation III (or other plate type)}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{to}{Character scalar indicating the plate type.
  User-defined plate types must be given literally. For
  generation-III plates, use \sQuote{gen.iii}; for
  ecoplates, use \sQuote{eco}; the remaining allowed values
  are only \sQuote{sf.n2} and \sQuote{sf.p2}, but matching
  is case-insensitive.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Novel \code{\link{OPM}} object.
}
\description{
  Change the plate type of an \code{\link{OPM}} object to
  \sQuote{Generation III} or another plate type.
}
\details{
  This is currently the only function to change plate
  names. It is intended for Generation-III plates which
  were run like PM plates. Usually they will be annotated
  as some PM plate by the
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  system. In contrast, input ID-mode plates are
  automatically detected (see
  \code{\link{read_single_opm}}).

  The actual spelling of the plate type used might (in
  theory) differ between distinct versions of \pkg{opm} but
  is internally consistent. It is an error to set one of
  the PM plate types or to assign an unknown plate type.
}
\examples{
# 'OPM' method
data(vaas_1)
plate_type(copy <- gen_iii(vaas_1))
stopifnot(identical(vaas_1, copy)) # the dataset already had that plate type
plate_type(copy <- gen_iii(vaas_1, "eco")) # which is wrong, actually
stopifnot(!identical(vaas_1, copy))

# 'OPMS' method
data(vaas_4)
plate_type(copy <- gen_iii(vaas_4))
stopifnot(identical(vaas_4, copy)) # as above
plate_type(copy <- gen_iii(vaas_4, "eco"))
stopifnot(!identical(vaas_4, copy)) # as above
}
\seealso{
  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{merge}}, \code{\link{oapply}},
  \code{\link{plates}}, \code{\link{rep}},
  \code{\link{rev}}, \code{\link{sort}},
  \code{\link{thin_out}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{manip}

\docType{methods}
\alias{gen_iii-methods}
\alias{gen_iii,OPM-method}
\alias{gen_iii,OPMS-method}
\usage{
  \S4method{gen_iii}{OPM}(object, to = "gen.iii") 

  \S4method{gen_iii}{OPMS}(object, ...) 

}
