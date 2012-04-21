\name{gen_iii}
\alias{gen_iii}
\title{Change to Generation III}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Novel \code{\link{OPM}} object.
}
\description{
  Change the plate type of an \code{\link{OPM}} object to
  \sQuote{Generation III}. The actual spelling used might
  differ but is internally consistent.
}
\note{
  This is currently the only function to change plate
  names. It is intended for Generation-III plates which
  were run like PM plates. Usually they will be annotated
  as some PM plate by the OmniLog(R) system. In contrast,
  input ID-mode plates are automatically detected (see
  \code{\link{read_single_opm}}).
}
\examples{
# 'OPM' method
data(vaas_1)
summary(copy <- gen_iii(vaas_1))
stopifnot(identical(vaas_1, copy)) # the dataset already had that plate type

# 'OPMS' method
data(vaas_4)
summary(copy <- gen_iii(vaas_4))
stopifnot(identical(vaas_4, copy)) # as above
}
\keyword{manip}

\docType{methods}
\alias{gen_iii-methods}
\alias{gen_iii,OPM-method}
\alias{gen_iii,OPMS-method}
\usage{
  \S4method{gen_iii}{OPM}(object) 

  \S4method{gen_iii}{OPMS}(object, ...) 

}
