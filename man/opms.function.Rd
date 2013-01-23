\name{opms}
\alias{opms}
\title{OPMS constructor}
\usage{
  opms(..., precomputed = TRUE, skip = FALSE,
    group = FALSE)
}
\arguments{
  \item{...}{One to several objects which are either
  potentially nested lists of \code{\link{OPMS}},
  \code{\link{OPM}} or \code{\link{OPMA}} objects, or
  really nested lists whose sublists can be converted to an
  \code{\link{OPM}} or \code{\link{OPMA}} object.}

  \item{precomputed}{Logical scalar. If \code{TRUE},
  sublists have already been converted to one of the three
  classes. Otherwise, suitable sublists will be converted.}

  \item{skip}{Logical scalar. If \code{precomputed} is
  \code{TRUE}, silently skip non-list elements of nested
  lists? If \code{precomputed} is \code{FALSE}, silently
  skip objects that do not belong to the three target
  classes? Otherwise, an error is generated if such a list
  element is encountered.}

  \item{group}{Logical or character scalar. If \code{TRUE},
  split the list of collected \code{\link{OPM}} objects
  according to the plate type and convert the sublists
  seperately if they contain more than one plate; otherwise
  just keep the \code{\link{OPM}} object. \code{FALSE} is
  the default: all plates are tried to be forced into a
  single \code{\link{OPMS}} object. If a character scalar,
  the name of the plate type to be extracted.}
}
\value{
  \code{\link{OPMS}} object, or list of such objects
  (and/or \code{\link{OPM}} objects), or \code{\link{OPM}}
  object, or \code{NULL}.
}
\description{
  Easily build \code{\link{OPMS}} objects.
}
\details{
  While otherwise rather flexible, this function will fail
  to return an \code{\link{OPMS}} object if the plate types
  do not match (simply because such \code{\link{OPMS}}
  objects are disallowed) and \code{group} is set to
  \code{FALSE}. But if \code{group} is set to \code{TRUE},
  a list, not a single \code{\link{OPMS}} object will be
  returned; and if \code{group} is of mode
  \sQuote{character}, this extracts the plate type(s) of
  interest.
}
\note{
  Consider also the plate-type selection options of
  \code{\link{read_opm}}.
}
\examples{
## Testing distinct OPM/OPMS combinations -- all should work
data(vaas_1, vaas_4)

(x <- opms())
stopifnot(is.null(x))

summary((x <- opms(vaas_1)))
stopifnot(identical(x, vaas_1))

summary((x <- opms(vaas_4, group = plate_type(vaas_4))))
stopifnot(identical(x, vaas_4))

summary((x <- opms(vaas_4, group = "PM01")))
stopifnot(is.null(x))

summary(x <- opms(vaas_1, vaas_1))
stopifnot(is(x, "OPMS"), length(x) == 2L)

summary(x <- opms(vaas_4, vaas_1))
stopifnot(is(x, "OPMS"), length(x) == 5L)

summary(x <- opms(vaas_1, vaas_4))
stopifnot(is(x, "OPMS"), length(x) == 5L)

summary(x <- opms(vaas_4, vaas_4))
stopifnot(is(x, "OPMS"), length(x) == 8L)
}
\seealso{
  Other combination-functions: \code{\link{c}},
  \code{\link{plus}}
}
\keyword{manip}

