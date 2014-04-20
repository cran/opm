\name{[<-}
\alias{$<-}
\alias{[<-}
\alias{[[<-}
\title{Assign subset}
\arguments{
  \item{x}{\code{\link{OPMS}} or \code{\link{MOPMX}}
  object.}

  \item{i}{One to several plate indexes. Should be
  compatible with the length of \code{value}. Otherwise any
  resulting \code{NULL} elements will be removed (with a
  warning), causing the resulting plate indexes to be
  unequal to \code{i}, which might be confusing.}

  \item{j}{Must \strong{not} be set. See the examples.}

  \item{...}{Must neither be used.}

  \item{name}{Unevaluated symbol used for as index of a
  single element.}

  \item{value}{Value to be assigned. \code{NULL} causes the
  selected plates to be removed. Alternatively,
  \code{\link{OPM}} or \code{\link{OPMS}} objects or lists
  of \code{\link{OPM}} objects can be assigned. All
  assignments are subject to the restrictions explained in
  the help entries of the \code{\link{OPMS}} and
  \code{\link{MOPMX}} classes.}
}
\value{
  \code{value}.
}
\description{
  Assign subsets of \code{\link{OPMS}} objects.
}
\examples{
copy <- vaas_4
copy[5] <- NULL # has no effect
stopifnot(identical(vaas_4, copy))
length(copy)
copy[2:3] <- NULL # removes these plates
length(copy)
stopifnot(length(vaas_4) == length(copy) + 2)
copy[1:4] <- vaas_4 # set all plates to the plates from 'vaas_4'
stopifnot(identical(vaas_4, copy))
copy[3] <- copy[3] # no change
stopifnot(identical(vaas_4, copy))
copy[3] <- copy[2] # now assign other plate
stopifnot(!identical(vaas_4, copy))
copy[6] <- copy[1] # gaps will be closed
stopifnot(length(copy) == 5) # not 6
}
\seealso{
  Other combination-functions: \code{\link{c}},
  \code{\link{opms}}, \code{\link{plus}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{dollar.set-methods}
\alias{dollar.set,MOPMX,ANY-method}
\alias{dollar.set,MOPMX-method}
\alias{$<-,MOPMX,ANY-method}
\alias{$<-,MOPMX-method}
\alias{bracket.set-methods}
\alias{bracket.set,MOPMX,ANY,missing,ANY-method}
\alias{bracket.set,MOPMX,ANY,missing-method}
\alias{[<-,MOPMX,ANY,missing,ANY-method}
\alias{[<-,MOPMX,ANY,missing-method}
\alias{bracket.set,OPMS,ANY,missing,NULL-method}
\alias{[<-,OPMS,ANY,missing,NULL-method}
\alias{bracket.set,OPMS,ANY,missing,OPM-method}
\alias{[<-,OPMS,ANY,missing,OPM-method}
\alias{bracket.set,OPMS,ANY,missing,OPMS-method}
\alias{[<-,OPMS,ANY,missing,OPMS-method}
\alias{bracket.set,OPMS,ANY,missing,list-method}
\alias{[<-,OPMS,ANY,missing,list-method}
\alias{double.bracket.set-methods}
\alias{double.bracket.set,MOPMX,ANY,missing,ANY-method}
\alias{double.bracket.set,MOPMX,ANY,missing-method}
\alias{[[<-,MOPMX,ANY,missing,ANY-method}
\alias{[[<-,MOPMX,ANY,missing-method}
\usage{
  \S4method{$}{MOPMX,ANY}(x, name) <- value

  \S4method{[}{MOPMX,ANY,missing,ANY}(x, i, ...) <- value
  \S4method{[}{OPMS,ANY,missing,NULL}(x, i, j) <- value
  \S4method{[}{OPMS,ANY,missing,OPM}(x, i, j) <- value
  \S4method{[}{OPMS,ANY,missing,OPMS}(x, i, j) <- value
  \S4method{[}{OPMS,ANY,missing,list}(x, i, j) <- value

  \S4method{[[}{MOPMX,ANY,missing,ANY}(x, i, ...) <- value
}
%% END INSERTION BY repair_S4_docu.rb
