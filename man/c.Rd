\name{c}
\alias{c}
\alias{plus}
\title{Combination and addition of plates}
\arguments{
  \item{x}{\code{\link{OPMX}} object.}

  \item{...}{Other \R objects.}

  \item{recursive}{Logical scalar. See \code{c} from the
  \pkg{base} package.}

  \item{e1}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{e2}{\code{\link{OPM}} or \code{\link{OPMS}} object,
  or list.}
}
\value{
  \code{c} creates an \code{\link{OPMS}} object if
  possible, otherwise a list, or an \code{\link{OPM}}
  object (if \code{\dots} is not given and \code{x} is such
  an object).

  If successful, \code{+} yields an \code{\link{OPMS}}
  object that contains the plates from both \code{e1} and
  \code{e2}, but it raises an error if the plates cannot be
  combined.
}
\description{
  Combine a \code{\link{OPM}} or \code{\link{OPMS}} object
  with other objects.
}
\examples{
# Adding nothing
dim(x <- c(vaas_1))
stopifnot(identical(x, vaas_1))
dim(x <- c(vaas_4))
stopifnot(identical(x, vaas_4))

# Not particularly useful: adding identical plates!
dim(x <- c(vaas_1, vaas_1)) # yields a two-plate OPMS object
stopifnot(identical(dim(x), c(2L, dim(vaas_1))))

# Also not particularly useful: adding partially identical plates!
dim(x <- c(vaas_4, vaas_1))
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))

# The following examples do not show particularly useful additions, as the
# plates are either entirely or partially identical. Note the changes in the
# dimensions.

# OPM+OPM method
dim(x <- vaas_1 + vaas_1)
stopifnot(identical(dim(x), c(2L, dim(vaas_1))))

# OPM+OPMS method
dim(x <- vaas_1 + vaas_4)
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))

# OPM+list method
dim(x <- vaas_1 + list(vaas_1, vaas_1))
stopifnot(identical(dim(x), c(3L, dim(vaas_1))))

# OPMS+OPMS method
dim(x <- vaas_4 + vaas_4)
stopifnot(identical(dim(x), c(8L, dim(vaas_4)[-1L])))

# OPMS+OPM method
dim(x <- vaas_4 + vaas_1)
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))

# OPMS+list method
dim(x <- vaas_4 + list(vaas_1))
stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
}
\seealso{
  base::c

  Other combination-functions: \code{\link{$<-}},
  \code{\link{[<-}}, \code{\link{[[<-}}, \code{\link{opms}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{plus-methods}
\alias{plus,OPM,OPM-method}
\alias{+,OPM,OPM-method}
\alias{plus,OPM,OPMS-method}
\alias{+,OPM,OPMS-method}
\alias{plus,OPM,list-method}
\alias{+,OPM,list-method}
\alias{plus,OPMS,OPM-method}
\alias{+,OPMS,OPM-method}
\alias{plus,OPMS,OPMS-method}
\alias{+,OPMS,OPMS-method}
\alias{plus,OPMS,list-method}
\alias{+,OPMS,list-method}
\alias{c-methods}
\alias{c,MOPMX-method}
\alias{c,OPMX-method}
\usage{
  \S4method{+}{OPM,OPM}(e1, e2) 
  \S4method{+}{OPM,OPMS}(e1, e2) 
  \S4method{+}{OPM,list}(e1, e2) 
  \S4method{+}{OPMS,OPM}(e1, e2) 
  \S4method{+}{OPMS,OPMS}(e1, e2) 
  \S4method{+}{OPMS,list}(e1, e2) 

  \S4method{c}{MOPMX}(x, ..., recursive = FALSE) 
  \S4method{c}{OPMX}(x, ..., recursive = FALSE) 
}
%% END INSERTION BY repair_S4_docu.rb
