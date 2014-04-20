\name{summary}
\alias{summary}
\title{Summarise \acronym{OPMX} or \acronym{MOPMX} objects}
\arguments{
  \item{object}{\code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object.}

  \item{...}{Optional arguments to be included in the
  output.}
}
\value{
  For the \code{\link{OPM}} method, a named list of the
  class \code{OPM_Summary}, returned invisibly. The
  \sQuote{Metadata} entry is the number of non-list
  elements in \code{\link{metadata}}. For the
  \code{\link{OPMS}} method, a list of such lists (one per
  plate), also returned invisibly, with the class set to
  \code{OPMS_Summary} and some information on the entire
  object in the attribute \sQuote{overall}.
}
\description{
  Generate a summary (which also prints nicely to the
  screen), or display an \code{\link{OPM}},
  \code{\link{OPMS}} or \code{\link{MOPMX}} object on
  screen.
}
\details{
  Currently the \code{show} methods are just wrappers for
  the \code{summary} methods for these objects with an
  additional call to \code{print}. The \acronym{CMAT}
  method is only for internal use.
}
\examples{
# OPM method
(x <- summary(vaas_1))
stopifnot(is.list(x), is.object(x))
vaas_1 # calls show()

# OPMS method
(x <- summary(vaas_4))
stopifnot(is.list(x), length(x) == 4L, all(sapply(x, is.list)),
  is.object(x))
vaas_4 # calls show()
}
\seealso{
  base::summary base::formatDL methods::show base::print

  Other plotting-functions: \code{\link{ci_plot}},
  \code{\link{heat_map}}, \code{\link{level_plot}},
  \code{\link{parallelplot}}, \code{\link{radial_plot}},
  \code{\link{xy_plot}}
}
\keyword{attribute}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{show-methods}
\alias{show,CMAT-method}
\alias{show,MOPMX-method}
\alias{show,OPMX-method}
\alias{summary-methods}
\alias{summary,MOPMX-method}
\alias{summary,OPM-method}
\alias{summary,OPMS-method}
\usage{
  \S4method{show}{CMAT}(object) 
  \S4method{show}{MOPMX}(object) 
  \S4method{show}{OPMX}(object) 

  \S4method{summary}{MOPMX}(object, ...) 
  \S4method{summary}{OPM}(object, ...) 
  \S4method{summary}{OPMS}(object, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
