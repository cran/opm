\docType{data}
\name{boccuto_et_al}
\alias{boccuto_et_al}
\title{Autism cell-line example data set}
\format{\code{boccuto_et_al} is a \code{\link{MOPMX}} object with four
  \code{\link{OPMX}} objects of the dimension 35 x 1 x 96 as elements. The
  plate types are \sQuote{CUSTOM:PM-M01} to \sQuote{CUSTOM:PM-M04}. The well
  assignment of these is fully identical to their non-custom counterparts,
  but separate plate types are nevertheless useful here to avoid comparing
  apples and oranges, as the scale of the measurements is totally different
  from OmniLog units.}
\description{
  Example data set for analysing phenotype microarray data
  that were not recorded with an
  OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
  instrument.
}
\details{
  The data set is identical to the supplement of Boccuto
  \emph{et al.} (2013) except for the following
  differences:\itemize{ \item The measurements are not
  logarithmised.  \item The negative controls are
  contained, hence the plates are complete.  \item The
  individuals N1 to N15 are missing, hence the data set is
  reduced to autism-spectrum disorder patients and control
  group.  } The data are point measurements, so a call to
  \code{\link{do_aggr}} would just copy the data. Two of
  the metadata entries are important, \sQuote{individual}
  for identifying the cell culture and \sQuote{group} for
  assigning the individuals to patients and control group,
  respectively.
}
\note{
  Information provided by C.E. Schwartz and colleagues
  additional to the supplement of Boccuto \emph{et al.}
  (2013) is gratefully acknowledged.
}
\examples{
\dontrun{

# Calling this yielded a variable 'boccuto_et_al' containing the data. The
# opm package must be loaded beforehand using library().
data(boccuto_et_al)

# Pseudo-aggregate the data (use a copy of each point measurement as
# maximum-height value).
boccuto_et_al <- do_aggr(boccuto_et_al)
}
# Copy the well maps of the pre-defined counterparts.
register_plate(`CUSTOM:PM-M01` = wells(plate = "PM-M01"))
register_plate(`CUSTOM:PM-M02` = wells(plate = "PM-M02"))
register_plate(`CUSTOM:PM-M03` = wells(plate = "PM-M03"))
register_plate(`CUSTOM:PM-M04` = wells(plate = "PM-M04"))
# Now the data would be ready for analysis.
}
\references{
  Boccuto, L., Chen, C.-F., Pittman, A.R., Skinner, C.D.,
  McCartney, H.J., Jones, K., Bochner, B.R., Stevenson,
  R.E., Schwartz, C.E.  2013. Decreased tryptophan
  metabolim in patients with autism spectrum disorder.
  \emph{Molecular Autism} \strong{4}: 16.

  Schwartz, C.E., pers. comm.
}
\keyword{datasets}

