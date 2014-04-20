\docType{data}
\name{potato}
\alias{potato}
\title{Potato cell-line growth data set}
\format{Data frame with 540 rows and six columns. The columns are:
\describe{
  \item{Genotype}{Factor containing the names of the cell lines. See Vaas
  \emph{et al.} (2013) for further details.}
  \item{Treatment}{Factor describing the three applied stress conditions.
  There is also one level for the control.}
  \item{Replicate}{Integer vector with the number of the replicate. There
    are five replicates per combination of \sQuote{Genotype} and
    \sQuote{Treatment}.}
  \item{Time}{Integer vector containing the measurement times in days.}
  \item{FM}{Integer vector containing the fresh-mass measurements in
  milligramme.}
  \item{DM}{Integer vector containing the dry-mass measurements in
  milligramme.}
}}
\description{
  Example data set for analysing growth curves with
  \pkg{opm} containing manually entered fresh-mass and
  dry-mass measurements over time from three distinct
  potato (\emph{Solanum tuberosum}) cell lines under
  several stress treatments.
}
\details{
  How to convert and analyse this data set is explained in
  the vignette on working with growth curves in \pkg{opm}.
}
\examples{
\dontrun{

# Calling this yielded a variable 'potato' containing the data. The opm
# package must be loaded beforehand using library().
data(potato)
}
}
\references{
  Vaas, L. A. I., Marheine, M., Sikorski, J., Goeker, M.,
  Schumacher, H.-M. 2013 Impacts of pr-10a overexpression
  at the molecular and the phenotypic level.
  \emph{International Journal of Molecular Sciences}
  \strong{14}: 15141--15166.

  El-Banna, A., Hajirezaei, M. R., Wissing, J., Ali, Z.,
  Vaas, L., Heine-Dobbernack, E., Jacobsen, H.-J.,
  Schumacher, H.-M., Kiesecker, H.  2010 Over-expression of
  PR-10a leads to increased salt and osmotic tolerance in
  potato cell cultures. \emph{Journal of Biotechnology}
  \strong{150}: 277--287.

  Sandford, S. A. 1995. Apples and Oranges -- A Comparison.
  \emph{Annals of Improbable Research} \strong{1} (3).
}
\keyword{datasets}

