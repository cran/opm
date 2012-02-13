\name{pick_from}
\alias{pick_from}
\title{Pick rows}
\arguments{
  \item{object}{Dataframe. At least two rows are needed.}

  \item{selection}{Named list, keys should correspond to
  column names of \code{object}, values to one to several
  alternative values that should occur in the respective
  dataframe column.}
}
\value{
  Dataframe.
}
\description{
  Pick rows from a dataframe if selected columns are
  identical to keys.
}
\keyword{internal}

\docType{methods}
\alias{pick_from-methods}
\alias{pick_from,data.frame-method}
\usage{
  \S4method{pick_from}{data.frame}(object, selection) 
}
