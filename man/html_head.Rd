\name{html_head}
\alias{html_head}
\title{Create HTML head}
\usage{
  html_head(title, css, meta)
}
\arguments{
  \item{title}{Character scalar defining the title of the
  \acronym{HTML} document. Must contain an attribute called
  as returned by \code{\link{opm_string}}.}

  \item{css}{Character vector containing the names of
  \acronym{CSS} files to link.}

  \item{meta}{Character vector defining additional meta
  tags.}
}
\value{
  Character vector.
}
\description{
  This is the helper function used by \code{\link{format}}
  and other functions to create the \sQuote{head} antry of
  an \acronym{HTML} strings.
}
\keyword{internal}

