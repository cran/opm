\docType{class}
\name{WMD}
\alias{WMD}
\title{WMD class}
\description{
  This is a virtual class facilitating the management of
  metadata. No objects can be created from it because
  metadata without data make not much sense. It is used by
  its child classes such as \code{\link{OPM}}, but it is
  not directly applied by an \pkg{opm} user.
}
\details{
  \itemize{ \item \sQuote{WMD} is an acronym for
  \sQuote{with metadata}.  \item Conceptually, this class
  treats metadata as arbitrarily nested lists with
  arbitrary content. Containers of objects that inherit
  from this class are not forced to contain the same
  metadata entries. Problems might arise if such data are
  queried and attempted to be converted to, e.g., data
  frames because some values might be missing. But metadata
  can be queried beforehand for the keys as well as the
  values they contain, and other methods support setting,
  modifying and deleting metadata.  \item For
  \code{\link{OPM}} and the other \pkg{opm} classes that
  use it, \sQuote{metadata} refers to information that, in
  contrast to, e.g., \code{\link{csv_data}} must be added
  by the user \strong{after} reading OmniLog(R) CSV files.
  Metadata might already be present in YAML files created
  by the \pkg{opm} package, however. }
}
\seealso{
  Methods

  Other classes: \code{\link{MOA}}, \code{\link{OPM}},
  \code{\link{OPMA}}, \code{\link{OPMS}},
  \code{\link{OPMX}}, \code{\link{YAML_VIA_LIST}}
}
\keyword{methods}

% \name{WMD-class}
\Rdversion{1.1}
% \docType{class}
\alias{WMD-class}
% \alias{\%K\%,character,WMD-method}
% \alias{\%K\%,list,WMD-method}
% \alias{\%Q\%,character,WMD-method}
% \alias{\%Q\%,list,WMD-method}
% \alias{\%k\%,character,WMD-method}
% \alias{\%k\%,list,WMD-method}
% \alias{\%q\%,character,WMD-method}
% \alias{\%q\%,list,WMD-method}
% \alias{map_metadata,WMD,character-method}
% \alias{map_metadata,WMD,function-method}
% \alias{metadata,WMD-method}
% \alias{metadata<-,WMD,character,ANY-method}
% \alias{metadata<-,WMD,list,list-method}
% \alias{metadata<-,WMD,missing,list-method}
% \alias{metadata<-,WMD,numeric,list-method}
% \alias{metadata_chars,WMD-method}

% \title{Class \code{"WMD"}}
% \description{
% %%  ~~ A concise (1-5 lines) description of what the class is. ~~
% }
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{metadata}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\%K\%}{\code{signature(x = "character", table = "WMD")}: ... }
    \item{\%K\%}{\code{signature(x = "list", table = "WMD")}: ... }
    \item{\%Q\%}{\code{signature(x = "character", table = "WMD")}: ... }
    \item{\%Q\%}{\code{signature(x = "list", table = "WMD")}: ... }
    \item{\%k\%}{\code{signature(x = "character", table = "WMD")}: ... }
    \item{\%k\%}{\code{signature(x = "list", table = "WMD")}: ... }
    \item{\%q\%}{\code{signature(x = "character", table = "WMD")}: ... }
    \item{\%q\%}{\code{signature(x = "list", table = "WMD")}: ... }
    \item{map_metadata}{\code{signature(object = "WMD", mapping = "character")}: ... }
    \item{map_metadata}{\code{signature(object = "WMD", mapping = "function")}: ... }
    \item{metadata}{\code{signature(object = "WMD")}: ... }
    \item{metadata<-}{\code{signature(object = "WMD", key = "character", value = "ANY")}: ... }
    \item{metadata<-}{\code{signature(object = "WMD", key = "list", value = "list")}: ... }
    \item{metadata<-}{\code{signature(object = "WMD", key = "missing", value = "list")}: ... }
    \item{metadata<-}{\code{signature(object = "WMD", key = "numeric", value = "list")}: ... }
    \item{metadata_chars}{\code{signature(object = "WMD")}: ... }
	 }
}
% \references{
% %%  ~~put references to the literature/web site here~~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
% %%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
% }
\examples{
showClass("WMD")
}
\keyword{classes}
