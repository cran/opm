\docType{class}
\name{OPMA}
\alias{OPMA}
\title{OPMA class}
\description{
  Class for holding single-plate OmniLog(R) phenotype
  microarray data together with aggregated values. For
  further details see its parent class, \code{\link{OPM}}.
  \sQuote{OPMA} is an acronym for \sQuote{OPM, aggregated}.
}
\seealso{
  Methods

  Other classes: \code{\link{MOA}}, \code{\link{OPM}},
  \code{\link{OPMS}}, \code{\link{OPMX}},
  \code{\link{WMD}}, \code{\link{YAML_VIA_LIST}}
}
\keyword{methods}

% \name{OPMA-class}
\Rdversion{1.1}
% \docType{class}
\alias{OPMA-class}
% \alias{[,OPMA-method}
%% \alias{aggr_settings,OPMA-method}
%% \alias{aggregated,OPMA-method}
%% \alias{coerce,OPMA,data.frame-method}
%% \alias{coerce,OPMA,list-method}
%% \alias{coerce,OPMA,matrix-method}
%% \alias{coerce,list,OPMA-method}
%% 
% \title{Class \code{"OPMA"}}
% \description{
%% %%  ~~ A concise (1-5 lines) description of what the class is. ~~
%% }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OPMA", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{aggregated}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{aggr_settings}:}{Object of class \code{"list"} ~~ }
    \item{\code{measurements}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{csv_data}:}{Object of class \code{"character"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{OPM}"}, directly.
Class \code{"\linkS4class{WMD}"}, by class "OPM", distance 2.
Class \code{"\linkS4class{OPMX}"}, by class "OPM", distance 2.
Class \code{"\linkS4class{YAML_VIA_LIST}"}, by class "OPM", distance 2.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "OPMA")}: ... }
    \item{aggr_settings}{\code{signature(object = "OPMA")}: ... }
    \item{aggregated}{\code{signature(object = "OPMA")}: ... }
    \item{coerce}{\code{signature(from = "OPMA", to = "data.frame")}: ... }
    \item{coerce}{\code{signature(from = "OPMA", to = "list")}: ... }
    \item{coerce}{\code{signature(from = "OPMA", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "list", to = "OPMA")}: ... }
	 }
}
% \references{
%% %%  ~~put references to the literature/web site here~~
%% }
% \author{
%% %%  ~~who you are~~
%% }
% \note{
%% %%  ~~further notes~~
%% }
% 
% %% ~Make other sections like Warning with \section{Warning }{....} ~
% 
% \seealso{
% %%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
% %%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
% }
\examples{
showClass("OPMA")
}
\keyword{classes}
