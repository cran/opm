\docType{class}
\name{MOA}
\alias{MOA}
\title{MOA class}
\description{
  This is a virtual class facilitating the implementation
  of functionality for both matrices and arrays. Methods
  defined for objects from the class can be applied to
  either kind of object, but this class is not directly
  dealt with by an \pkg{opm} user.
}
\details{
  \sQuote{MOA} is an acronym for \sQuote{matrix or array}.
}
\seealso{
  Other classes: \code{\link{OPM}}, \code{\link{OPMA}},
  \code{\link{OPMS}}, \code{\link{OPMX}},
  \code{\link{WMD}}, \code{\link{YAML_VIA_LIST}}
}
\keyword{methods}

% \name{MOA-class}
\Rdversion{1.1}
% \docType{class}
\alias{MOA-class}
% \alias{discrete,MOA-method}
%% \alias{is_constant,MOA-method}
%% \alias{map_names,MOA,character-method}
%% \alias{map_names,MOA,function-method}
%% \alias{map_names,MOA,missing-method}
%% \alias{map_values,MOA,character-method}
%% \alias{map_values,MOA,function-method}
%% \alias{map_values,MOA,missing-method}
%% \alias{ranging,MOA-method}
%% 
% \title{Class \code{"MOA"}}
% \description{
%% %%  ~~ A concise (1-5 lines) description of what the class is. ~~
%% }
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
  \describe{
    \item{discrete}{\code{signature(x = "MOA")}: ... }
    \item{is_constant}{\code{signature(x = "MOA")}: ... }
    \item{map_names}{\code{signature(object = "MOA", mapping = "character")}: ... }
    \item{map_names}{\code{signature(object = "MOA", mapping = "function")}: ... }
    \item{map_names}{\code{signature(object = "MOA", mapping = "missing")}: ... }
    \item{map_values}{\code{signature(object = "MOA", mapping = "character")}: ... }
    \item{map_values}{\code{signature(object = "MOA", mapping = "function")}: ... }
    \item{map_values}{\code{signature(object = "MOA", mapping = "missing")}: ... }
    \item{ranging}{\code{signature(object = "MOA")}: ... }
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
showClass("MOA")
}
\keyword{classes}
