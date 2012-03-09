\docType{class}
\name{OPM}
\alias{OPM}
\title{OPM class}
\description{
  Class for holding single-plate OmniLog(R) phenotype
  microarray data without aggregated values, but with
  information read from the original input CSV files as
  well as an additional arbitrary amount of arbitrarily
  organized metadata. \sQuote{OPM} is an acronym for
  \sQuote{OmniLog(R) Phenotype Microarray}.
}
\note{
  Regarding the coercion of this class to other classes
  (see the \code{coerce} methods listed above and \code{as}
  from the \pkg{methods} package), consider the following:
  \itemize{ \item The coercion of this class (and its child
  classes) to a list (and vice versa) relies on a mapping
  between slot names and keys in the list, i.e. the list
  must be appropriately named. For instance, this is the
  mechanism when reading from and writing to YAML, see
  \code{\link{to_yaml}}.  \item Coercions to other
  dataframes and matrices first coerce the
  \code{\link{measurements}} and then add the other slots
  as attributes.  \item Methods such as
  \code{\link{flatten}} might be more appropriate for
  converting \code{\link{OPM}} objects.  }
}
\seealso{
  Methods

  Other classes: \code{\link{OPMA}}, \code{\link{OPMS}},
  \code{\link{OPMX}}, \code{\link{WMD}},
  \code{\link{YAML_VIA_LIST}}
}
\keyword{methods}

% \name{OPM-class}
\Rdversion{1.1}
% \docType{class}
\alias{OPM-class}
% \alias{+,OPM,OPM-method}
% \alias{+,OPM,OPMS-method}
% \alias{+,OPM,list-method}
% \alias{+,OPMS,OPM-method}
% \alias{[,OPM-method}
% \alias{attach_attr,OPM-method}
% \alias{coerce,OPM,data.frame-method}
% \alias{coerce,OPM,list-method}
% \alias{coerce,OPM,matrix-method}
% \alias{coerce,list,OPM-method}
% \alias{collect_template,OPM-method}
% \alias{csv_data,OPM-method}
% \alias{dim,OPM-method}
% \alias{do_aggr,OPM-method}
% \alias{filename,OPM-method}
% \alias{flatten,OPM-method}
% \alias{gen_iii,OPM-method}
% \alias{has_aggr,OPM-method}
% \alias{hours,OPM-method}
% \alias{include_metadata,OPM-method}
% \alias{initialize,OPM-method}
% \alias{level_plot,OPM-method}
% \alias{max,OPM-method}
% \alias{measurements,OPM-method}
% \alias{minmax,OPM-method}
% \alias{other_slots,OPM-method}
% \alias{plate_type,OPM-method}
% \alias{position,OPM-method}
% \alias{setup_time,OPM-method}
% \alias{summary,OPM-method}
% \alias{thin_out,OPM-method}
% \alias{to_grofit_data,OPM-method}
% \alias{to_grofit_time,OPM-method}
% \alias{well,OPM-method}
% \alias{wells,OPM-method}
% \alias{xy_plot,OPM-method}

% \title{Class \code{"OPM"}}
% \description{
% %%  ~~ A concise (1-5 lines) description of what the class is. ~~
% }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OPM", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{measurements}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{csv_data}:}{Object of class \code{"character"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{WMD}"}, directly.
Class \code{"\linkS4class{OPMX}"}, directly.
Class \code{"\linkS4class{YAML_VIA_LIST}"}, directly.
}
\section{Methods}{
  \describe{
    \item{+}{\code{signature(e1 = "OPM", e2 = "OPM")}: ... }
    \item{+}{\code{signature(e1 = "OPM", e2 = "OPMS")}: ... }
    \item{+}{\code{signature(e1 = "OPM", e2 = "list")}: ... }
    \item{+}{\code{signature(e1 = "OPMS", e2 = "OPM")}: ... }
    \item{[}{\code{signature(x = "OPM")}: ... }
    \item{attach_attr}{\code{signature(object = "OPM")}: ... }
    \item{coerce}{\code{signature(from = "OPM", to = "data.frame")}: ... }
    \item{coerce}{\code{signature(from = "OPM", to = "list")}: ... }
    \item{coerce}{\code{signature(from = "OPM", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "list", to = "OPM")}: ... }
    \item{collect_template}{\code{signature(object = "OPM")}: ... }
    \item{csv_data}{\code{signature(object = "OPM")}: ... }
    \item{dim}{\code{signature(x = "OPM")}: ... }
    \item{do_aggr}{\code{signature(object = "OPM")}: ... }
    \item{filename}{\code{signature(object = "OPM")}: ... }
    \item{flatten}{\code{signature(object = "OPM")}: ... }
    \item{gen_iii}{\code{signature(object = "OPM")}: ... }
    \item{has_aggr}{\code{signature(object = "OPM")}: ... }
    \item{hours}{\code{signature(object = "OPM")}: ... }
    \item{include_metadata}{\code{signature(object = "OPM")}: ... }
    \item{initialize}{\code{signature(.Object = "OPM")}: ... }
    \item{level_plot}{\code{signature(x = "OPM")}: ... }
    \item{max}{\code{signature(x = "OPM")}: ... }
    \item{measurements}{\code{signature(object = "OPM")}: ... }
    \item{minmax}{\code{signature(x = "OPM")}: ... }
    \item{other_slots}{\code{signature(object = "OPM")}: ... }
    \item{plate_type}{\code{signature(object = "OPM")}: ... }
    \item{position}{\code{signature(object = "OPM")}: ... }
    \item{setup_time}{\code{signature(object = "OPM")}: ... }
    \item{summary}{\code{signature(object = "OPM")}: ... }
    \item{thin_out}{\code{signature(object = "OPM")}: ... }
    \item{to_grofit_data}{\code{signature(object = "OPM")}: ... }
    \item{to_grofit_time}{\code{signature(object = "OPM")}: ... }
    \item{well}{\code{signature(object = "OPM")}: ... }
    \item{wells}{\code{signature(object = "OPM")}: ... }
    \item{xy_plot}{\code{signature(x = "OPM")}: ... }
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
showClass("OPM")
}
\keyword{classes}
