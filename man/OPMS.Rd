\docType{class}
\name{OPMS}
\alias{OPMS}
\title{OPMS class}
\description{
  Class for holding multi-plate OmniLog(R) phenotype
  microarray data with or without aggregated values. The
  data may have been obtained from distinct organisms
  and/or replicates, but \strong{must} correspond to the
  same plate type and \strong{must} contain the same wells.
  Regarding the name: \sQuote{OPMS} is just the plural of
  \sQuote{OPM}.
}
\note{
  As a rule, OPMS has the same methods as the
  \code{\link{OPM}} class, but adapted to a collection of
  more than one \code{\link{OPM}} object.  Only the
  additional ones and those with special arguments and/or
  behaviors are documented in detail. Also, OPMS can hold
  \code{\link{OPMA}} as well as \code{\link{OPM}} objects,
  even though this is not indicated for all its methods in
  this manual.
}
\seealso{
  Methods

  Other classes: \code{\link{MOA}}, \code{\link{OPM}},
  \code{\link{OPMA}}, \code{\link{OPMX}},
  \code{\link{WMD}}, \code{\link{YAML_VIA_LIST}}
}
\keyword{methods}

% \name{OPMS-class}
\Rdversion{1.1}
% \docType{class}
\alias{OPMS-class}
% \alias{\%K\%,character,OPMS-method}
%% \alias{\%K\%,factor,OPMS-method}
%% \alias{\%K\%,list,OPMS-method}
%% \alias{\%Q\%,character,OPMS-method}
%% \alias{\%Q\%,factor,OPMS-method}
%% \alias{\%Q\%,list,OPMS-method}
%% \alias{\%k\%,character,OPMS-method}
%% \alias{\%k\%,factor,OPMS-method}
%% \alias{\%k\%,list,OPMS-method}
%% \alias{\%q\%,character,OPMS-method}
%% \alias{\%q\%,factor,OPMS-method}
%% \alias{\%q\%,list,OPMS-method}
%% \alias{+,OPM,OPMS-method}
%% \alias{+,OPMS,OPM-method}
%% \alias{+,OPMS,OPMS-method}
%% \alias{+,OPMS,list-method}
%% \alias{[,OPMS-method}
%% \alias{aggr_settings,OPMS-method}
%% \alias{aggregated,OPMS-method}
%% \alias{anyDuplicated,OPMS-method}
%% \alias{ci_plot,OPMS-method}
%% \alias{coerce,OPMS,list-method}
%% \alias{coerce,list,OPMS-method}
%% \alias{collect_template,OPMS-method}
%% \alias{contains,OPMS,OPM-method}
%% \alias{csv_data,OPMS-method}
%% \alias{dim,OPMS-method}
%% \alias{do_aggr,OPMS-method}
%% \alias{duplicated,OPMS-method}
%% \alias{extract,OPMS-method}
%% \alias{extract_columns,OPMS-method}
%% \alias{filename,OPMS-method}
%% \alias{flatten,OPMS-method}
%% \alias{gen_iii,OPMS-method}
%% \alias{has_aggr,OPMS-method}
%% \alias{heat_map,OPMS-method}
%% \alias{hours,OPMS-method}
%% \alias{include_metadata,OPMS-method}
%% \alias{initialize,OPMS-method}
%% \alias{length,OPMS-method}
%% \alias{level_plot,OPMS-method}
%% \alias{map_metadata,OPMS,ANY-method}
%% \alias{max,OPMS-method}
%% \alias{measurements,OPMS-method}
%% \alias{merge,OPMS,missing-method}
%% \alias{merge,OPMS,numeric-method}
%% \alias{metadata,OPMS-method}
%% \alias{metadata<-,OPMS,ANY,ANY-method}
%% \alias{metadata<-,OPMS,ANY,data.frame-method}
%% \alias{metadata<-,OPMS,character,data.frame-method}
%% \alias{metadata<-,OPMS,factor,data.frame-method}
%% \alias{metadata<-,OPMS,missing,data.frame-method}
%% \alias{metadata<-,OPMS,missing,formula-method}
%% \alias{metadata<-,OPMS,missing,list-method}
%% \alias{metadata_chars,OPMS-method}
%% \alias{minmax,OPMS-method}
%% \alias{oapply,OPMS-method}
%% \alias{phylo_data,OPMS-method}
%% \alias{plate_type,OPMS-method}
%% \alias{plates,OPMS-method}
%% \alias{position,OPMS-method}
%% \alias{radial_plot,OPMS-method}
%% \alias{rep,OPMS-method}
%% \alias{rev,OPMS-method}
%% \alias{select,OPMS-method}
%% \alias{seq,OPMS-method}
%% \alias{setup_time,OPMS-method}
%% \alias{show,OPMS-method}
%% \alias{sort,OPMS-method}
%% \alias{summary,OPMS-method}
%% \alias{thin_out,OPMS-method}
%% \alias{well,OPMS-method}
%% \alias{wells,OPMS-method}
%% \alias{xy_plot,OPMS-method}
%% 
% \title{Class \code{"OPMS"}}
% \description{
%% %%  ~~ A concise (1-5 lines) description of what the class is. ~~
%% }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OPMS", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{plates}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{OPMX}"}, directly.
Class \code{"\linkS4class{YAML_VIA_LIST}"}, directly.
}
\section{Methods}{
  \describe{
    \item{\%K\%}{\code{signature(x = "character", table = "OPMS")}: ... }
    \item{\%K\%}{\code{signature(x = "factor", table = "OPMS")}: ... }
    \item{\%K\%}{\code{signature(x = "list", table = "OPMS")}: ... }
    \item{\%Q\%}{\code{signature(x = "character", table = "OPMS")}: ... }
    \item{\%Q\%}{\code{signature(x = "factor", table = "OPMS")}: ... }
    \item{\%Q\%}{\code{signature(x = "list", table = "OPMS")}: ... }
    \item{\%k\%}{\code{signature(x = "character", table = "OPMS")}: ... }
    \item{\%k\%}{\code{signature(x = "factor", table = "OPMS")}: ... }
    \item{\%k\%}{\code{signature(x = "list", table = "OPMS")}: ... }
    \item{\%q\%}{\code{signature(x = "character", table = "OPMS")}: ... }
    \item{\%q\%}{\code{signature(x = "factor", table = "OPMS")}: ... }
    \item{\%q\%}{\code{signature(x = "list", table = "OPMS")}: ... }
    \item{+}{\code{signature(e1 = "OPM", e2 = "OPMS")}: ... }
    \item{+}{\code{signature(e1 = "OPMS", e2 = "OPM")}: ... }
    \item{+}{\code{signature(e1 = "OPMS", e2 = "OPMS")}: ... }
    \item{+}{\code{signature(e1 = "OPMS", e2 = "list")}: ... }
    \item{[}{\code{signature(x = "OPMS")}: ... }
    \item{aggr_settings}{\code{signature(object = "OPMS")}: ... }
    \item{aggregated}{\code{signature(object = "OPMS")}: ... }
    \item{anyDuplicated}{\code{signature(x = "OPMS")}: ... }
    \item{ci_plot}{\code{signature(object = "OPMS")}: ... }
    \item{coerce}{\code{signature(from = "OPMS", to = "list")}: ... }
    \item{coerce}{\code{signature(from = "list", to = "OPMS")}: ... }
    \item{collect_template}{\code{signature(object = "OPMS")}: ... }
    \item{contains}{\code{signature(object = "OPMS", other = "OPM")}: ... }
    \item{csv_data}{\code{signature(object = "OPMS")}: ... }
    \item{dim}{\code{signature(x = "OPMS")}: ... }
    \item{do_aggr}{\code{signature(object = "OPMS")}: ... }
    \item{duplicated}{\code{signature(x = "OPMS")}: ... }
    \item{extract}{\code{signature(object = "OPMS")}: ... }
    \item{extract_columns}{\code{signature(object = "OPMS")}: ... }
    \item{filename}{\code{signature(object = "OPMS")}: ... }
    \item{flatten}{\code{signature(object = "OPMS")}: ... }
    \item{gen_iii}{\code{signature(object = "OPMS")}: ... }
    \item{has_aggr}{\code{signature(object = "OPMS")}: ... }
    \item{heat_map}{\code{signature(object = "OPMS")}: ... }
    \item{hours}{\code{signature(object = "OPMS")}: ... }
    \item{include_metadata}{\code{signature(object = "OPMS")}: ... }
    \item{initialize}{\code{signature(.Object = "OPMS")}: ... }
    \item{length}{\code{signature(x = "OPMS")}: ... }
    \item{level_plot}{\code{signature(x = "OPMS")}: ... }
    \item{map_metadata}{\code{signature(object = "OPMS", mapping = "ANY")}: ... }
    \item{max}{\code{signature(x = "OPMS")}: ... }
    \item{measurements}{\code{signature(object = "OPMS")}: ... }
    \item{merge}{\code{signature(x = "OPMS", y = "missing")}: ... }
    \item{merge}{\code{signature(x = "OPMS", y = "numeric")}: ... }
    \item{metadata}{\code{signature(object = "OPMS")}: ... }
    \item{metadata<-}{\code{signature(object = "OPMS", key = "ANY", value = "ANY")}: ... }
    \item{metadata<-}{\code{signature(object = "OPMS", key = "ANY", value = "data.frame")}: ... }
    \item{metadata<-}{\code{signature(object = "OPMS", key = "character", value = "data.frame")}: ... }
    \item{metadata<-}{\code{signature(object = "OPMS", key = "factor", value = "data.frame")}: ... }
    \item{metadata<-}{\code{signature(object = "OPMS", key = "missing", value = "data.frame")}: ... }
    \item{metadata<-}{\code{signature(object = "OPMS", key = "missing", value = "formula")}: ... }
    \item{metadata<-}{\code{signature(object = "OPMS", key = "missing", value = "list")}: ... }
    \item{metadata_chars}{\code{signature(object = "OPMS")}: ... }
    \item{minmax}{\code{signature(x = "OPMS")}: ... }
    \item{oapply}{\code{signature(object = "OPMS")}: ... }
    \item{phylo_data}{\code{signature(object = "OPMS")}: ... }
    \item{plate_type}{\code{signature(object = "OPMS")}: ... }
    \item{plates}{\code{signature(object = "OPMS")}: ... }
    \item{position}{\code{signature(object = "OPMS")}: ... }
    \item{radial_plot}{\code{signature(object = "OPMS")}: ... }
    \item{rep}{\code{signature(x = "OPMS")}: ... }
    \item{rev}{\code{signature(x = "OPMS")}: ... }
    \item{select}{\code{signature(object = "OPMS")}: ... }
    \item{seq}{\code{signature(... = "OPMS")}: ... }
    \item{setup_time}{\code{signature(object = "OPMS")}: ... }
    \item{show}{\code{signature(object = "OPMS")}: ... }
    \item{sort}{\code{signature(x = "OPMS")}: ... }
    \item{summary}{\code{signature(object = "OPMS")}: ... }
    \item{thin_out}{\code{signature(object = "OPMS")}: ... }
    \item{well}{\code{signature(object = "OPMS")}: ... }
    \item{wells}{\code{signature(object = "OPMS")}: ... }
    \item{xy_plot}{\code{signature(x = "OPMS")}: ... }
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
showClass("OPMS")
}
\keyword{classes}
