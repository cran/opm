\name{opm_dbput}
\alias{opm_dbcheck}
\alias{opm_dbclass}
\alias{opm_dbclear}
\alias{opm_dbfind}
\alias{opm_dbget}
\alias{opm_dbnext}
\alias{opm_dbput}
\title{Database I/O for \pkg{opm}}
\arguments{
  \item{object}{\code{\link{OPMX}}, \code{\link{MOPMX}} or
  \code{\link{OPM_DB}} object, integer vector containing
  real or potential primary keys of a database table, or
  character scalar containing a partial \acronym{SQL} query
  (the part after the \sQuote{WHERE} keyword).}

  \item{conn}{Database connection object. Currently
  \code{DBIConnection} objects from one of the reverse
  dependencies of \pkg{DBI} (recommended) and \code{RODBC}
  objects as created by the \pkg{RODBC} package are
  supported.}

  \item{map.tables}{Passed as \code{do_map} argument to
  \code{by} from the \pkg{pkgutils} package. Necessary if
  table names that deviate from the defaults are to be
  used.}

  \item{include}{Integer scalar indicating whether
  aggregated data (1) or aggregated and discretised data
  (2) or neither (0) should be added to the result. The
  numeric method of \code{opm_dbnext} needs the same kind
  of \code{object} argument.}

  \item{start}{Integer vector determining the minimum
  primary keys to which those in \code{object} should be
  coerced. Necessary for appending to a database table
  without overwriting previously inserted data.}

  \item{metadata}{Empty or data frame with metadata to be
  added to the check object \code{vaas_4}. If a data frame,
  it must contain exactly 2 rows.  Adding metadata makes
  only sense if according columns have been added to the
  database table for the plates; see the examples below.
  The original metadata from \code{vaas_4} are always
  removed.}

  \item{time.points}{Index of one to several time points.
  Selection speeds up database I/O during checking.}

  \item{wells}{Index of one to several wells. Selection
  speeds up database I/O during checking.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  The main functions are those for create, search, read and
  delete operations:\itemize{ \item{\code{opm_dbput}
  returns an integer vector containing the primary keys of
  the inserted plates.} \item{\code{opm_dbfind} returns an
  integer vector containing the primary keys of the found
  plates.} \item{\code{opm_dbget} returns a
  \code{\link{MOPMX}} object with one element per plate
  type.} \item{\code{opm_dbclear} invisibly returns the
  result of \code{dbGetQuery} (which is usually
  \code{NULL}).} } Regarding the helper functions,
  \code{opm_dbnext} returns an integer scalar that is
  suitable as \code{start} argument of \code{opm_dbput},
  whereas \code{opm_dbclass} returns a character scalar
  with the name of the intermediary class (derived from
  \code{\link{OPM_DB}}) to be created for database I/O.
  These need not normally be called by an \pkg{opm} user.

  For checking whether a database (connection) is correctly
  set up, \code{opm_dbcheck} is available, which returns a
  character vector whose elements are either \kbd{ok} or a
  description of the error that has occurred at that step
  of the checking process.
}
\description{
  Methods for inserting, querying and deleting
  \code{\link{OPMX}} objects into or from
  (\acronym{SQL}-based) relational databases. A common
  database scheme is assumed as defined in the auxiliary
  \acronym{SQL} files of this package (run
  \code{\link{opm_files}} in \code{"sql"} mode), but tables
  could be named differently, and columns could be added
  containing user-defined combinations of metadata.
}
\details{
  The \code{DBIConnection} methods send table and column
  names are through \code{make.db.names} from the \pkg{DBI}
  package or its dependencies before including them into
  \acronym{SQL} queries, if any. As dictated by \code{by}
  from the \pkg{pkgutils} packages, this is done after
  applying \code{map.tables}. The \code{RODBC} methods use
  a simple quoting scheme.

  \code{opm_dbcheck} attempts to insert, query and delete
  the first two plates from the object \code{vaas_4} into
  the database. If everything is correctly set up, this
  should work without error \strong{unless} these two
  plates from \code{vaas_4} have already been inserted. If
  errors occur, it is up to the user to clean up the data
  base (as far as necessary).

  Note that the deletion mechanism is based on \code{ON
  DELETE CASCADE}. To enable this in \code{SQLite},
  \code{PRAGMA foreign_keys = ON;} has to be called each
  time a database is opened. See the according \code{demo}
  entry.
}
\examples{
# The SQL files for generating the expected database tables. Tables can
# be renamed, but then an according 'map.tables' argument must be used.
opm_files("sql")

# Usage examples are given in these demos. An according database must be
# made accessible beforehand.
if (interactive())
  demo(package = "opm")
}
\seealso{
  DBI::make.db.names pkgutils::by
}
\keyword{database}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{opm_dbcheck-methods}
\alias{opm_dbcheck,ANY-method}
\alias{opm_dbclass-methods}
\alias{opm_dbclass,MOPMX-method}
\alias{opm_dbclass,OPM-method}
\alias{opm_dbclass,OPMS-method}
\alias{opm_dbclass,numeric-method}
\alias{opm_dbclear-methods}
\alias{opm_dbclear,character,ANY-method}
\alias{opm_dbclear,character-method}
\alias{opm_dbclear,integer,DBIConnection-method}
\alias{opm_dbclear,integer,RODBC-method}
\alias{opm_dbfind-methods}
\alias{opm_dbfind,character,DBIConnection-method}
\alias{opm_dbfind,character,RODBC-method}
\alias{opm_dbget-methods}
\alias{opm_dbget,character,ANY-method}
\alias{opm_dbget,character-method}
\alias{opm_dbget,integer,DBIConnection-method}
\alias{opm_dbget,integer,RODBC-method}
\alias{opm_dbnext-methods}
\alias{opm_dbnext,ANY,ANY-method}
\alias{opm_dbnext,ANY-method}
\alias{opm_dbnext,OPM_DB,DBIConnection-method}
\alias{opm_dbnext,OPM_DB,RODBC-method}
\alias{opm_dbput-methods}
\alias{opm_dbput,ANY,ANY-method}
\alias{opm_dbput,ANY-method}
\alias{opm_dbput,OPM_DB,DBIConnection-method}
\alias{opm_dbput,OPM_DB,RODBC-method}
\usage{
  \S4method{opm_dbcheck}{ANY}(conn, metadata = NULL,
    time.points = TRUE, wells = TRUE) 

  \S4method{opm_dbclass}{MOPMX}(object) 
  \S4method{opm_dbclass}{OPM}(object) 
  \S4method{opm_dbclass}{OPMS}(object) 
  \S4method{opm_dbclass}{numeric}(object) 

  \S4method{opm_dbclear}{character,ANY}(object, conn,
    map.tables = NULL) 
  \S4method{opm_dbclear}{integer,DBIConnection}(object, conn,
    map.tables = NULL) 
  \S4method{opm_dbclear}{integer,RODBC}(object, conn,
    map.tables = NULL) 

  \S4method{opm_dbfind}{character,DBIConnection}(object, conn,
    map.tables = NULL) 
  \S4method{opm_dbfind}{character,RODBC}(object, conn,
    map.tables = NULL) 

  \S4method{opm_dbget}{character,ANY}(object, conn,
    map.tables = NULL, include = 2L) 
  \S4method{opm_dbget}{integer,DBIConnection}(object, conn,
    map.tables = NULL, include = 2L) 
  \S4method{opm_dbget}{integer,RODBC}(object, conn,
    map.tables = NULL, include = 2L) 

  \S4method{opm_dbnext}{ANY,ANY}(object, conn,
    map.tables = NULL) 
  \S4method{opm_dbnext}{OPM_DB,DBIConnection}(object, conn,
    map.tables = NULL) 
  \S4method{opm_dbnext}{OPM_DB,RODBC}(object, conn,
    map.tables = NULL) 

  \S4method{opm_dbput}{ANY,ANY}(object, conn, ...) 
  \S4method{opm_dbput}{OPM_DB,DBIConnection}(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) 
  \S4method{opm_dbput}{OPM_DB,RODBC}(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) 
}
%% END INSERTION BY repair_S4_docu.rb
