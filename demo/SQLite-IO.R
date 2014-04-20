#' # Analysing phenotype microarray data: database I/O with `SQLite`
#'
#' This is example R code for using **opm** to store PM data in an `SQLite`
#' database and retrieving them again.
#'
#' This code can be used to check whether a database either found in an R or
#' environment variable or identical to the default value (see below) is
#' correctly set up for this purpose. The code also shows how to include a
#' user-defined selection of metadata.
#'
#' **Note**: The database file must be accessible with the current user as user
#' and without a password, and the tables must have been set up using the `SQL`
#' that comes with **opm**.
#'
#' Author: *Markus Goeker*


library(opm)
library(RSQLite)


#' This tries to get the name of the database file from the R or environment
#' variable *OPM_SQLITE_DB*:

if (exists("OPM_SQLITE_DB")) {
  dbname <- OPM_SQLITE_DB
} else {
  dbname <- Sys.getenv("OPM_SQLITE_DB", file.path("misc", "pmdata.db"))
}

print(dbname)
conn <- dbConnect("SQLite", dbname = dbname)

#' Next comes an `SQLite`-specific command necessary to enable the deletion
#' mechanism. Must be called each time the database is opened:
#'
dbGetQuery(conn, "PRAGMA foreign_keys = ON;")

#' ### Check without metadata

result <- opm_dbcheck(conn)

print(opm_dbnext(2L, conn))

#' ### Check with metadata

if (all(result == "ok")) {

  # addition of metadata columns
  dbGetQuery(conn, "ALTER TABLE plates ADD COLUMN strain text;")
  dbGetQuery(conn, "ALTER TABLE plates ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md)

  # note: removal of metadata columns is impossible with `SQLite` 3.7.9

}


#' ### Tidying up


dbDisconnect(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")


detach("package:RSQLite")

