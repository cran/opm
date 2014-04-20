#' # Analysing phenotype microarray data: database I/O with `PostgreSQL`

#' This is example R code for using **opm** to store PM data in a `PostgreSQL`
#' database and retrieving them again.
#'
#' This code can be used to check whether a database either found in an R or
#' environment variable or identical to the default value (see below) is
#' correctly set up for this purpose. The code also shows how to include a
#' user-defined selection of metadata.
#'
#' **Note**: The database must be accessible with `localhost` as server, the
#' current user as user and without a password, and the tables must have been
#' set up using the `SQL` that comes with **opm**.
#'
#' Author: *Markus Goeker*


library(opm)
library(RPostgreSQL)

#' This tries to get the name of the database file from the R or environment
#' variable *OPM_POSTGRESQL_DB*:

if (exists("OPM_POSTGRESQL_DB")) {
  dbname <- OPM_POSTGRESQL_DB
} else {
  dbname <- Sys.getenv("OPM_POSTGRESQL_DB", "pmdata")
}

print(dbname)
conn <- dbConnect("PostgreSQL", dbname = dbname)

#' ### Check without metadata

result <- opm_dbcheck(conn)

print(opm_dbnext(2L, conn))

#' ### Check with metadata

if (all(result == "ok")) {

  # addition of metadata columns
  dbGetQuery(conn,
    "ALTER TABLE plates ADD COLUMN strain text, ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md)

  # removal of metadata columns
  dbGetQuery(conn,
    "ALTER TABLE plates DROP COLUMN strain, DROP COLUMN replicate;")

}


#' ### Tidying up


dbDisconnect(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")


detach("package:RPostgreSQL", unload = TRUE)

