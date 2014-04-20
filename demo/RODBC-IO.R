#' # Analysing phenotype microarray data: database I/O with `RODBC`

#' This is example R code for using **opm** to store PM data in a database
#' accessible via `ODBC` and retrieving them again.
#'
#' This code can be used to check whether a database either found in an
#' environment variable or identical to the default value (see below) is
#' correctly set up for this purpose. The code also shows how to include a
#' user-defined selection of metadata.
#'
#' **Note**: The `ODBC` connection must be accordingly defined beforehand to
#' allow for the simple-minded connection attempt stated below. The database
#' tables must have been set up using the `SQL` that comes with **opm**.
#'
#' Author: *Markus Goeker*


library(opm)
library(RODBC)


#' This tries to get the `DSN` from the R or environment variable `RODBC_DSN`:

if (exists("RODBC_DSN")) {
  dsn <- RODBC_DSN
} else {
  dsn <- Sys.getenv("RODBC_DSN", "test_opm")
}

print(dsn)
conn <- odbcConnect(dsn)

#' Insertions via **RODBC** in this manner are slow. Creating subsets speeds
#' things up.
#'
result <- opm_dbcheck(conn, time.points = 1:5, wells = 12:14)

#' ### Check without metadata

print(opm_dbnext(2L, conn))

#' ### Check with metadata

if (all(result == "ok")) {

  # addition of metadata columns
  sqlQuery(conn,
    "ALTER TABLE plates ADD COLUMN strain text, ADD COLUMN replicate integer;")

  # check with metadata
  md <- data.frame(strain = c("X", "Y"), replicate = c(3L, 7L),
    stringsAsFactors = FALSE)
  result2 <- opm_dbcheck(conn, md, time.points = 1:5, wells = 12:14)

  # removal of metadata columns
  sqlQuery(conn,
    "ALTER TABLE plates DROP COLUMN strain, DROP COLUMN replicate;")

}


#' ### Tidying up


odbcClose(conn)

print(result)
stopifnot(result == "ok")
print(result2)
stopifnot(result2 == "ok")


detach("package:RODBC")
