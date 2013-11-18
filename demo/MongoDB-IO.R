### Analysing Phenotype MicroArray data: MongoDB I/O

# MongoDB is a popular document-oriented database. The RMongo package can be
# used in conjunction with opm to store and receive Phenotype MicroArray data
# in such a database. OPMX objects fit nicely to such a pattern because they
# do not impose a certain structure on the metadata, much like MongoDB is able
# to store any kinds of data structures. The same holds for the options parts
# of the aggr_settings and disc_settings entries of OPMX objects. So the only
# work that needs to be done is to convert between OPMX objects and JSON
# strings.
#
# See the documentation of MongoDB and RMongo for further details.
#
# Author: Markus Goeker


library(RMongo)
library(opm)


### Define helper function:

# In our case, RMongo unfortunately returns a data frame with one plate per
# row and unparsed JSON strings per field. But the following short function is
# able to convert all such results. Note that opms() also takes care of names
# converted by to_yaml(), if any (see below).
#
mongo2opm <- function(x) {
  x <- split(x, seq_len(nrow(x)))
  x <- rapply(x, rjson::fromJSON, "character", NULL, "replace")
  opms(x, precomputed = FALSE, skip = FALSE, group = TRUE)
}


### Create MongoDB connection:

# Connect to database 'test', which comes with MongoDB. We will use the
# collection 'pmdata' to store JSON representations of OPM objects.
#
conn <- mongoDbConnect("test", "localhost", 27017)
coll <- "pmdata"


### Insert plates:

# We insert each plate separately to be able to query them separately. Note that
# under these settings, to_yaml() takes care of removing names with dots, which
# are disallowed in MongoDB.
#
result <- vapply(plates(vaas_4), function(plate) {
  dbInsertDocument(conn, coll, to_yaml(plate, json = TRUE, nodots = TRUE))
}, "")
stopifnot(result == "ok")
print(result)


### Search for plates with E. coli as species entry in the metadata:

got <- dbGetQuery(conn, coll, '{"metadata.Species": "Escherichia coli"}')
stopifnot(is.data.frame(got), dim(got) > 0)


### Convert to list of OPMX objects:

# Conversion necessary. See comments to mongo2opm() above.
#
got <- mongo2opm(got)

# Yields list with one element per plate type (only one plate type here).
# Some checks:
#
stopifnot(is.list(got), length(got) == 1, names(got) == plate_type(vaas_4))
got <- got[[1]]
stopifnot(is(got, "OPMS"), has_disc(got), dim(got) == c(2, dim(vaas_4)[-1]))
print(got)


### Remove plates again:

result <- dbRemoveQuery(conn, coll,
  '{$and: [{"csv_data": {$exists: true}}, {"measurements": {$exists: true}}]}')
stopifnot(result == "ok")
print(result)


### Check whether some plates are left over:

empty <- dbGetQuery(conn, coll, '{"metadata.Species": "Escherichia coli"}')
stopifnot(is.data.frame(empty), dim(empty) == 0)


### Close connection:

dbDisconnect(conn)

