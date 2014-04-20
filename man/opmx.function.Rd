\name{opmx}
\alias{opmx}
\title{Convert user-defined objects to \acronym{OPMX}}
\arguments{
  \item{object}{Data frame containing numeric data with
  growth or respiration measurements and optional or
  mandatory additional columns, depending on the
  \code{format} argument.}

  \item{format}{Character scalar indicating the data layout
  within \code{object}. See below for examples. In brief,
  the formats are: \describe{ \item{horizontal}{One row per
  well, with additional columns providing the substrate
  names, metadata that identify the plate, and optionally
  other columns to be used as \code{\link{metadata}} or
  \code{\link{csv_data}} entries. The time points must be
  given in columns that can be identified with a certain
  prefix. The part after the prefix must be convertible to
  numeric data (the time points, ideally given in hours).
  Several plates and even several plate types can be
  contained within \code{object}.}
  \item{rectangular}{Several rows and columns per time
  point, yielding a set of rectangles per plate. Only a
  single plate, and only measurements are contained within
  \code{object}, thus some of the other arguments cannot be
  empty. See the \code{interval} argument for setting time
  points.} \item{vertical}{One column per well. Only a
  single plate, and only measurements and time points are
  contained within \code{object}, thus some of the other
  arguments cannot be empty. The time points are either
  contained as column or can be read from the row names.
  Ideally, they are given in hours.If \code{object} has no
  column names, the first column or the row names yield the
  time points.} }}

  \item{position}{Character vector. In \sQuote{horizontal}
  format, the name of one to several columns to be joined
  yielding \sQuote{position} indicators.  These will be
  used to uniquely identify each plate. The columns to be
  joined will be kept, too; usually they will end up in the
  \code{\link{metadata}}. In this case, the resulting
  \sQuote{position} indicators are newly generated rather
  than literally taken from the input, but yield the same
  grouping. An empty \code{position} argument is possible,
  but then an accordingly named column must already be
  present, whose content is used literally.

  The \code{position} argument is mandatory for the
  \sQuote{rectangular} and \sQuote{vertical} formats. It
  should be chosen so as to identify the resulting
  \code{\link{OPM}} object again once it is combined with
  others into an \code{\link{OPMS}} object. (By default the
  setup time is additionally considered but the default for
  the \code{setup.time} argument is just the time of the
  call to \code{opmx}.)

  For plate position values to be used literally, integers
  between 0 and 99 (inclusively) followed by a single
  letters are recommended, because this allows \pkg{opm} to
  normalise this entry. See the eponymous argument of
  \code{\link{csv_data}}.}

  \item{plate.type}{Character scalar. In
  \sQuote{horizontal} mode, the name of the column
  containing the plate-type indicators. After
  normalisation, these will be used for storing the mapping
  of well coordinates. The argument is ignored if empty.
  But then an accordingly named column must already be
  present \strong{OR} \code{full.name} must contain a
  single named element, whose name is then inserted as
  plate name.

  The \code{plate.type} argument is mandatory for the
  \sQuote{rectangular} and \code{vertical} formats. An
  according plate type must already have been stored using
  \code{\link{register_plate}} and contain the well
  coordinates found in \code{object}. Normalisation of the
  plate-type name is done, however.

  In \sQuote{horizontal} mode, the plate type can be
  registered beforehand, too, which is useful to enforce a
  certain ordering of wells. But then the registered
  well-coordinate map must contains all well coordinates
  found in \code{object}.}

  \item{well}{Character scalar. In \sQuote{horizontal}
  format, the name of the column containing the well
  indicators. These should be substrate names; an according
  mapping from (newly assigned) well coordinates to these
  substrate names will then be stored using
  \code{\link{register_plate}} if it is not yet present.
  Ignored if empty (but then an accordingly named column
  must be present).}

  \item{prefix}{Character scalar. In \sQuote{horizontal}
  format, used for identifying the measurements columns.}

  \item{sep}{Character scalar. In \sQuote{rectangular}
  format, used for identifying the rows and column with
  time-points and well coordinates.}

  \item{full.name}{Named character vector indicating the
  full plate names. Ignored if empty. Names should be names
  of the plate types found within \code{object}, if any,
  but normalisation will be done. Values should be the
  respective full names. Missing ones are silently ignored.

  If the plate type is not found within \code{object}, then
  it is taken from the name of \code{full.name}, assuming a
  uniform plate type throughout \code{object}. In that
  case, \code{full.name} must contain only a single element
  (and a single name).}

  \item{setup.time}{Character scalar to be inserted if
  missing in the data.  Like the next argument, the value
  goes into the \code{\link{csv_data}}.}

  \item{filename}{Character scalar to be inserted if
  missing in the data.}

  \item{interval}{Numeric vector. If of length one,
  indicating the time interval between measurements in the
  \sQuote{rectangular} format. If the length corresponds to
  the number of measurements per well in \code{object}, it
  is interpreted directly as the time points. This is
  useful if the intervals are non-unique. Ignored if empty,
  causing \code{0, 1, 2, ...} to be used as time points.
  (This is often acceptable as it only causes a different
  scaling; it is not acceptable if the time points were not
  in regular intervals.)

  In the case of the \code{vertical} format, a non-empty
  \code{interval} value causes the time points to not be
  extracted from \code{object} but constructed from
  \code{interval}. Ideally, \code{interval} is given in
  hours (because this corresponds to the default axis
  labelling of some plotting functions).}
}
\value{
  \code{\link{OPMX}} or \code{\link{MOPMX}} object or
  \code{NULL}, depending on how many distinct plate types
  are encountered within \code{object}.
}
\description{
  Convert data frames with user-defined plate types to
  \code{\link{OPMX}} or \code{\link{MOPMX}} objects.
}
\details{
  The main purpose of this function is to convert objects
  that hold non-\acronym{PM} data to \code{\link{OPMX}}
  objects that can be analysed with \pkg{opm}. The
  mechanism for dealing with user-defined plate types is
  implemented in \code{\link{register_plate}}, whereas
  \code{opmx} also takes care of the necessary changes in
  format and naming for converting a data frame to a
  \code{\link{MOPMX}} object.
}
\examples{
## 'horizontal' input format

# fake data frame containing growth or respiration measurements
x <- data.frame(
  Treatment = c(rep("Control", 4), rep("Heat stress", 4)),
  Strain = paste0("X", c(1, 2, 2, 1, 2, 1, 1, 2)),
  Substrate = c(rep("Glucose", 2), rep("Galactose", 4), rep("Glucose", 2)),
  T_0 = c(12, 5, 8, 6, 8, 9, 7, 10),
  T_5 = c(23, 7, 7, 18, 30, 10, 8, 9),
  T_10 = c(79, 9, 10, 64, 67, 8, 6, 11),
  T_15 = c(103, 8, 46, 99, 101, 17, 9, 8),
  T_20 = c(105, 9, 77, 112, 103, 44, 8, 12)
)

# The plate type is not contained and thus taken from 'full.name', but
# the wells are obviously within 'Substrate', and each combination of
# 'Treatment' and 'Strain' is apparently one group of measurements
# (interpreted as 'plate').
y <- opmx(x, well = "Substrate", position = c("Treatment", "Strain"),
  full.name = c(sugars = "Fake sugar test plate"))

# This yields a single OPMX object as there is only one plate type.
stopifnot(is(y, "OPMX"), dim(y) == c(4, 5, 2))
print(xy_plot(y, include = list("Strain", "Treatment"),
  theor.max = FALSE, main = list(in.parens = FALSE), ylab = "Hours"))

## 'rectangular' input format

# Get the input file. The rectangular format is hardly suitable for R
# but produced by plate readers such as those distributed by the TECAN
# company.
growth.data.file <- grep("tecan", opm_files("growth"), ignore.case = TRUE,
  value = TRUE)

if (length(growth.data.file)) { # if the file was found

  x <- read.table(growth.data.file)
  head(x)

  # Creating a fake well map. For really making sense of these data, one
  # would need to know the real substrate names.
  well.map <- rbind(1:6, 1:6, 1:6, 1:6)
  well.map[] <- paste0("Substrate ", LETTERS[1:4], well.map)

  # Registering the plate type beforehand is mandatory here because the
  # file does not contain the real substrate names.
  register_plate(XYZ = well.map)
  (y <- opmx(x, "rectangular", plate.type = "XYZ", position = 1,
    interval = 0.25))
  plate_type(y) # => a custom (user-defined) plate
  stopifnot(setequal(wells(y, full = TRUE, in.parens = FALSE), well.map))

  register_plate(XYZ = NULL) # tidying up

} else {
  warning("file with growth data not found")
}

## 'vertical' input format

# Fake data frame. It is safer to set all column names explicitly.
# If none are there, the first column yields the time points unless
# there are explicitly set row names.
x <- data.frame(
  c(0, 5, 10, 15, 20),
  c(12, 23, 79, 103, 105),
  c(5, 7, 9, 8, 9),
  c(8, 7, 10, 46, 77),
  c(6, 18, 64, 99, 112),
  c(8, 30, 67, 101, 103),
  c(9, 10, 8, 17, 44),
  c(7, 8, 6, 9, 8),
  c(10, 9, 11, 8, 12)
)
colnames(x) <- NULL # necessary for this example

# Creating a fake well map for the fake data frame.
well.map <- paste("Substrate", 1:8)
names(well.map) <- paste0("A", 1:8)

# Registering the plate type beforehand is mandatory here because the
# input data frame does not contain the real substrate names.
register_plate(XYZ = well.map)
wells(plate = "CUSTOM:XYZ")[1:10]

(y <- opmx(x, "vertical", plate.type = "XYZ", position = 1))

plate_type(y) # => a custom (user-defined) plate
stopifnot(setequal(wells(y, full = TRUE, in.parens = FALSE), well.map))

register_plate(XYZ = NULL) # tidying up
}
\seealso{
  Other conversion-functions: \code{\link{as.data.frame}},
  \code{\link{extract}}, \code{\link{extract_columns}},
  \code{\link{flatten}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{split}}, \code{\link{to_yaml}},
  \code{\link{unique}}
}
\keyword{manip}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{opmx-methods}
\alias{opmx,data.frame-method}
\usage{
  \S4method{opmx}{data.frame}(object,
    format = c("horizontal", "rectangular", "vertical"), plate.type = NULL,
    position = NULL, well = NULL, prefix = "T_", sep = "<>", full.name = NULL,
    setup.time = date(), filename = "", interval = NULL) 
}
%% END INSERTION BY repair_S4_docu.rb
