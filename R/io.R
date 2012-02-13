

################################################################################
#
# IO helpers
#


## NOTE: not an S4 method because check is done using match.arg()

#' File pattern
#'
#' Create regexp matching certain file extensions. This is not normally
#' directly called by an \pkg{opm} user but by the other IO functions of the
#' package.
#'
#' @param type Character scalar indicating the file types to be matched by 
#'   extension.
#' @param compressed Logical scalar. Shall compressed files also be matched?
#' @export
#' @return Character scalar, holding a regular expression.
#' @family IO-functions
#' @keywords utilities
#' @seealso file_ext
#' @examples
#' (x <- file_pattern())
#' (y <- file_pattern(type = "csv", compressed = FALSE))
#' stopifnot(nchar(x) > nchar(y))
#'
file_pattern <- function(type = c("both", "csv", "yaml", "any", "empty"),
    compressed = TRUE) {
  result <- sprintf("\\.%s", switch(match.arg(type),
    both = "(csv|ya?ml)",
    csv = "csv",
    yaml = "ya?ml",
    any = "[^.]+",
    empty = "",
    stop(BUG_MSG)
  ))
  if (compressed)
    result <- sprintf("%s(\\.(bz2|gz|lzma|xz))?", result)
  sprintf("%s$", result)
}


## NOTE: not an S4 method because dispatch is done manually

#' File pattern
#'
#' Create regexp matching certain file extensions.
#'
#' @param arg If a list, call \code{\link{file_pattern}} with it as arguments.
#'   If \code{NULL}, return it unchanged. Otherwise convert it to character and 
#'   optionally using \code{\link{glob_to_regex}}, depending on \code{wildcard}.
#' @param wildcard Logical scalar. Convert \code{arg} using 
#'   \code{\link{glob_to_regex}}?
#' @return Character scalar, holding a regular expression, or any object, 
#'   unchanged.
#' @keywords internal
#' @seealso file_ext
#'
extended_file_pattern <- function(arg, wildcard) {
  if (is.null(arg))
    return(arg)
  if (is.list(arg))
    return(do.call(file_pattern, arg))
  result <- as.character(arg)
  if (wildcard)
    result <- glob_to_regex(result)
  result
}


################################################################################
#
# Input of single OPM files
#


## NOTE: not an S4 method because scan() gives an adequate error message

#' Read old-style OmniLog CSV file
#'
#' Read OmniLog(R) data from single pseudo-CSV file in the old(pre-2011)
#' format.
#'
#' @param filename Character scalar with the obvious meaning.
#' @return \code{\link{OPM}} object.
#' @references \url{http://www.biolog.com/}
#' @keywords internal
#'
read_old_opm <- function(filename) {
  
  fold_comments <- function(x) {
    dlen <- length(data <- x[c(FALSE, TRUE)])
    llen <- length(labels <- x[c(TRUE, FALSE)])
    if (dlen != llen)
      warning("odd number of comment fields -- you might experience problems")
    pos <- seq.int(1L, min(dlen, llen))
    structure(.Data = data[pos], names = labels[pos])
  }
  
  # Read data and determine HOUR field in proper CSV header
  data <- scan(file = filename, sep = ",", what = "character", quiet = TRUE,
    comment.char = "#", strip.white = TRUE)
  pos <- which(data == HOUR)
  if (length(pos) != 1L)
    stop("uninterpretable header (maybe because there is not 1 plate per file)")
  pos <- seq(pos - 1L)
  
  # Process comments
  comments <- fold_comments(c(FILE, filename, data[pos]))
  names(comments)[names(comments) == "Set up Time"] <- SETUP
  comments[PLATE_TYPE == "PM 1-"] <- "PM01"

  # Process data
  data <- data[-pos]
  data <- data[nzchar(data)]
  ncol <- 97L
  if (length(data) %% ncol != 0L)
    stop("wrong number of fields")
  data <- matrix(as.numeric(data[-seq(ncol)]), ncol = ncol, byrow = TRUE,
    dimnames = list(NULL, data[seq(ncol)]))

  new(OPM, measurements = data, metadata = list(), csv_data = comments)
}


## NOTE: not an S4 method because scan() gives an adequate error message

#' Read OmniLog CSV file
#'
#' Read OmniLog(R) data from single CSV file in novel (2011) format.
#'
#' @param filename Character scalar with the obvious meaning.
#' @return \code{\link{OPM}} object.
#' @references \url{http://www.biolog.com/}
#' @keywords internal
#'
read_new_opm <- function(filename) {
  data <- scan(file = filename, sep = ",", what = "character", quiet = TRUE,
    comment.char = "#", strip.white = TRUE)
  pos <- which(data == HOUR)
  if (length(pos) != 1L)
    stop("uninterpretable header (maybe because there is not 1 plate per file)")
  ncol <- pos + 96L
  if (length(data) %% ncol != 0L)
    stop("wrong number of fields")
  data <- matrix(data[-seq(ncol)], ncol = ncol, byrow = TRUE,
    dimnames = list(NULL, data[seq(ncol)]))
  pos <- seq(pos - 1L)
  comments <- structure(.Data = c(filename, data[1L, pos]),
    names = c(FILE, colnames(data)[pos]))
  data <- apply(data[, -pos], MARGIN = 2L, FUN = as.numeric)
  new(OPM, measurements = data, metadata = list(), csv_data = comments)
}


## NOTE: not an S4 method because yaml.load_file() gives an adequate error 
##   message

#' Read opm YAML file
#'
#' Read \pkg{opm} data from a single YAML file. Arbitrary nesting levels are
#' allowed, i.e. the YAML input results in either a list representing the slots
#' of an \code{\link{OPM}} or \code{\link{OPMA}} object, or a list whose
#' direct or indirect sublists represent such an object. However, it is an
#' error if non-list elements are encountered when traversing such lists, and
#' if no \code{\link{OPM}} or \code{\link{OPMA}} objects can be found at all.
#'
#' @param filename Character scalar with the obvious meaning.
#' @return \code{\link{OPM}}, \code{\link{OPMA}} object, depending on the
#'   presence of aggregated data.
#' @references \url{http://www.yaml.org/}
#' @keywords internal
#'
read_opm_yaml <- function(filename) {
  if (!require("yaml", quietly = TRUE, warn.conflicts = FALSE))
    stop("the 'yaml' library is needed for inputting opm YAML files")
  result <- yaml::yaml.load_file(filename)
  result <- to_opm_list(result, precomputed = FALSE, skip = FALSE)
  if (!length(result))
    stop("YAML file contained no interpretable data")
  result
}


## NOTE: not an S4 method because conversion is done

#' Read single PM file
#'
#' Read single OmniLog(R) or \pkg{opm} data file in either new- or old-style
#' CSV or YAML format. Files compressed using gzip, bzip2 or lzma/xz are also 
#' understood.
#'
#' @param filename Character scalar, or convertible to such, with the obvious 
#'   meaning.
#' @export
#' @return \code{\link{OPM}} object. In the case of YAML input, this might also
#'   be an \code{\link{OPMA}} object or a list of such objects, but \strong{not}
#'   an \code{\link{OPMS}} object.
#' @family IO-functions
#' @note \itemize{
#'   \item The expected CSV format is what is output by the OmniLog(R) device, 
#'     one plate per file. Other formats, or OmniLog(R) files resaved with 
#'     distinct CSV settings, are not understood. For this reason, if any 
#'     editing of the files was necessary at all, it is advisable to do this in
#'     an editor for plain text, not in a spreadsheet program. 
#'   \item It is impossible to read CSV files that contain more than one plate.
#'     For splitting old-style and new-style CSV files into one file per plate,
#'     see the example under \code{\link{split_files}}.
#'   \item In contrast, input YAML files can contain data from more than one 
#'     plate.
#' }
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso read.csv
#' @keywords IO
#' @examples
#' test.files <- opm_files("testdata")
#' if (length(test.files) > 0) { # if the folder is found
#'   x <- read_single_opm(test.files[1])
#'   class(x)
#'   dim(x)
#'   summary(x)
#'   stopifnot(inherits(x, "OPM"), identical(dim(x), c(384L, 96L)))
#' }
#' # this can be repeated for the other input test files
#'
read_single_opm <- function(filename) {
  assert_length(filename)
  filename <- as.character(filename)
  if (!file.exists(filename))
    stop(sprintf("file '%s' does not exist", filename))
  e <- function(error) error$message
  errs <- list()
  result <- tryCatch(read_new_opm(filename), error = e)
  if (is.character(result)) {
    errs$`New CSV` <- result
    result <- tryCatch(read_old_opm(filename), error = e)
  }
  if (is.character(result)) {
    errs$`Old CSV` <- result
    result <- tryCatch(read_opm_yaml(filename), error = e)
  }
  if (is.character(result)) {
    errs$YAML <- result
    names(errs) <- paste(names(errs), "error")
    errs$Filename <- filename
    stop(listing(errs, header = "Unknown file format:"))
  }
  result
}


################################################################################
#
# Input of multiple OPM files
#


## NOTE: not an S4 method because conversion is done

#' Conversion of directory names to file names
#'
#' Turn a mixed file/directory list into a list of files. This is not normally
#' directly called by an \pkg{opm} user but by the other IO functions of the 
#' package. One can use their \code{demo} argument directly for testing the 
#' results of the applied filename patterns.
#'
#' @param names Character vector containing filenames or directories, or 
#'   convertible to such.
#'
#' @param include If a character scalar, used as regular expression or wildcard
#'   (see the \code{wildcard} argument) for selecting from the input files. If
#'   \code{NULL}, ignored. If a list, used as arguments of
#'   \code{\link{file_pattern}} and its result used as regular expression. Note
#'   that selection is done \strong{after} expanding the directory names to 
#'   filenames.
#' @param exclude Like \code{include}, but for excluding matching input files.
#'   Note that exclusion is done \strong{after} applying \code{include}.
#'
#' @param ignore.case Logical scalar. Ignore differences between uppercase and
#'   lowercase when using \code{include} and \code{exclude}? Has no effect for
#'   \code{NULL} values for \code{include} or \code{exclude}, respectively.
#' @param wildcard Logical scalar. Are \code{include} and \code{exclude}
#'   wildcards (as used by UNIX shells) that first need to be concerted to
#'   regular expressions? Has no effect if lists are used for \code{include} or 
#'   \code{exclude}, respectively. See \code{\link{glob_to_regex}} for details
#'   on such wildcards (a.k.a. globbing patterns).
#'
#' @param recursive Logical scalar. Traverse directories recursively and also
#'   consider all subdirectories? See \code{list.files} from the \pkg{base}
#'   package for details.
#' @param missing.error Logical scalar. If a file/directory does not exist,
#'   raise an error or only a warning?
#'
#' @param remove.dups Logical scalar. Remove duplicates from \code{names}? Note
#'   that if requested this is done \strong{before} expanding the names of
#'   directories, if any.
#'
#' @return Character vector (which would be empty if all existing files, if any,
#'   had been unselected).
#'
#' @note Other functions that call this function should have a \code{demo}
#'   argument which, if set to \code{TRUE}, caused the respective function to
#'   do no real work but print the names of the files that it would process in
#'   normal running mode.
#'
#' @export
#' @seealso list.files Sys.glob
#' @family IO-functions
#' @keywords IO character
#' @examples
#'
#' # Example with temporary directory
#' td <- tempdir()
#' tf <- tempfile()
#' (x <- explode_dir(td))
#' write(letters, tf)
#' (y <- explode_dir(td))
#' stopifnot(length(y) > length(x))
#' unlink(tf)
#' (y <- explode_dir(td))
#' stopifnot(length(y) == length(x))
#'
#' # Example with R installation directory
#' (x <- explode_dir(R.home(), include = "*/doc/html/*"))
#' (y <- explode_dir(R.home(), include = "*/doc/html/*", exclude = "*.html"))
#' stopifnot(length(x) == 0L || length(x) > length(y))
#'
#' # More interesting use cases are provided by the functions that call
#' # explode_dir(). Consider to first try them in 'demo' mode. Globbing
#' # examples are given under glob_to_regex().
#'
explode_dir <- function(names,
    include = NULL, exclude = NULL, ignore.case = TRUE, wildcard = TRUE,
    recursive = TRUE, missing.error = TRUE, remove.dups = TRUE) {
  explode_names <- function(names, recursive) {
    is.dir <- file.info(names)$isdir
    if (any(no.info <- is.na(is.dir))) {
      msg <- sprintf("File or directory not found: '%s'", 
        paste(names[no.info], collapse = " "))
      if (missing.error)
        stop(msg)
      else
        warning(msg)
    }
    is.dir <- is.dir[!no.info]
    names <- as.list(names[!no.info]) # use of a list ensures input order
    names[is.dir] <- lapply(names[is.dir], FUN = list.files, full.names = TRUE, 
      recursive = recursive)
    unlist(names)
  }
  select_files <- function(data, pattern, invert) {
    if (is.null(pattern))
      return(data)
    pattern <- extended_file_pattern(pattern, wildcard)
    grep(pattern, data, ignore.case = ignore.case, value = TRUE,
      invert = invert)
  }
  names <- as.character(names)
  if (remove.dups)
    names <- unique(names)
  result <- explode_names(names, recursive = recursive)
  result <- select_files(result, include, invert = FALSE)
  select_files(result, exclude, invert = TRUE)
}


#' Opm files
#'
#' Get list of files from the \pkg{opm} package of interest for the user.
#'
#' @param what Character scalar indicating the subdirectory to search in.
#'   Currently the following ones are included:
#'   \describe{
#'     \item{scripts}{R script files for non-interactive uses of the 
#'        \pkg{opm} package, particularly for the batch processing of many
#'        files. When called withput input arguments or with the
#'        \sQuote{-h} switch, the scripts output usage information.}
#'     \item{testdata}{Files as output by the OmniLog(R) device for testing
#'        data input and metadata management.}
#'   }
#' @export
#' @return Character vector of filenames.
#' @note This might fail with unusual installations of the \pkg{opm} package.
#' @family IO-functions
#' @seealso list.files
#' @keywords utilities
#' @examples
#' (x <- opm_files("scripts"))
#' (y <- opm_files("testdata"))
#' if (length(y) > 0) {
#'   stopifnot(grepl("\\.R$", x, ignore.case = TRUE))
#'   stopifnot(!grepl("\\.R$", y, ignore.case = TRUE))
#' }
#'
#' # On UNIX systems you should be able to do this if Rscript and the optparse
#' # package are properly installed:
#' # invisible(sapply(opm_files(), function(f) system(paste("Rscript", f))))
#' # ...and get the usage messages of all scripts.
#'
opm_files <- function(what = c("scripts", "testdata")) {
  pattern <- file.path("*", "opm", match.arg(what), "*")
  explode_dir(.libPaths(), include = pattern)
}


## Not an S4 method because conversion is done

#' Read multiple PM files at once
#'
#' Read OmniLog(R) or \pkg{opm} data file(s) in one of three possible formats:
#' either new- or old-style OmniLog(R) CSV or \pkg{opm} YAML format. Files 
#' compressed using gzip, bzip2 or lzma/xz are also understood (but may be
#' excluded using \code{include} and/or \code{exclude}).
#'
#' @param names Character vector with names of files in one of three formats
#'   accepted by \code{read_opm} ((see above), or names of directories 
#'   containing such files, or both; or convertible to such a vector. See the
#'   \code{include} argument below and \code{\link{explode_dir}} for how to 
#'   select subsets from the input files or directories.
#'
#' @param convert Character scalar. If \sQuote{no}, always return a list. If
#'   \sQuote{yes}, convert to \code{NULL}, \code{\link{OPM}} object, or OPMS
#'   object, depending on the number of files read (0, 1, or more). \sQuote{try}
#'   behaves like \sQuote{yes} but does not result in an error message if
#'   conversion to OPMS is impossible; a list is returned in that case.
#' @param gen.iii Logical scalar. If \code{TRUE}, invoke \code{\link{gen_iii}} 
#'   on each plate.
#'
#' @param include Pattern for selecting from the input files. The default value
#'   results in the output of \code{\link{file_pattern}}, which should be
#'   sufficient in most cases. See \code{\link{explode_dir}} for details on
#'   other possibilities.
#' @param ... Optional further arguments passed to \code{\link{explode_dir}}.
#'
#' @param demo Logical scalar. Do not read files, but print a vector with the
#'   names of the files that would be (attempted to) read, and return them
#'   invisibly?
#'
#' @return \code{\link{OPM}} object (maybe \code{\link{OPMA}} in case of YAML
#'   input) or \code{\link{OPMS}} object. If \code{demo} is \code{TRUE}, a
#'   character vector instead.
#'
#' @note Regarding the CSV format, see the remark to 
#'   \code{\link{read_single_opm}}.
#'
#' @export
#' @family IO-functions
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso read.csv yaml::yaml.load_file
#' @keywords IO
#' @examples
#' test.files <- grep("Multiple", opm_files("testdata"), invert = TRUE, 
#'   value = TRUE, fixed = TRUE)
#' if (length(test.files) > 0L) { # if the folder is found
#'   x <- read_opm(test.files, demo = TRUE) # check first what you would get
#'   stopifnot(identical(test.files, x))
#'   x <- read_opm(test.files[1:2]) # these two have the same plate type
#'   class(x)
#'   dim(x)
#'   summary(x)
#'   stopifnot(inherits(x, "OPMS"), identical(dim(x), c(2L, 384L, 96L)))
#' }
#' # This can be repeated for the other input test files. Instead of a several
#' # file names one can also provide a single one, one to several directory 
#' # names, or mixture of file and directory names.
#'
#' \dontrun{
#'
#' # Reading all files from the current working directory is also easy:
#' x <- read_opm(getwd())
#' # or
#' x <- read_opm(".")  
#' }
#'
read_opm <- function(names, convert = c("try", "no", "yes"), gen.iii = FALSE,
    include = list(), ..., demo = FALSE) {
  convert <- match.arg(convert)
  names <- explode_dir(names = names, include = include, ...)
  if (demo) {
    message(paste(names, collapse = "\n"))
    return(invisible(names))
  }
  # The c() call is necessary to flatten lists from YAML input.
  result <- c(lapply(names, read_single_opm), recursive = TRUE)
  if (gen.iii)
    result <- lapply(result, gen_iii)
  if (length(result) == 0L)
    switch(convert, no = result, NULL)
  else if (length(result) == 1L)
    switch(convert, no = result, result[[1L]])
  else
    switch(convert,
      no = result,
      yes = new(OPMS, plates = result),
      try = tryCatch(new(OPMS, plates = result), error = function(e) {
        warning("the data from distinct files could not be converted to a ",
          "single OPMS object and will be returned as a list")
        result
      }),
      stop(BUG_MSG)
    )
}


################################################################################
#
# Metadata IO
#


setGeneric("to_metadata",
  function(object, ...) standardGeneric("to_metadata"))
#' Input metadata (filename version)
#'
#' Read metadata from an input file. This is only a thin wrapper of
#' \code{read.delim} but contains some useful adaptations (such as \strong{not}
#' converting strings to factors, and not modifying column names).
#'
#' @name to_metadata,character
#'
#' @param object Name of input file (character scalar).
#' @param sep Character scalar. Field separator in input file. This and the
#'   following parameters are passed to \code{read.delim}.
#' @param check.names Logical scalar.
#' @param strip.white Logical scalar.
#' @param stringsAsFactors Logical scalar.
#' @param ... Optional other arguments for \code{read.delim}.
#' @export
#' @return Dataframe.
#' @family metadata-functions
#' @family IO-functions
#' @keywords IO
#' @seealso default.stringsAsFactors read.delim
#' @examples
#' (x <- to_metadata(list(a = 7:8, `b c` = letters[1:2])))
#' tmpfile <- tempfile()
#' write.table(x, tmpfile, row.names = FALSE, sep = "\t")
#' (x1 <- read.delim(tmpfile))
#' (x2 <- to_metadata(tmpfile))
#' stopifnot(identical(names(x2), names(x)), !identical(names(x1), names(x)))
#'
setMethod("to_metadata", "character", function(object,
    sep = "\t", check.names = FALSE, strip.white = TRUE,
    stringsAsFactors = FALSE, ...) {
  assert_length(object)
  read.delim(object, sep = sep, check.names = check.names, 
    strip.white = strip.white, stringsAsFactors = stringsAsFactors, ...)
}, sealed = SEALED)


#' Input metadata
#'
#' Read metadata from an object convertible to a dataframe. This is only a thin
#' wrapper of \code{as.data.frame} but contains some useful adaptations, such 
#' as \strong{not} converting strings to factors, and not modifying column 
#' names by default.
#'
#' @param object Object convertible to a dataframe.
#' @param stringsAsFactors Logical scalar passed to \code{as.data.frame}.
#' @param optional Logical scalar passed to \code{as.data.frame}.
#' @param ... Optional arguments passed to \code{as.data.frame}.
#' @return Dataframe.
#' @export
#' @family metadata-functions
#' @family IO-functions
#' @keywords manip
#' @seealso default.stringsAsFactors as.data.frame
#' @examples
#' x <- list(a = 7:8, `b c` = letters[1:2])
#' (x1 <- as.data.frame(x))
#' (x2 <- to_metadata(x))
#' stopifnot(!identical(names(x), names(x1)), identical(names(x), names(x2)))
#'
setMethod("to_metadata", "ANY", function(object, stringsAsFactors = FALSE, 
    optional = TRUE, ...) {
  as.data.frame(object, stringsAsFactors = stringsAsFactors, 
    optional = optional, ...)
}, sealed = SEALED)


################################################################################
#
# Batch-collection functions
#


## NOTE: not an S4 method because conversion is done

#' Collect information from files
#'
#' Batch-collect information from a series of input files. This is not normally
#' directly called by an \pkg{opm} user because \code{\link{collect_template}}
#' is available.
#'
#' @inheritParams read_opm
#' @param names Character vector with file or directory names, or convertible 
#'   to such. See \code{\link{explode_dir}} for details.
#' @param fun Collecting function. Should use the filename as first argument.
#' @param fun.args Optional list of arguments to \code{fun}.
#' @param ... Optional further arguments passed to \code{\link{explode_dir}}.
#' @param simplify Logical scalar. Should the resulting list be simplified to a
#'   vector or matrix if possible?
#' @export
#' @family IO-functions
#' @return List, potentially simplified to a vector, depending on the output of
#'   \code{fun} and the value of \code{simplify}. See also \code{demo}.
#' @keywords IO
#' @examples
#' # Read the first line from each of the OPM test dataset files
#' f <- opm_files("testdata")
#' if (length(f) > 0L) {
#'   x <- batch_collect(f, fun = readLines, fun.args = list(n = 1L))
#'   # yields a list with the input files as names and the result from each
#'   # file as values (exactly one line)
#'   stopifnot(is.list(x), identical(names(x), f))
#'   stopifnot(sapply(x, is.character), sapply(x, length) == 1L)
#' }
#'
batch_collect <- function(names, fun, fun.args = list(), ...,
    simplify = FALSE, demo = FALSE) {
  names <- explode_dir(names, ...)
  if (demo) {
    message(paste(names, collapse = "\n"))
    return(invisible(names))
  }
  fun.args <- as.list(fun.args)
  sapply(names, FUN = function(infile) do.call(fun, c(infile, fun.args)),
    simplify = simplify)
}


#' Collect metadata template
#'
#' Batch-collect information from OmniLog(R) CSV comments for later on adding
#' metadata. Optionally add these as novel rows to previously collected data.
#' Write collected template to a file for use with an external editor, and/or
#' create data frame for editing the data directly in R with the \code{edit} 
#' function.
#'
#' @param object Character vector. Acts like the \code{names} argument of 
#'   \code{\link{read_opm}}. That is, if it a directory name, this is 
#'   automatically scanned for all CSV and YAML files it contains (unless
#'   restrictions with patterns are made). One can also provide file names, or
#'   a mixture of file and directory names.
#' @param outfile Character scalar. Ignored if \code{NULL} or empty string.
#'   Otherwise, interpreted as the name of a CSV output file. If metadata have 
#'   already been collected in an older file with the same name, old metadata
#'   will be kept, identifiers for novel files will be included, their so far
#'   empty entries set to NA. Users who wish to keep the old version will use 
#'   two distinct names for novel and old files; see \code{previous}.
#' @param sep Character scalar. CSV field separator for \code{outfile}.
#' @param previous Ignored if \code{NULL}. Otherwise passed to 
#'   \code{\link{to_metadata}} or \code{\link{to_metadata,character}}. If it is
#'   a filename different from \code{outfile}, it is an error if the file does
#'   not exist.
#' @param md.args List of other arguments passed to the \sQuote{to_metadata}
#'   methods.
#' @param add.cols Optional character vector with the names of columns to be 
#'   added to the result. See the eponymous argument of 
#'   \code{\link{collect_template,OPM}} for details.
#' @param selection Elements to be extracted from the CSV comments contained
#'   in each file. See \code{\link{collect_template,OPM}} for details.
#' @param include File inclusion pattern (or generator for a pattern). Passed
#'   to \code{\link{batch_collect}}.
#' @param ... Other arguments passed to \code{\link{batch_collect}}.
#' @param demo Logical scalar. Run in \sQuote{demo} mode? Also passed to
#'   \code{\link{batch_collect}}.
#' @export
#' @return Dataframe, returned invisibly if \code{outfile} is given; if 
#'   \code{demo} is \code{TRUE}, a character vector of filenames instead, also
#'   returned invisibly.
#' @note Regarding the CSV format, see the remark to 
#'   \code{\link{read_single_opm}}.
#' @seealso edit read.delim
#' @family IO-functions
#' @family metadata-functions
#' @references \url{http://www.biolog.com/}
#' @keywords IO
#' @examples 
#' test.files <- grep("Multiple", opm_files("testdata"), invert = TRUE, 
#'   value = TRUE, fixed = TRUE)
#' if (length(test.files) > 0) {
#'
#'   # Without writing to a file
#'   (x <- collect_template(test.files))
#'   stopifnot(is.data.frame(x), identical(x[, "File"], test.files))
#'   # now proceed with e.g. 
#'   # x <- edit(x)
#'
#'   # Write to file
#'   outfile <- tempfile()
#'   stopifnot(!file.exists(outfile))
#'   # This results in a CSV outfile which could be used as a starting point 
#'   # for including the metadata of interest together with the plate 
#'   # identifiers in a single file. include_metadata() can then be used to
#'   # integrate the metadata in OPM, OPMA or OPMS objects.
#'   x <- collect_template(test.files, outfile = outfile)
#'   stopifnot(file.exists(outfile))
#'   unlink(outfile)
#' }
#'
setMethod("collect_template", "character", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = c(SETUP, POS, FILE), add.cols = NULL, 
    include = list(), ..., demo = FALSE) {
  result <- batch_collect(object, fun = function(infile) {
    opm.data <- read_single_opm(infile)
    if (is.list(opm.data)) # possible in case of YAML input
      do.call(rbind, lapply(opm.data, FUN = collect_template, 
        selection = selection, add.cols = add.cols))
    else  
      collect_template(opm.data, selection = selection, add.cols = add.cols)
  }, include = include, ..., simplify = FALSE, demo = demo)
  if (demo)
    return(invisible(result))
  result <- do.call(rbind, result)
  rownames(result) <- NULL # if 'previous' was given, row names lacked anyway
  if (!is.null(previous))
    tryCatch({
      suppressWarnings(previous <- do.call(to_metadata, 
        c(list(object = previous), md.args)))
    }, error = function(e) {
      if (identical(outfile, previous))
        previous <<- NULL
      else
        stop(e$message)
    })
  if (!is.null(previous))
    result <- merge(previous, result, all = TRUE)
  result <- unique(result)
  if (is.null(outfile) || !nzchar(outfile))
    result
  else {
    write.table(result, file = outfile, sep = sep, row.names = FALSE)
    invisible(result)
  }
}, sealed = SEALED)


################################################################################
#
# Batch conversion functions
#


## NOTE: not an S4 method because conversion is done by calling functions, and
## error message would be meaningful.

#' Conversion between two files
#'
#' Convert data from an input file to data in an output file.
#'
#' @param files Two-element character vector containing the input and output 
#'   files.
#' @param io.fun Conversion function. Should accept \code{infile} and
#'   \code{outfile} as the first two arguments.
#' @param fun.args Optional list of further arguments of \code{io.fun}.
#' @param overwrite Character scalar. If \sQuote{yes}, conversion is always
#'   tried if \code{infile} exists and is not empty. If \sQuote{no},
#'   conversion is not tried if \code{outfile} exists and is not empty. If
#'   \sQuote{older}, conversion is tried if \code{outfile} does not exist or
#'   is empty or is older than \code{infile} (with respect to the modification
#'   time).
#' @param verbose Logical scalar. Print conversion and success/failure
#'   information?
#' @return Character vector corresponding to one of the rows of the result of
#'   \code{\link{batch_process}}.
#' @keywords internal
#'
process_io <- function(files, io.fun, fun.args = list(),
    overwrite = c("no", "older", "yes"), verbose = TRUE) {
  empty <- function(file.status) {
    is.na(file.status$size) || file.status$size == 0
  }
  create_parent <- function(filename) {
    outdir <- dirname(filename)
    file.exists(outdir) || dir.create(outdir, recursive = TRUE,
      showWarnings = FALSE)
  }
  prepare_conversion <- function(infile, outfile, overwrite) {
    istat <- file.info(c(infile, outfile)) # fails if not character
    ostat <- istat[2L, ]
    istat <- istat[1L, ]
    if (empty(istat))
      "infile unknown or empty"
    else
      switch(overwrite,
        yes = if (unlink(outfile) == 0L)
          ""
        else
          "could not delete outfile",
        no = if (empty(ostat))
          ""
        else
          "outfile not empty",
        older = if (!empty(ostat) && istat$mtime < ostat$mtime)
          "outfile not empty and newer"
        else if (unlink(outfile) == 0L)
          ""
        else
          "could not delete outfile",
        stop(BUG_MSG)
      )
  }
  conduct_conversion <- function(infile, outfile, fun, fun.args) {
    if (!create_parent(outfile))
      return("could not create parent directory")
    problem <- tryCatch({
      do.call(fun, c(infile = infile, outfile = outfile, fun.args))
      ""
    }, error = function(e) e$message)
    if (nzchar(problem))
      problem
    else if (empty(file.info(outfile)))
      "outfile not created or empty"
    else
      "ok"
  }
  assert_length(files, .wanted = 2L)
  overwrite <- match.arg(overwrite)
  result <- list(infile = files[1L], outfile = files[2L], before = "", 
    after = "")
  result$before <- prepare_conversion(files[1L], files[2L], overwrite)
  if (!nzchar(result$before)) {
    result$before <- "attempt to create outfile"
    result$after <- conduct_conversion(files[1L], files[2L], io.fun, fun.args)
  }
  if (verbose) {
    sapply(formatDL(unlist(result), style = "list"), FUN = message)
    message("")
  }
  unlist(result)
}


## NOTE: not an S4 method because conversion is done

#' Convert infiles to outfiles
#'
#' Batch-convert data from infiles to data in outfiles. This is not normally
#' directly called by an \pkg{opm} user because \code{\link{batch_opm_to_yaml}}
#' is available.
#'
#' @inheritParams process_io
#' @inheritParams batch_collect
#' @param out.ext Character scalar. The extension of the outfile names (without
#'   the dot).
#' @param proc Integer scalar. The number pf processes to spawn.
#' @param outdir Character vector. Directories in which to place the outfiles.
#'   If \code{NULL}, each infile's directory is used.
#' @param in.ext Character scalar. Passed through \code{\link{file_pattern}},
#'   then used for the replacement of old file extensions with new ones.
#' @param compressed Logical scalar. Passed as 2nd argument to 
#'   \code{\link{file_pattern}}.
#' @param demo Logical scalar. Do not convert files, but print the attempted
#'   infile-outfile conversions and invisibly return a matrix with infiles in
#'   the first and outfiles in the second column?
#' @export
#' @return In normal mode, an invisibly returned character matrix in which each
#'   row corresponds to a named character vector with the keys \sQuote{infile},
#'   \sQuote{outfile}, \sQuote{before} and \sQuote{after}. The latter two
#'   describe the result of the action(s) before and after attempting to
#'   convert \code{infile} to \code{outfile}. \sQuote{after} is the empty
#'   string if no conversion was tried (see \code{overwrite}), \sQuote{ok} if
#'   conversion was successful and a message describing the problems otherwise.
#'   For the results of the \code{demo} mode see above.
#' @keywords IO
#' @family IO-functions
#' @examples
#' # Read the first line from each of the OPM test dataset files and store it
#' # in temporary files
#' pf <- function(infile, outfile) write(readLines(infile, n = 1), outfile)
#' infiles <- opm_files("testdata")
#' if (length(infiles) > 0) {
#'   x <- batch_process(infiles, out.ext = "tmp", io.fun = pf, 
#'     outdir = tempdir())
#'   stopifnot(is.matrix(x), identical(x[, 1], infiles))
#'   stopifnot(file.exists(x[, 2]))
#'   unlink(x[, 2])
#' }
#'
batch_process <- function(names, out.ext, io.fun, fun.args = list(), proc = 1L,
    outdir = NULL, overwrite = c("yes", "older", "no"), 
    in.ext = "any", compressed = TRUE,
    ..., verbose = TRUE, demo = FALSE) {
  create_outfile_names <- function(infiles, outdir, out.ext) {
    if (length(outdir) == 0L)
      outdir <- dirname(infiles)
    result <- sub(in.ext, "", basename(infiles), perl = TRUE,
      ignore.case = TRUE)
    result <- paste(result, sub("^\\.+", "", out.ext), sep = ".")
    file.path(outdir, result)
  }
  in.ext <- file_pattern(in.ext, compressed)
  overwrite <- match.arg(overwrite)
  infiles <- explode_dir(names, ...)
  outfiles <- create_outfile_names(infiles, outdir, out.ext)
  if (demo) {
    message(paste(infiles, outfiles, sep = "\n  => ", collapse = "\n"))
    return(invisible(cbind(infiles, outfiles)))
  }
  fun.args <- as.list(fun.args)
  data <- mapply(c, infiles, outfiles, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  result <- traverse(object = data, func = process_io, cores = proc, 
    io.fun = io.fun, fun.args = fun.args,
    overwrite = overwrite, verbose = verbose)
  invisible(do.call(rbind, result))
}


################################################################################
#
# Batch IO with \code{\link{OPM}} objects
#


## NOTE: not an S4 method because conversion is done

#' Batch-convert to YAML
#'
#' Batch-convert from OmniLog(R) CSV (or previous \pkg{opm} YAML) to \pkg{opm}
#' YAML. It is possible to add metadata to each set of raw data and to aggregate
#' the curves; these additional data will then be included in the YAML output
#' file.
#'
#' @inheritParams read_opm
#' @inheritParams batch_process
#' @param md.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{include_metadata}} with the data read from each individual
#'   file as additional argument \sQuote{object}. If \code{NULL}, metadata are
#'   not included (but may already be present in the case of YAML input).
#' @param aggr.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{do_aggr}} with the data read from each individual file as
#'   additional argument \code{x}.  If \code{NULL}, aggregation takes not place
#'   (but aggregated data may already be present in case of YAML input).
#' @param force.aggr Logical scalar. If \code{FALSE}, do not aggregate already
#'   aggregated data (which can be present in YAML input).
#' @param ... Optional arguments passed to \code{\link{batch_process}} in
#'   addition to \code{verbose} and \code{demo}. Note that \code{out.ext},
#'   \code{fun} and \code{fun.args} are set automatically.
#' @export
#' @note \itemize{
#'   \item Regarding the CSV format, see the remark to 
#'     \code{\link{read_single_opm}}. 
#'   \item This function is for batch-converting many
#'     files; for writing a single object to a YAML file (or string), see
#'     \code{\link{to_yaml}}.
#'   \item When inputting YAML files generated with the help of the \pkg{yaml}
#'     package (on which the \pkg{opm} implementation is based) using other 
#'     programming languages, a potential problem is that they, and YAML in 
#'     general, lack a native representation of \code{NA} values. Such entries
#'     are likely to be misunderstood as \sQuote{NA} character scalars.
#' }
#' @return The function invisibly returns a matrix which describes each 
#'   attempted file conversion. See \code{\link{batch_process}} for details.
#' @family IO-functions
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso read.csv yaml::yaml.load_file
#' @keywords IO
#'
#' @details
#'   A YAML document can comprise \emph{scalars} (single values of some 
#'   type), \emph{sequences} (ordered collections of some values, without 
#'   names) and \emph{mappings} (collections assigning a name to each value),
#'   in a variety of combinations (e.g., mappings of sequences). The output
#'   of \code{batch_opm_to_yaml} is one YAML document \emph{per plate} which
#'   represents a mapping with the following components (key-value pairs):
#'   \describe{
#'     \item{metadata}{Arbitrarily nested mapping of arbitrary metadata 
#'       entries. Empty if no metadata have been added.}
#'     \item{csv_data}{Non-nested mapping containing the OmniLog(R) run 
#'       information read from the input CSV file (character scalars) together
#'       with the measurements. The most important entry is most likely the 
#'       plate type.}
#'     \item{measurements}{A mapping whose values are sequences of 
#'       floating-point numbers of the same length and in the appropriate 
#'       order. The keys are \sQuote{hours}, which refers to the time points, 
#'       and the well coordinates, ranging between \sQuote{A01} and 
#'       \sQuote{H12}.}
#'     \item{aggregated}{A mapping, only present if curve parameters have been
#'       estimated. Its keys correspond to those of \sQuote{measurements} with
#'       the exception of \sQuote{hours}. The values are themselves mappings, 
#'       whose keys indicate the respective curve parameter and whether this
#'       is the point estimate or the upper or lower confidence interval. The 
#'       values of these secondary mappings are floating-point numbers.}
#'     \item{aggr_settings}{A mapping, only present if curve parameters have 
#'       been estimated. Its keys are \sQuote{program} and \sQuote{options}. 
#'       The value of the former is a character scalar. The value of the latter
#'       is an arbitrarily nested mapping with arbitrary content.}
#'   }
#'   Details of the contents should be obvious from the documentation of the
#'   classes of the objects from which the YAML output is generated. In the 
#'   case of YAML input with several plates per file, \code{batch_opm_to_yaml}
#'   generates YAML output files containing a sequence of mappings as described
#'   above, one per plate, to keep a 1:1 relationship between input and output 
#'   files.
#' 
#' @examples 
#' test.files <- grep("Multiple", opm_files("testdata"), invert = TRUE, 
#'   value = TRUE, fixed = TRUE)
#' if (length(test.files) > 0) {
#'   num.files <- length(list.files(outdir <- tempdir()))
#'   x <- batch_opm_to_yaml(test.files[1], outdir = outdir)
#'   stopifnot(length(list.files(outdir)) == num.files + 1, is.matrix(x))
#'   stopifnot(file.exists(x[, "outfile"]))
#'   stopifnot(test.files[1] == x[, "infile"])
#'   unlink(x[, "outfile"])
#' }
#'
batch_opm_to_yaml <- function(names, md.args = NULL, aggr.args = NULL, 
    force.aggr = FALSE, gen.iii = FALSE, ..., verbose = TRUE, demo = FALSE) {

  convert_dataset <- function(data) {
    if (gen.iii) {
      if (verbose)
        message("conversion: changing to 'Generation III'...")
      data <- gen_iii(data)
    }
    if (length(md.args) > 0L) {
      if (verbose)
        message("conversion: including metadata...")
      data <- do.call(include_metadata, c(object = data, md.args))
    }
    if (length(aggr.args) > 0L)
      if (force.aggr || !has_aggr(data)) {
        if (verbose)
          message("conversion: aggregating data...")
        data <- do.call(do_aggr, c(list(data), aggr.args))
      } else if (verbose)
        message("conversion: previously aggregated data present, ",
          "skipping that step")
    data
  }

  convert_to_yaml <- function(infile, outfile) {
    data <- read_single_opm(infile)
    # YAML input can result in lists of several OPM objects
    data <- if (is.list(data))
      lapply(lapply(data, FUN = convert_dataset), FUN = as, Class = "list")
    else
      convert_dataset(data)
    write(to_yaml(data), outfile)
  }

  # If a metadata filename is given, read it into dataframe right now to
  # avoid opening the file each time in the batch_process() loop
  if (length(md.args) > 0L && is.character(md.args$md)) {
    tmp <- md.args
    names(tmp)[names(tmp) == "md"] <- "object"
    tmp$replace <- NULL
    md.args$md <- do.call(to_metadata, tmp)
  }

  batch_process(names = names, out.ext = "yml", io.fun = convert_to_yaml,
    in.ext = "both", compressed = TRUE, ..., verbose = verbose, demo = demo)

}


################################################################################
#
# File splitting
#


## NOTE: not an S4 method because conversion is done

#' Split files
#'
#' Split files, i.e. subdivide each file into sections which are written 
#' individually to newly generated files. Sections are determined with patterns
#' that match the start of a section.
#'
#' @param files Character vector or convertible to such. Names of the files to
#'   be split.
#' @param pattern Regular expression or shell globbing pattern for matching the
#'   separator lines if \code{invert} is \code{FALSE} (the default) or matching
#'   the non-separator lines if otherwise. Conceptually each of the sections 
#'   into which a file is split comprises a separator line followed by 
#'   non-separator lines. That is, separator lines followed by another 
#'   separator line are ignored. Non-separator lines not preceded by a 
#'   separator line are treated as a section of their own, however.
#'
#' @param outdir Character scalar determining the output directory. If empty,
#'   each file's input directory is used.
#' @param demo Logical scalar. Do not create files, just return the usual
#'   list containing all potentially created files. Note that in contrast to
#'   the \code{demo} arguments of other IO functions, this requires the input
#'   files to be read.
#'
#' @param single Logical scalar. If there is only one group per file, i.e. only
#'   one output file would result from the splitting, create this file anyway?
#'   Such cases could be recognized by empty character vectors as values of the
#'   returned list (see below).
#' @param wildcard Logical scalar. Is \code{pattern} a shell-globbing wildcard
#'   that first needs to be converted to a regular expression?
#' @param invert Logical scalar. Invert pattern matching, i.e. treat all lines
#'   that \strong{not} match \code{pattern} as separators?
#'
#' @param format Character scalar determining the outfile name format. It is
#'   passed to \code{sprintf} and expects three placeholders: (i) the basename
#'   of the file; (ii) the index of the section; and (iii) the file extension.
#'   Getting \code{format} wrong might result in non-unique filenames and thus
#'   probably in overwritten files; accordingly, it should be used with care.
#' @param compressed Logical scalar. Passed to \code{\link{file_pattern}}, but
#'   here only affects the way filenames are split in extensions and basenames.
#'   Should only be set to \code{FALSE} if input files are not compressed (and
#'   have according file extensions).
#'
#' @param ... Optional arguments passed to \code{grepl}, which is used for
#'   matching the seperator lines. See also \code{invert} listed above.
#'
#' @export
#' @return List of character vectors, each vector containing the names of the 
#'   newly generated files. The names of the list are the input filenames. The
#'   list is returned invisibly.
#'
#' @family IO-functions
#' @seealso split strsplit
#' @keywords utilities
#' @examples
#' # Splitting an old-style CSV file containing several plates
#' (x <- grep("Multiple", opm_files("testdata"), value = TRUE, fixed = TRUE))
#' if (length(x) > 0) {
#'   outdir <- tempdir()
#'   # For old-style CSV, use either "^Data File" as pattern or "Data File*"
#'   # with 'wildcard' set to TRUE:
#'   (result <- split_files(x, pattern = "^Data File", outdir = outdir))
#'   stopifnot(is.list(result), length(result) == length(x))
#'   stopifnot(sapply(result, length) == 3)
#'   result <- unlist(result)
#'   stopifnot(file.exists(result))
#'   unlink(result) # tidy up
#' }
#' ## One could split new-style CSV as follows (if x is a vector of filenames):
#' # split_files(x, pattern = '^"Data File",')
#' ## note the correct setting of the quotes
#' ## A pattern that covers both old and new-style CSV is:
#' # split_files(x, pattern = '^("Data File",|Data File)')
#' ## This is used by the run_opm.R script
#'
split_files <- function(files, pattern, outdir = "", demo = FALSE, 
    single = TRUE, wildcard = FALSE, invert = FALSE, format = "%s-%05i.%s", 
    compressed = TRUE, ...) {
  
  create_outnames <- function(files, compressed, outdir) {
    file.pat <- file_pattern("any", compressed = compressed)
    out.base <- sub(file.pat, "", files, perl = TRUE, ignore.case = TRUE)
    out.ext <- substring(files, nchar(out.base) + 2L)
    if (compressed)
      out.ext <- sub("\\.[^.]+$", "", out.ext, perl = TRUE)
    if (nzchar(outdir))
      out.base <- file.path(outdir, basename(out.base))
    list(base = out.base, ext = out.ext)
  }

  assert_length(pattern, outdir, demo, single, wildcard, invert, format, 
    compressed)
  files <- unique(as.character(files))
  out <- create_outnames(files, compressed = compressed, outdir = outdir)
  if (wildcard)
    pattern <- glob_to_regex(pattern)

  invisible(mapply(function(infile, out.base, out.ext) {
    data <- readLines(con = infile)
    data <- split(data, group_by_sep(object = data, pattern = pattern, 
      invert = invert, ...))
    if ((len <- length(data)) == 0L || (!single && len == 1L))
      return(character())
    outnames <- sprintf(format, out.base, seq_along(data), out.ext)
    if(demo)
      message(listing(structure(.Data = outnames, names = seq_along(outnames)),
        header = infile))
    else
      mapply(write, data, outnames, USE.NAMES = FALSE, SIMPLIFY = FALSE)
    outnames
  }, files, out$base, out$ext, SIMPLIFY = FALSE))
}


################################################################################
#
# File renaming
#
                   

## NOTE: not an S4 method because conversion is done

#' Clean file names
#'
#' Clean file names by removing anything else then word characters, dashes, and
#' dots. Also remove trailing and leading dashes and underscores (per part of a
#' file name, with dots separating these parts) and reduce adjacent dashes and
#' underscores to a single one. Note that directory parts within the file 
#' names, if any, are not affected.
#'
#' @param x Character vector or convertible to such. Names of the files to
#'   be modified.
#' @param overwrite Logical scalar. Overwrite pre-existing files, and do not
#'   care for duplicate names created by cleaning the filenames?
#' @param demo Logical scalar. Do not rename files, just return the usual
#'   result indicating the renamings that would be attempted? (Note that this
#'   does not indicate whether the renamings would also by successful.)
#' @param empty.tmpl Character scalar. The template to use for filenames that
#'   become empty after cleaning. Should include an integer placeholder to
#'   enable incrementing an index for creating unique filenames. (Empty 
#'   filenames should occur rarely anyway.)
#' @export
#' @return Character vector, its names corresponding to the renamed old files,
#'   values corresponding to the novel names, returned invisibly.
#' @family IO-functions
#' @keywords utilities
#' @seealso file.rename
#' @examples
#'  
#' # Check the example files: they should be ok
#' (x <- clean_filenames(opm_files("testdata"), demo = TRUE))
#' stopifnot(length(x) == 0)  
#'   
#' # Example with temporary files
#' (x <- tempfile(pattern = "cb ahi? si--"))  
#' write("test", x)
#' stopifnot(file.exists(x))
#' (y <- clean_filenames(x))
#' stopifnot(!file.exists(x), file.exists(y))
#' unlink(y) # tidy up
#'
clean_filenames <- function(x, overwrite = FALSE, demo = FALSE, 
    empty.tmpl = "__EMPTY__%05i__") {
  empty.idx <- 0L
  clean_parts <- function(x) {
    x <- gsub("[^\\w-]+", "_", x, perl = TRUE)
    x <- gsub("_*-_*", "-", x, perl = TRUE)
    x <- gsub("-+", "-", gsub("_+", "_", x, perl = TRUE), perl = TRUE)
    x <- sub("[_-]+$", "", sub("^[_-]+", "", x, perl = TRUE), perl = TRUE)
    x <- x[nzchar(x)]
    if (!length(x))
      x <- sprintf(empty.tmpl, empty.idx <<- empty.idx + 1L)
    x
  }
  clean_basenames <- function(x) {
    x <- lapply(strsplit(x, ".", fixed = TRUE), FUN = clean_parts)
    unlist(lapply(x, FUN = paste, collapse = "."))
  }
  assert_length(overwrite, demo, empty.tmpl)
  x <- unique(as.character(x))
  result <- file.path(dirname(x), clean_basenames(basename(x)))
  different <- result != x
  result <- structure(.Data = result[different], names = x[different])
  if (!overwrite) {
    result <- result[!duplicated(result)]
    result <- result[!file.exists(result)]
  }
  if (demo)
    message(listing(result, header = "Attempted renamings:"))
  else
    result <- result[file.rename(names(result), result)]
  invisible(result)
}

                   
