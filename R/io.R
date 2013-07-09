

################################################################################
################################################################################
#
# IO helpers
#


## NOTE: not an S4 method because check is done using match.arg()

#' File pattern
#'
#' Create a regular expression matching certain file extensions. This is not
#' normally directly called by an \pkg{opm} user but by the other IO functions
#' of the package.
#'
#' @param type Character scalar indicating the file types to be matched by
#'   extension. Alternaticely, directly the extension or extensions, or a list
#'   of file names (not \code{NA}).
#' @param compressed Logical scalar. Shall compressed files also be matched?
#'   This affects the returned pattern as well as the pattern used for
#'   extracting file extensions from complete file names (if \code{literally}
#'   is \code{TRUE}).
#' @param literally Logical scalar. Interpret \code{type} literally? This also
#'   allows for vectors with more than a single element, as well as the
#'   extraction of file extensions from file names.
#' @export
#' @return Character scalar, holding a regular expression.
#' @family io-functions
#' @keywords utilities
#' @seealso tools::file_ext
#' @examples
#' (x <- file_pattern())
#' (y <- file_pattern(type = "csv", compressed = FALSE))
#' stopifnot(nchar(x) > nchar(y))
#' # constructing pattern from existing files
#' (files <- list.files(pattern = "[.]"))
#' (x <- file_pattern(I(files))) # I() causes 'literally' to be TRUE
#' stopifnot(grepl(x, files, ignore.case = TRUE))
#'
file_pattern <- function(
    type = c("both", "csv", "yaml", "json", "any", "empty"),
    compressed = TRUE, literally = inherits(type, "AsIs")) {
  make_pat <- function(x, compressed, enclose = "\\.%s$") {
    if (compressed)
      x <- sprintf("%s(\\.(bz2|gz|lzma|xz))?", x)
    sprintf(enclose, x)
  }
  LL(literally, compressed)
  result <- if (literally) {
    x <- make_pat("([^.]+)", compressed, "^.*?\\.%s$")
    x <- sub(x, "\\1", type, perl = TRUE)
    if (all(same <- x == basename(type))) { # assuming extensions
      type <- x
      bad <- "^\\w+$"
    } else { # assuming file names
      type <- x[!same]
      bad <- "^\\w+(\\.\\w+)?$"
    }
    if (any(bad <- !grepl(bad, type <- unique.default(type), perl = TRUE)))
      stop("'type' must contain word characters (only): ", type[bad][1L])
    case(length(type), stop("'type' must be non-empty"), type,
      sprintf("(%s)", paste(type, collapse = "|")))
  } else
    case(match.arg(type), both = "(csv|ya?ml|json)", csv = "csv",
      yaml = "ya?ml", json = "json", any = "[^.]+", empty = "")
  make_pat(result, compressed)
}


################################################################################


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
#' @seealso tools::file_ext
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


#' Repair OTH plates
#'
#' Plates run in generation-III mode, if converted to \acronym{CSV} by the
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} software, comprise
#' duplicated rows with only two sets of distinct values, the first set
#' containing only \code{0}. This function checks for such a matrix and, if
#' identified, reduces it to its single informative row.
#'
#' @param x Numeric matrix.
#' @return Matrix. Either equal to \code{x} or conatining a single row only.
#' @keywords internal
#'
setGeneric("repair_oth", function(x, ...) standardGeneric("repair_oth"))

setMethod("repair_oth", "matrix", function(x) {
  result <- unique(x[, -1L, drop = FALSE], MARGIN = 1L)
  if ((nr <- nrow(result)) > 2L || !all(result[1L, ] == 0))
    return(x)
  result <- if (nr == 2L)
    cbind(0.25, result[2L, , drop = FALSE])
  else
    cbind(0, result) # in the unexpected case that ONLY 0s are encountered
  colnames(result)[1L] <- colnames(x)[1L]
  result
}, sealed = SEALED)


################################################################################
################################################################################
#
# Input of single OPM files
#


## NOTE: not an S4 method because scan() gives an adequate error message

#' Read old-style OmniLog CSV file
#'
#' Read OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data from single
#' pseudo-\acronym{CSV} file in the old (pre-2011) format.
#'
#' @param filename Character scalar with the obvious meaning.
#' @return \code{\link{OPM}} object.
#' @references \url{http://www.biolog.com/}
#' @keywords internal
#'
read_old_opm <- function(filename) {

  strip <- function(x) sub("\\s+$", "", sub("^\\s+", "", x, perl = TRUE),
    perl = TRUE)

  prepare_comments <- function(x, filename) {
    ok <- nzchar(n <- strip(vapply(x, `[[`, "", 1L)))
    n[n == "Set up Time"] <- CSV_NAMES[["SETUP"]]
    x <- strip(vapply(lapply(x, `[`, -1L), paste0, "", collapse = ","))
    structure(c(filename, x[ok]), names = c(CSV_NAMES[["FILE"]], n[ok]))
  }

  con <- file(description = filename, encoding = opm_opt("file.encoding"))
  data <- readLines(con = con, warn = FALSE)
  close(con)
  data <- strsplit(data, ",", fixed = TRUE)
  data <- data[vapply(data, length, 0L) > 0L]

  # determine position of first field of data header, then split lines into
  # comments and data fields accordingly
  pos <- which(strip(vapply(data, `[[`, "", 1L)) == HOUR)
  if (length(pos) != 1L)
    stop("uninterpretable header (maybe because there is not 1 plate per file)")
  pos <- seq_len(pos - 1L)
  comments <- prepare_comments(data[pos], filename)

  data <- strip(unlist(data[-pos]))
  data <- data[nzchar(data)]
  ncol <- 97L
  if (length(data) %% ncol != 0L)
    stop("wrong number of fields")
  data <- matrix(as.numeric(data[-seq(ncol)]), ncol = ncol, byrow = TRUE,
    dimnames = list(NULL, data[seq(ncol)]))

  # Repair OTH (this affects both data and comments)
  if (comments[CSV_NAMES[["PLATE_TYPE"]]] == "OTH") {
    comments[CSV_NAMES[["PLATE_TYPE"]]] <- SPECIAL_PLATES[["gen.iii"]]
    data <- repair_oth(data)
  }

  new(OPM, measurements = data, metadata = list(), csv_data = comments)
}


################################################################################


## NOTE: not an S4 method because scan() gives an adequate error message

#' Read OmniLog CSV file
#'
#' Read OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} data from single
#' \acronym{CSV} file in novel (2011) format.
#'
#' @param filename Character scalar with the obvious meaning.
#' @return \code{\link{OPM}} object.
#' @references \url{http://www.biolog.com/}
#' @keywords internal
#'
read_new_opm <- function(filename) {
  data <- scan(file = filename, sep = ",", what = "character", quiet = TRUE,
    comment.char = "#", strip.white = TRUE,
    fileEncoding = opm_opt("file.encoding"))
  pos <- which(data == HOUR)
  if (length(pos) != 1L)
    stop("uninterpretable header (maybe there is not 1 plate per file)")
  ncol <- pos + 96L
  if (length(data) %% ncol != 0L)
    stop("wrong number of fields")
  data <- matrix(data[-seq_len(ncol)], ncol = ncol, byrow = TRUE,
    dimnames = list(NULL, data[seq_len(ncol)]))
  pos <- seq_len(pos - 1L)
  comments <- structure(.Data = c(filename, data[1L, pos]),
    names = c(CSV_NAMES[["FILE"]], colnames(data)[pos]))
  data <- apply(data[, -pos], MARGIN = 2L, FUN = as.numeric)
  if (comments[CSV_NAMES[["PLATE_TYPE"]]] == "OTH") {
    comments[CSV_NAMES[["PLATE_TYPE"]]] <- SPECIAL_PLATES[["gen.iii"]]
    data <- repair_oth(data)
  }
  new(OPM, measurements = data, metadata = list(), csv_data = comments)
}


################################################################################


## NOTE: not an S4 method because yaml.load_file() gives an adequate error
##       message

#' Read opm YAML file
#'
#' Read \pkg{opm} data from a single \acronym{YAML} (or \acronym{JSON}) file.
#'
#' @param filename Character scalar with the obvious meaning.
#' @return \code{\link{OPM}}, \code{\link{OPMA}} or \code{\link{OPMD}} object,
#'   depending on the presence of aggregated and discretized data. Can also be a
#'   list of such objects.
#' @details Arbitrary nesting levels are allowed, i.e. the \acronym{YAML} input
#'   results in either a list representing the slots of an \code{\link{OPM}} or
#'   \code{\link{OPMA}} object, or a list whose direct or indirect sublists
#'   represent such an object. However, it is an error if non-list elements are
#'   encountered when traversing such lists, and if no \code{\link{OPM}} or
#'   \code{\link{OPMA}} objects can be found at all.
#'
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.json.org/}
#' @keywords internal
#'
read_opm_yaml <- function(filename) {
  result <- yaml.load_file(filename)
  result <- to_opm_list(result, precomputed = FALSE, skip = FALSE)
  if (!length(result))
    stop("YAML file contained no data interpretable by opm")
  result
}


################################################################################


#' Read opm from old MicroStation output
#'
#' Read \pkg{opm} data from a single
#' MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)} file. This must be a
#' comma-separated file with a certain number of defined columns. There are only
#' end-point measurements present: each resulting \code{\link{OPM}} object
#' contains only a single time point.
#'
#' @param filename Character scalar with the obvious meaning.
#' @return List of \code{\link{OPM}} objects.
#' @note It is unclear whether \sQuote{Plate Number} can really serve as an
#'   identifier, as duplicates might be present.
#' @keywords internal
#'
read_microstation_opm <- function(filename) {
  x <- read.table(filename, sep = ",", comment.char = "", header = TRUE,
    check.names = FALSE, stringsAsFactors = FALSE, quote = "",
    fileEncoding = opm_opt("file.encoding"))
  names(x)[!nzchar(names(x))] <- "N.N."
  pat <- ": Dual Wavelength O\\.D\\.$"
  wells <- grep(pat, names(x), perl = TRUE, value = TRUE)
  if (length(wells) != 96L)
    stop("expected 96 column names ending in ': Dual Wavelength O.D.'")
  wanted <- c("Plate Type", "Created", "Plate Number", "Incubation Time", wells)
  y <- x[, setdiff(names(x), wanted), drop = FALSE]
  x <- x[, wanted, drop = FALSE]
  x <- cbind(filename, x, stringsAsFactors = FALSE)
  names(x) <- c(CSV_NAMES[c("FILE", "PLATE_TYPE", "SETUP", "POS")], HOUR,
    clean_coords(sub(pat, "", wells, perl = TRUE)))
  pos <- seq_len(4L)
  x <- to_opm_list.list(lapply(seq_len(nrow(x)), function(i) {
    list(csv_data = as.list(x[i, pos, drop = FALSE]),
      metadata = y[i, , drop = TRUE],
      measurements = as.list(x[i, -pos, drop = FALSE]))
  }), FALSE, FALSE, FALSE)
  if (!length(x))
    stop("MicroStation CSV file contained no interpretable data")
  x
}


################################################################################


## NOTE: not an S4 method because conversion is done

#' Read single PM file
#'
#' Read single OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} or \pkg{opm}
#' data file in either new- or old-style \acronym{CSV} or \acronym{YAML}
#' (including \acronym{JSON} format.
#' MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)} \acronym{CSV} are
#' also understood, as well as files compressed using \command{gzip},
#' \command{bzip2}, \command{lzma} or \command{xz}.
#'
#' @param filename Character scalar, or convertible to such, with the obvious
#'   meaning.
#' @export
#' @return \code{\link{OPM}} object. In the case of \acronym{YAML} input, this
#'   might also be an \code{\link{OPMA}} object or a list of such objects, but
#'   \strong{not} an \code{\link{OPMS}} object.
#' @family io-functions
#'
#' @details The expected \acronym{CSV} format is what is output by an
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} instrument, one plate
#'   per file, or a MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}
#'   instrument, with one to many plates per file. Other formats, or
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} files re-saved with
#'   distinct \acronym{CSV} settings, are not understood. For this reason, if
#'   any editing of the files was necessary at all, it is advisable to do this
#'   in an editor for plain text, not in a spreadsheet program.
#'
#'   Plates run in ID mode are automatically detected as such (their plate type
#'   is changed from \sQuote{OTH} to the internally used spelling of
#'   \sQuote{Generation III}). A generation-III or other plate type can also be
#'   forced later on by using \code{\link{gen_iii}}.
#'
#'   It is \strong{impossible} to read \acronym{CSV} files that contain more
#'   than one plate. For splitting old-style and new-style \acronym{CSV} files
#'   into one file per plate, see the example under \code{\link{split_files}}.
#'   In contrast, input \acronym{YAML} files can contain data from more than one
#'   plate. The format (which includes \acronym{JSON}) is described in detail
#'   under \code{\link{batch_opm}}.
#'
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.json.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso utils::read.csv
#' @keywords IO
#' @examples
#' test.files <- opm_files("testdata")
#' if (length(test.files) > 0) { # if the folder is found
#'   x <- read_single_opm(test.files[1]) # => 'OPM' object
#'   class(x)
#'   dim(x)
#'   summary(x)
#'   stopifnot(is(x, "OPM"), identical(dim(x), c(384L, 96L)))
#' } else {
#'   warning("test-file folder not found")
#' }
#' # this can be repeated for the other input test files
#'
read_single_opm <- function(filename) {
  if (!file.exists(filename <- as.character(L(filename))))
    stop(sprintf("file '%s' does not exist", filename))
  errs <- list()
  routines <- list(`New CSV` = read_new_opm, `Old CSV` = read_old_opm,
    YAML = read_opm_yaml, `MicroStation CSV` = read_microstation_opm)
  for (name in names(routines)) {
    result <- tryCatch(routines[[name]](filename), error = conditionMessage)
    if (!is.character(result))
      return(result)
    errs[[name]] <- result
  }
  names(errs) <- paste(names(errs), "error")
  errs$Filename <- filename
  stop(listing(errs, header = "Unknown file format:"))
}


################################################################################
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
#'   argument which, if set to \code{TRUE}, caused the respective function to do
#'   no real work but print the names of the files that it would process in
#'   normal running mode.
#'
#' @export
#' @seealso base::list.files base::Sys.glo
#' @family io-functions
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


################################################################################


#' Opm files
#'
#' Get list of files from the \pkg{opm} package of interest for the user.
#'
#' @param what Character scalar indicating the subdirectory to search in.
#'   Currently the following ones are included:
#'   \describe{
#'     \item{auxiliary}{Miscellaneous files which are neither executable
#'       scripts nor test data. For instance, predefined \acronym{CSS} files
#'       for \acronym{HTML} files generated by \code{\link{phylo_data}} are
#'       found here.}
#'     \item{doc}{The vignette (documentation) in several formats, including
#'       the extracted \R code.}
#'     \item{examples}{Example \R code using the \pkg{opm} package that neither
#'       fitted into these help pages nor into the vignette.}
#'     \item{scripts}{\R script files for non-interactive uses of the
#'       \pkg{opm} package, particularly for the batch processing of many files.
#'       When called without input arguments or with the \sQuote{-h} switch, the
#'       scripts output usage information.}
#'     \item{testdata}{Files as output by the
#'       OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} instrument for
#'       testing data input and metadata management.}
#'   }
#' @export
#' @return Character vector of filenames.
#' @note This might fail with very unusual installations of the \pkg{opm}
#'   package.
#' @family io-functions
#' @seealso pkgutils::pkg_files
#' @keywords utilities
#' @examples
#' isRfile <- function(x) grepl("\\.R$", x, ignore.case = TRUE)
#' (x <- opm_files("auxiliary"))
#' stopifnot(!isRfile(x))
#' (x <- opm_files("doc"))
#' stopifnot(!all(isRfile(x)), any(isRfile(x)))
#' (x <- opm_files("examples"))
#' stopifnot(isRfile(x))
#' (x <- opm_files("scripts"))
#' stopifnot(isRfile(x))
#' (x <- opm_files("testdata"))
#' stopifnot(!isRfile(x))
#'
#' # On UNIX systems you should be able to do this if Rscript and the optparse
#' # package are properly installed:
#' # invisible(sapply(paste("Rscript", opm_files()), system))
#' # ...and get the usage messages of all scripts.
#'
opm_files <- function(
    what = c("scripts", "testdata", "auxiliary", "examples", "doc")) {
  pkg_files(x = opm_string(), what = match.arg(what))
}


################################################################################


## Not an S4 method because conversion is done

#' Read multiple PM files at once
#'
#' Read OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} or \pkg{opm} data
#' file(s) in one of three possible formats: either new- or old-style
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV} or
#' \pkg{opm} \acronym{YAML} format. Files compressed using \command{gzip},
#' \command{bzip2}, \command{lzma} or \command{xz} are also understood (but may
#' be excluded using \code{include} and/or \code{exclude}).
#'
#' @param names Character vector with names of files in one of the formats
#'   accepted by \code{\link{read_single_opm}}, or names of directories
#'   containing such files, or both; or convertible to such a vector. See the
#'   \code{include} argument of \code{\link{read_opm}} and
#'   \code{\link{explode_dir}} for how to select subsets from the input files or
#'   directories.
#'
#' @param convert Character scalar with one of the following values:
#'   \describe{
#'   \item{no}{Always return a list (of \code{\link{OPM}} objects).}
#'   \item{yes}{Convert to \code{NULL}, \code{\link{OPM}} object, or
#'   \code{\link{OPMS}} object, depending on the number of files read (0, 1, or
#'   more).}
#'   \item{try}{Behave like \sQuote{yes} but do not result in an error message
#'   if conversion to OPMS is impossible; return a list in that case.}
#'   \item{sep}{Return a nested list, each sublist containing \code{\link{OPM}}
#'   objects of the same plate type.}
#'   \item{grp}{Also split into such sublists but convert them to
#'   \code{\link{OPMS}} objects if more than one plate is encountered. An error
#'   is raised if this is impossible (in contrast to \sQuote{try}).}
#'   }
#' @param gen.iii Logical or character scalar. If \code{TRUE}, invoke
#'   \code{\link{gen_iii}} on each plate. This is automatically done with
#'   \acronym{CSV} input if the plate type is given as \sQuote{OTH} (which is
#'   usually the case for plates run in ID mode). If a character scalar, it is
#'   used as the \code{to} argument of \code{\link{gen_iii}} to set other plate
#'   types unless it is empty.
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
#' @return \code{\link{OPM}} object (maybe \code{\link{OPMA}} in case of
#'   \acronym{YAML} input), or list of such objects, or \code{\link{OPMS}}
#'   object. If \code{demo} is \code{TRUE}, a character vector instead.
#'
#' @details Regarding the \acronym{CSV} format, see the remark to
#'   \code{\link{read_single_opm}}. The \acronym{YAML} format is described in
#'   detail under \code{\link{batch_opm}}.
#'
#'   or splitting lists of \code{\link{OPM}} objects according to the plate
#'   type, see \code{\link{plate_type}}, and consider the plate-type selection
#'   options of \code{\link{opms}}.
#'
#' @export
#' @family io-functions
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.json.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso utils::read.csv yaml::yaml.load_file
#' @keywords IO
#' @examples
#' test.files <- grep("Multiple|Ecoplate", opm_files("testdata"), invert = TRUE,
#'   value = TRUE, perl = TRUE)
#' if (length(test.files) > 0) { # if the folder is found
#'   x <- read_opm(test.files, demo = TRUE) # check first what you would get
#'   stopifnot(identical(test.files, x))
#'   x <- read_opm(test.files[1:2]) # these two have the same plate type
#'   class(x)
#'   dim(x)
#'   summary(x)
#'   stopifnot(is(x, "OPMS"), identical(dim(x), c(2L, 384L, 96L)))
#' } else {
#'   warning("test files not found")
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
read_opm <- function(names, convert = c("try", "no", "yes", "sep", "grp"),
    gen.iii = opm_opt("gen.iii"), include = list(), ..., demo = FALSE) {
  do_split <- function(x) split(x, vapply(x, plate_type, ""))
  do_opms <- function(x) case(length(x), , x[[1L]], new(OPMS, plates = x))
  convert <- match.arg(convert)
  LL(gen.iii, demo)
  names <- explode_dir(names = names, include = include, ...)
  if (demo) {
    message(paste(names, collapse = "\n"))
    return(invisible(names))
  }
  # The c() call is necessary to flatten lists from YAML/JSON input.
  result <- c(lapply(names, read_single_opm), recursive = TRUE)
  switch(mode(gen.iii),
    logical = if (gen.iii)
      result <- lapply(result, gen_iii),
    character = if (nzchar(gen.iii))
      result <- lapply(result, gen_iii, to = gen.iii),
    stop("'gen.iii' must either be logical or character scalar")
  )
  case(length(result),
    switch(convert, no = result, NULL),
    switch(convert, no = result, result[[1L]]),
    case(convert,
      no = result,
      yes = new(OPMS, plates = result),
      grp = lapply(do_split(result), do_opms),
      sep = do_split(result),
      try = tryCatch(new(OPMS, plates = result), error = function(e) {
        warning("the data from distinct files could not be converted to a ",
          "single OPMS object and will be returned as a list")
        result
      })
    )
  )
}


################################################################################
################################################################################
#
# Metadata IO
#


#' Input metadata
#'
#' Create data frame holding potential \code{\link{OPM}} or \code{\link{OPMS}}
#' object metadata.
#'
#' @param object Name of input file (character scalar), or any object
#'   convertible to a data frame. Might also be \code{\link{WMD}} or
#'   \code{\link{OPMS}} object.
#' @param stringsAsFactors Logical scalar passed to \code{as.data.frame}.
#' @param optional Logical scalar passed to \code{as.data.frame} or
#'   \code{read.delim}.
#' @param sep Character scalar. Field separator in input file. This and the
#'   following parameters are passed to \code{read.delim}.
#' @param check.names Logical scalar.
#' @param strip.white Logical scalar.
#' @param ... Optional other arguments for \code{read.delim} or
#'   \code{as.data.frame}.
#' @export
#' @return Data frame.
#' @details The character method reads metadata from an input file and is only a
#'   thin wrapper for \code{read.delim} but contains some useful adaptations
#'   (such as \strong{not} converting strings to factors, and not modifying
#'   column names). The default method reads metadata from an object convertible
#'   to a data frame and is only a thin wrapper of \code{as.data.frame} but
#'   contains the same useful adaptations as the filename method.
#'
#'   The \code{\link{WMD}} and \code{\link{OPMS}} methods create a data frame
#'   from the contained metadata, where necessary converting nested metadata
#'   entries to data-frame columns of mode \sQuote{list}. The number of rows
#'   of the resuling data frame corresponds to the length of \code{object}, the
#'   number of columns to the size of the set created from all valid names at
#'   the top level of the metadata entries.
#'
#' @family io-functions
#' @keywords IO manip
#' @seealso base::default.stringsAsFactors utils::read.delim base::as.data.frame
#' @examples
#'
#' # Character method
#' (x <- to_metadata(list(a = 7:8, `b c` = letters[1:2])))
#' tmpfile <- tempfile()
#' write.table(x, tmpfile, row.names = FALSE, sep = "\t")
#' (x1 <- read.delim(tmpfile)) # comparison with base R function
#' (x2 <- to_metadata(tmpfile))
#' stopifnot(identical(names(x2), names(x)), !identical(names(x1), names(x)))
#'
#' # Default method
#' x <- list(a = 7:8, `b c` = letters[1:2])
#' (x1 <- as.data.frame(x))
#' (x2 <- to_metadata(x))
#' stopifnot(!identical(names(x), names(x1)), identical(names(x), names(x2)))
#'
#' # WMD method
#' data(vaas_1)
#' (x <- to_metadata(vaas_1)) # one row per OPM object
#' stopifnot(is.data.frame(x), nrow(x) == length(vaas_1), ncol(x) > 0)
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- to_metadata(vaas_4)) # one row per OPM object
#' stopifnot(is.data.frame(x), nrow(x) == length(vaas_4), ncol(x) > 0)
#' copy <- vaas_4
#' metadata(copy) <- x
#' stopifnot(identical(copy, vaas_4))
#' # ... this works only in the special case of non-nested metadata that
#' # have the same set of entries in all OPMS elements
#'
setGeneric("to_metadata",
  function(object, ...) standardGeneric("to_metadata"))

setMethod("to_metadata", "character", function(object, sep = "\t",
    check.names = !optional, strip.white = TRUE, stringsAsFactors = FALSE,
    optional = TRUE, ...) {
  if (!missing(check.names))
    warning("'check.names' is deprecated, use 'optional'")
  read.delim(file = L(object), sep = sep, check.names = check.names,
    strip.white = strip.white, stringsAsFactors = stringsAsFactors, ...)
}, sealed = SEALED)

setMethod("to_metadata", "ANY", function(object, stringsAsFactors = FALSE,
    optional = TRUE, ...) {
  as.data.frame(x = object, stringsAsFactors = stringsAsFactors,
    optional = optional, ...)
}, sealed = SEALED)

setMethod("to_metadata", WMD, function(object, stringsAsFactors = FALSE,
    optional = TRUE, ...) {
  md_data_frame(list(object@metadata), stringsAsFactors, optional, ...)
}, sealed = SEALED)

setMethod("to_metadata", OPMS, function(object, stringsAsFactors = FALSE,
    optional = TRUE, ...) {
  md_data_frame(metadata(object), stringsAsFactors, optional, ...)
}, sealed = SEALED)


################################################################################
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
#' @param names Character vector with file or directory names, or convertible to
#'   such. See \code{\link{explode_dir}} for details.
#' @param fun Collecting function. Should use the filename as first argument.
#' @param fun.args Optional list of arguments to \code{fun}.
#' @param ... Optional further arguments passed to \code{\link{explode_dir}}.
#' @param proc Integer scalar. The number of processes to spawn. Cannot be set
#'   to more than 1 core if running under Windows. See the \code{cores}
#'   argument of \code{\link{do_aggr}} for details.
#' @param simplify Logical scalar. Should the resulting list be simplified to a
#'   vector or matrix if possible?
#' @param use.names Logical scalar. Should \code{names} be used for naming the
#'   elements of the result?
#' @export
#' @family io-functions
#' @return List, potentially simplified to a vector, depending on the output of
#'   \code{fun} and the value of \code{simplify}. See also \code{demo}.
#' @keywords IO
#' @examples
#' # Read the first line from each of the OPM test dataset files
#' f <- opm_files("testdata")
#' if (length(f) > 0) { # if the files are found
#'   x <- batch_collect(f, fun = readLines, fun.args = list(n = 1L))
#'   # yields a list with the input files as names and the result from each
#'   # file as values (exactly one line)
#'   stopifnot(is.list(x), identical(names(x), f))
#'   stopifnot(sapply(x, is.character), sapply(x, length) == 1L)
#' } else {
#'   warning("test files not found")
#' }
#'
batch_collect <- function(names, fun, fun.args = list(), proc = 1L, ...,
    use.names = TRUE, simplify = FALSE, demo = FALSE) {
  names <- explode_dir(names, ...)
  if (demo) {
    message(paste(names, collapse = "\n"))
    return(invisible(names))
  }
  fun.args <- as.list(fun.args)
  mcmapply(FUN = fun, names, MoreArgs = as.list(fun.args), SIMPLIFY = simplify,
    USE.NAMES = use.names, mc.cores = proc)
}


################################################################################


#' Collect metadata template
#'
#' Collect a metadata template from
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV} comments
#' assisting in later on adding metadata using  \code{\link{include_metadata}}.
#'
#' @param object Character vector or \code{\link{OPM}} or \code{\link{OPMS}}
#'   object. The \code{\link{OPM}} and \code{\link{OPMS}} methods just collect a
#'   data frame from their input object.
#'
#'   If a character vector is provided, it acts like the \code{names}
#'   argument of \code{\link{read_opm}}. That is, if it is a directory name,
#'   this is automatically scanned for all \acronym{CSV} and \acronym{YAML}
#'   files it contains (unless restrictions with patterns are made). One can
#'   also provide file names, or a mixture of file and directory names.
#'   Regarding the supported input file formats, see
#'   \code{\link{read_single_opm}}.
#'
#' @param outfile Character scalar. Ignored if \code{NULL} or empty string.
#'   Otherwise, interpreted as the name of a \acronym{CSV} output file. If
#'   metadata have already been collected in an older file with the same name,
#'   old metadata will be kept, identifiers for novel files will be included,
#'   their so far empty entries set to \code{NA}. Users who wish to keep the old
#'   version can use two distinct names for novel and old files; see
#'   \code{previous}.
#' @param sep Character scalar. \acronym{CSV} field separator for
#'   \code{outfile}.
#' @param previous Ignored if \code{NULL}. Otherwise passed to
#'   \code{\link{to_metadata}}. If it is a filename different from
#'   \code{outfile}, it is an error if the file does not exist.
#' @param md.args List of other arguments passed to the \sQuote{to_metadata}
#'   methods.
#' @param add.cols Optional character vector with the names of columns to be
#'   added to the result, or \code{NULL}. If not empty, names of columns to be
#'   added, initially containing \code{NA}.
#' @param selection Elements to be extracted from the \acronym{CSV} comments
#'   contained in each file. Character vector passed to \code{\link{csv_data}}.
#' @param include File inclusion pattern (or generator for a pattern). Passed to
#'   \code{\link{batch_collect}}.
#' @param ... Other arguments passed to \code{\link{batch_collect}}, or
#'   arguments passed from the \code{\link{OPMS}} to the \code{\link{OPM}}
#'   method.
#' @param demo Logical scalar. Run in \sQuote{demo} mode? Also passed to
#'   \code{\link{batch_collect}}.
#' @export
#' @return In the case of the character method, a data frame, returned invisibly
#'   if \code{outfile} is given; if \code{demo} is \code{TRUE}, a character
#'   vector of filenames instead, also returned invisibly. The \code{\link{OPM}}
#'   method returns a data frame with one row and the number of columns equal to
#'   the sum of the lengths of \code{selection} and \code{add.cols}. The
#'   \code{\link{OPM}} method returns such a data frame with one row per
#'   contained plate.
#' @details The character method batch-collects templates for meta-information
#'   from files and optionally add these data as novel rows to previously
#'   collected data. It writes the collected template to a file for use with an
#'   external editor, and/or creates a data frame for editing the data directly
#'   in \R with the \code{edit} function.
#'
#' @seealso utils::edit utils::read.delim
#' @family io-functions
#' @references \url{http://www.biolog.com/}
#' @keywords IO attribute
#' @examples
#'
#' # Character method
#' test.files <- grep("Multiple|Ecoplate", opm_files("testdata"), invert = TRUE,
#'   value = TRUE, perl = TRUE)
#' if (length(test.files) > 0) { # if the files are found
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
#' } else {
#'   warning("test files not found")
#' }
#'
#' # OPM method
#' data(vaas_1)
#' (x <- collect_template(vaas_1)) # => data frame, one row per plate
#' stopifnot(identical(dim(x), c(1L, 3L)))
#' (x <- collect_template(vaas_1, add.cols = c("A", "B")))
#' stopifnot(identical(dim(x), c(1L, 5L))) # => data frame with more columns
#' # see include_metadata() for how to use this to add metadata information
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- collect_template(vaas_4)) # => data frame, one row per plate
#' stopifnot(identical(dim(x), c(4L, 3L)))
#' (x <- collect_template(vaas_4, add.cols = c("A", "B")))
#' stopifnot(identical(dim(x), c(4L, 5L))) # => data frame with more columns
#' # again see include_metadata() for how to use this to add metadata
#' # information
#'
setGeneric("collect_template",
  function(object, ...) standardGeneric("collect_template"))

setMethod("collect_template", "character", function(object, outfile = NULL,
    sep = "\t", previous = outfile, md.args = list(),
    selection = opm_opt("csv.selection"), add.cols = NULL,
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
        stop(conditionMessage(e))
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

setMethod("collect_template", OPM, function(object,
    selection = opm_opt("csv.selection"), add.cols = NULL) {
  result <- as.list(csv_data(object, selection))
  result <- as.data.frame(result, stringsAsFactors = FALSE, optional = TRUE)
  if (length(add.cols) > 0L) {
    to.add <- matrix(nrow = nrow(result), ncol = length(add.cols),
      dimnames = list(NULL, add.cols), data = NA_character_)
    result <- cbind(result, to.add, stringsAsFactors = FALSE)
  }
  result
}, sealed = SEALED)

setMethod("collect_template", OPMS, function(object, ...) {
  result <- lapply(object@plates, collect_template, ...)
  do.call(rbind, result)
}, sealed = SEALED)


################################################################################
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
#'   tried if \code{infile} exists and is not empty. If \sQuote{no}, conversion
#'   is not tried if \code{outfile} exists and is not empty. If \sQuote{older},
#'   conversion is tried if \code{outfile} does not exist or is empty or is
#'   older than \code{infile} (with respect to the modification time).
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
      case(overwrite,
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
          "could not delete outfile"
      )
  }
  conduct_conversion <- function(infile, outfile, fun, fun.args) {
    if (!create_parent(outfile))
      return("could not create parent directory")
    problem <- tryCatch({
      do.call(fun, c(infile = infile, outfile = outfile, fun.args))
      ""
    }, error = conditionMessage)
    if (nzchar(problem))
      problem
    else if (empty(file.info(outfile)))
      "outfile not created or empty"
    else
      "ok"
  }
  LL(files, .wanted = 2L)
  overwrite <- match.arg(overwrite)
  result <- list(infile = files[1L], outfile = files[2L], before = "",
    after = "")
  result$before <- prepare_conversion(files[1L], files[2L], overwrite)
  if (!nzchar(result$before)) {
    result$before <- "attempt to create outfile"
    result$after <- conduct_conversion(files[1L], files[2L], io.fun, fun.args)
  }
  if (verbose) {
    lapply(formatDL(unlist(result), style = "list"), message)
    message("")
  }
  unlist(result)
}


################################################################################


## NOTE: not an S4 method because conversion is done

#' Convert infiles to outfiles
#'
#' Batch-convert data from infiles to data in outfiles. This is not normally
#' directly called by an \pkg{opm} user because \code{\link{batch_opm}}
#' is available.
#'
#' @inheritParams process_io
#' @inheritParams batch_collect
#' @param out.ext Character scalar. The extension of the outfile names (without
#'   the dot).
#' @param outdir Character vector. Directories in which to place the outfiles.
#'   If \code{NULL} or only containing empty strings, each infile's directory is
#'   used.
#' @param in.ext Character scalar. Passed through \code{\link{file_pattern}},
#'   then used for the replacement of old file extensions with new ones.
#' @param compressed Logical scalar. Passed as 2nd argument to
#'   \code{\link{file_pattern}}.
#' @param literally Logical scalar. Passed as 3rd argument to
#'   \code{\link{file_pattern}}.
#' @param demo Logical scalar. Do not convert files, but print the attempted
#'   infile-outfile conversions and invisibly return a matrix with infiles in
#'   the first and outfiles in the second column?
#' @export
#' @return In normal mode, an invisibly returned character matrix in which each
#'   row corresponds to a named character vector with the keys \sQuote{infile},
#'   \sQuote{outfile}, \sQuote{before} and \sQuote{after}. The latter two
#'   describe the result of the action(s) before and after attempting to convert
#'   \code{infile} to \code{outfile}. \sQuote{after} is the empty string if no
#'   conversion was tried (see \code{overwrite}), \sQuote{ok} if conversion was
#'   successful and a message describing the problems otherwise. For the results
#'   of the \code{demo} mode see above.
#' @keywords IO
#' @family io-functions
#' @examples
#' # Read the first line from each of the OPM test dataset files and store it
#' # in temporary files
#' pf <- function(infile, outfile) write(readLines(infile, n = 1), outfile)
#' infiles <- opm_files("testdata")
#' if (length(infiles) > 0) { # if the files are found
#'   x <- batch_process(infiles, out.ext = "tmp", io.fun = pf,
#'     outdir = tempdir())
#'   stopifnot(is.matrix(x), identical(x[, 1], infiles))
#'   stopifnot(file.exists(x[, 2]))
#'   unlink(x[, 2])
#' } else {
#'   warning("test files not found")
#' }
#'
batch_process <- function(names, out.ext, io.fun, fun.args = list(), proc = 1L,
    outdir = NULL, overwrite = c("yes", "older", "no"), in.ext = "any",
    compressed = TRUE, literally = inherits(in.ext, "AsIs"), ...,
    verbose = TRUE, demo = FALSE) {
  create_outfile_names <- function(infiles, outdir, out.ext) {
    if (length(outdir) == 0L || all(!nzchar(outdir)))
      outdir <- dirname(infiles)
    result <- sub(in.ext, "", basename(infiles), perl = TRUE,
      ignore.case = TRUE)
    result <- paste(result, sub("^\\.+", "", out.ext), sep = ".")
    file.path(outdir, result)
  }
  LL(demo, verbose, compressed)
  in.ext <- file_pattern(in.ext, compressed, literally)
  overwrite <- match.arg(overwrite)
  infiles <- explode_dir(names, ...)
  outfiles <- create_outfile_names(infiles, outdir, out.ext)
  if (demo) {
    message(paste(infiles, outfiles, sep = "\n  => ", collapse = "\n"))
    return(invisible(cbind(infiles, outfiles)))
  }
  fun.args <- as.list(fun.args)
  data <- mapply(c, infiles, outfiles, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  result <- mclapply(X = data, FUN = process_io, mc.cores = proc,
    io.fun = io.fun, fun.args = fun.args, overwrite = overwrite,
    verbose = verbose)
  invisible(do.call(rbind, result))
}


################################################################################
################################################################################
#
# Batch IO with OPM objects
#


## NOTE: not an S4 method because conversion is done

#' Batch-convert PM data
#'
#' Batch-convert from OmniLog\eqn{\textsuperscript{\textregistered}}{(R)}
#' \acronym{CSV} (or previous \pkg{opm} \acronym{YAML} or \acronym{JSON}) to
#' \pkg{opm} \acronym{YAML} (or \acronym{JSON}). It is possible to add metadata
#' to each set of raw data and to aggregate the curves; these additional data
#' will then be included in the output files.
#'
#' @inheritParams read_opm
#' @inheritParams batch_process
#' @param md.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{include_metadata}} with the data read from each individual file
#'   as additional argument \sQuote{object}. If \code{NULL}, metadata are not
#'   included (but may already be present in the case of \acronym{YAML} input).
#' @param aggr.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{do_aggr}} with the data read from each individual file as
#'   additional argument \code{object}.  If \code{NULL}, aggregation takes not
#'   place (but aggregated data may already be present in case of \acronym{YAML}
#'   input).
#' @param force.aggr Logical scalar. If \code{FALSE}, do not aggregate already
#'   aggregated data (which can be present in \acronym{YAML} input).
#' @param disc.args If not \code{NULL} but a list, passed as arguments to
#'   \code{\link{do_disc}} with the data read from each individual file as
#'   additional argument \code{object}.  If \code{NULL}, discretization takes
#'   not place (but discretized data may already be present in case of
#'   \acronym{YAML} input).
#' @param force.disc Logical scalar. If \code{FALSE}, do not discretize already
#'   discretized data (which can be present in \acronym{YAML} input).
#' @param device Character scalar describing the graphics device used for
#'   outputting plots. See \code{Devices} from the \pkg{grDevices} package and
#'   \code{mypdf} from the \pkg{pkgutils} package for possible values. The
#'   extension of the output files is created from the device name after a few
#'   adaptations (such as converting \sQuote{postscript} to \sQuote{ps}).
#' @param dev.args List. Passed as additional arguments to \code{device}.
#' @param plot.args List. Passed as additional arguments to the plotting
#'   function used.
#' @param csv.args If not \code{NULL} but a list, used for specifying ways to
#'   use \code{\link{csv_data}} entries directly as \code{\link{metadata}}. The
#'   list can contain character vectors used for selecting and optionally
#'   renaming \acronym{CSV} entries or functions that can be applied to an
#'   entire data frame containing all \acronym{CSV} entries. Note that this
#'   argument has nothing to do with \sQuote{csv} output.
#' @param table.args Passed to \code{write.table} from the \pkg{utils} package
#'   if \code{output} is set to \sQuote{csv}. Do not confuse this with
#'   \code{csv.args}.
#' @param ... Optional arguments passed to \code{\link{batch_process}} in
#'   addition to \code{verbose} and \code{demo}. Note that \code{out.ext},
#'   \code{fun} and \code{fun.args} are set automatically. Alternatively,
#'   these are parameters passed to \code{\link{batch_collect}}.
#' @param output Character scalar determining the main output mode. \describe{
#'   \item{clean}{Apply \code{\link{clean_filenames}}.}
#'   \item{csv}{Create \acronym{CSV} files, by default one per input file.}
#'   \item{json}{Create \acronym{JSON} files, by default one per input file.}
#'   \item{levelplot}{Create graphics files, by default one per input file,
#'     containing the output of \code{\link{level_plot}}.}
#'   \item{split}{Split multi-plate new style or old style \acronym{CSV} files
#'     with \code{\link{split_files}}.}
#'   \item{yaml}{Create \acronym{YAML} files, by default one per input file.}
#'   \item{xyplot}{Create graphics files, by default one per input file,
#'     containing the output of \code{\link{xy_plot}}.}
#'   }
#' @param combine.into Empty or character scalar modifying the output mode
#'   unless it is \sQuote{clean} or \sQuote{split}. If non-empty, causes the
#'   creation of a single output file named per plate type encountered in the
#'   input, instead of one per input file (the default). Thus
#'   \code{combine.into} should be given as a template string for \code{sprintf}
#'   from the \pkg{base} package with one placeholder for the plate-type, and
#'   without a file extension.
#' @export
#' @note This function is for batch-converting many files; for writing a single
#'   object to a \acronym{YAML} file (or string), see \code{\link{to_yaml}}.
#' @return The function invisibly returns a matrix which describes each
#'   attempted file conversion. See \code{\link{batch_process}} for details.
#' @family io-functions
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.json.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso utils::read.csv yaml::yaml.load_file grDevices::Devices
#' @seealso pkgutils::mypdf
#' @keywords IO
#'
#' @details
#'   A \acronym{YAML} document can comprise \emph{scalars} (single values of
#'   some type), \emph{sequences} (ordered collections of some values, without
#'   names) and \emph{mappings} (collections assigning a name to each value),
#'   in a variety of combinations (e.g., mappings of sequences). The output
#'   of \code{batch_opm} is one \acronym{YAML} document \emph{per plate}
#'   which represents a mapping with the following components (key-value pairs):
#'   \describe{
#'     \item{metadata}{Arbitrarily nested mapping of arbitrary metadata
#'       entries. Empty if no metadata have been added.}
#'     \item{csv_data}{Non-nested mapping containing the
#'       OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} run information
#'       read from the input \acronym{CSV} file (character scalars) together
#'       with the measurements. The most important entry is most likely the
#'       plate type.}
#'     \item{measurements}{A mapping whose values are sequences of
#'       floating-point numbers of the same length and in the appropriate order.
#'       The keys are \sQuote{hours}, which refers to the time points, and the
#'       well coordinates, ranging between \sQuote{A01} and \sQuote{H12}.}
#'     \item{aggregated}{A mapping, only present if curve parameters have been
#'       estimated. Its keys correspond to those of \sQuote{measurements} with
#'       the exception of \sQuote{hours}. The values are themselves mappings,
#'       whose keys indicate the respective curve parameter and whether this is
#'       the point estimate or the upper or lower confidence interval. The
#'       values of these secondary mappings are floating-point numbers.}
#'     \item{aggr_settings}{A mapping, only present if curve parameters have
#'       been estimated. Its keys are \sQuote{software}, \sQuote{version} and
#'       \sQuote{options}. The value of the former two is a character scalar.
#'       The value of \sQuote{options} is an arbitrarily nested mapping with
#'       arbitrary content.}
#'     \item{discretized}{A mapping, only present if curve parameters have been
#'       estimated and also discretized. Its keys correspond to those of
#'       \sQuote{measurements} with the exception of \sQuote{hours}. The values
#'       are logical scalars.}
#'     \item{disc_settings}{A mapping, only present if curve parameters have
#'       been estimated and also discretized. Its keys are \sQuote{software},
#'       \sQuote{version} and \sQuote{options}. The value of the former two is a
#'       character scalar. The value of \sQuote{options} is an arbitrarily
#'       nested mapping with arbitrary content.}
#'   }
#'   Details of the contents should be obvious from the documentation of the
#'   classes of the objects from which the \acronym{YAML} output is generated.
#'   In the case of \acronym{YAML} input with several plates per file,
#'   \code{batch_opm} generates \acronym{YAML} output files containing a
#'   sequence of mappings as described above, one per plate, to keep a 1:1
#'   relationship between input and output files.
#'
#'   Attempting to generate \acronym{YAML} from input data with a wrong
#'   character encoding might cause \R to crash or hang. This problem was
#'   observed with \acronym{CSV} files that were generated on a distinct
#'   operating system and contained special characters such as German umlauts.
#'   It is then necessary to explicitely (and correctly) specify the encoding
#'   used in these files; see the \sQuote{file.encoding} option of
#'   \code{\link{opm_opt}} for how to do this.
#'
#'   \acronym{JSON}, which is almost a subset of \acronym{YAML}, can also be
#'   generated, but has more restrictions. It is only recommended if a
#'   \acronym{YAML} parser is unavailable. It is also more delicate regarding
#'   the encoding of character strings.
#'
#'   When inputting \acronym{YAML} files generated with the help of the
#'   \pkg{yaml} package (on which the \pkg{opm} implementation is based), or
#'   \acronym{JSON} files generated with the help of the \pkg{rjson} package,
#'   using other programming languages, a potential problem is that they, and
#'   \acronym{YAML} in general, lack a native representation of \code{NA}
#'   values. Such entries are likely to be misunderstood as \sQuote{NA}
#'   character scalars (if the \pkg{json} package or the \pkg{yaml} package
#'   prior to version 2.1.7 are used) or as \sQuote{.na}, \sQuote{.na.real},
#'   \sQuote{.na.logical} or \sQuote{.na.character} character scalars (if more
#'   recent versions of the \pkg{yaml} package are used). Input functions in
#'   other programming languages should conduct according conversions. \pkg{opm}
#'   translates these values when converting a list to a \code{\link{OPM}}
#'   object.
#'
#'   See \code{\link{as.data.frame}} regarding the generated \acronym{CSV}.
#'
#' @examples
#' test.files <- grep("Multiple", opm_files("testdata"), invert = TRUE,
#'   value = TRUE, fixed = TRUE)
#' if (length(test.files) > 0) { # if the files are found
#'   num.files <- length(list.files(outdir <- tempdir()))
#'   x <- batch_opm(test.files[1], outdir = outdir)
#'   stopifnot(length(list.files(outdir)) == num.files + 1, is.matrix(x))
#'   stopifnot(file.exists(x[, "outfile"]))
#'   stopifnot(test.files[1] == x[, "infile"])
#'   unlink(x[, "outfile"])
#' } else {
#'   warning("opm example data files not found")
#' }
#'
batch_opm <- function(names, md.args = NULL, aggr.args = NULL,
    force.aggr = FALSE, disc.args = NULL, force.disc = FALSE,
    gen.iii = opm_opt("gen.iii"), device = "mypdf", dev.args = NULL,
    plot.args = NULL, csv.args = NULL,
    table.args = list(sep = "\t", row.names = FALSE),
    ..., proc = 1L, outdir = "", overwrite = "no",
    output = c("yaml", "json", "csv", "xyplot", "levelplot", "split", "clean"),
    combine.into = NULL, verbose = TRUE, demo = FALSE) {

  csv2md <- function(x, spec) {
    if (!is.matrix(x)) # OPM objects yield only a character vector
      x <- t(as.matrix(x))
    x <- to_metadata(x)
    spec <- flatten(list(spec))
    spec <- rapply(spec, as.character, "factor", NULL, "replace")
    if (any(!vapply(spec, inherits, NA, c("character", "function"))))
      stop("can only apply character vector, factor or function to CSV data")
    for (approach in spec)
      if (is.character(approach)) {
        for (name in approach[!approach %in% colnames(x)])
          x[, name] <- seq_len(nrow(x))
        x <- x[, approach, drop = FALSE]
        if (!is.null(names(approach)))
          colnames(x) <- names(approach)
      } else {
        x <- approach(x)
        if (!is.data.frame(x)) # wrong no. of rows should yield error later on
          stop("function applied to CSV data must yield data frame")
      }
    x
  }

  convert_dataset <- function(data) {
    switch(mode(gen.iii),
      logical = if (gen.iii) {
        if (verbose)
          message("conversion: changing to 'Generation III'...")
        data <- gen_iii(data)
      },
      character = if (nzchar(gen.iii)) {
        if (verbose)
          message(sprintf("conversion: changing to '%s'...", gen.iii))
        data <- gen_iii(data, to = gen.iii)
      },
      stop("'gen.iii' must either be a logical or a character scalar")
    )
    if (length(csv.args)) {
      if (verbose)
        message("conversion: using CSV data as metadata...")
      metadata(data, 1L) <- csv2md(csv_data(data), csv.args)
    }
    if (length(md.args)) {
      if (verbose)
        message("conversion: including metadata...")
      data <- do.call(include_metadata, c(object = data, md.args))
    }
    if (length(aggr.args)) {
      if (force.aggr || !has_aggr(data)) {
        if (verbose)
          message("conversion: aggregating data...")
        data <- do.call(do_aggr, c(list(object = data), aggr.args))
      } else if (verbose)
        message("conversion: previously aggregated data present, ",
          "skipping that step")
    }
    if (length(disc.args)) {
      if (force.aggr || !has_disc(data)) {
        if (verbose)
          message("conversion: discretizing data")
        data <- do.call(do_disc, c(list(data), disc.args))
      } else if (verbose)
        message("conversion: previously discretized data present, ",
          "skipping that step")
    }
    data
  }

  read_file <- function(infile) {
    data <- read_single_opm(infile)
    if (is.list(data)) # YAML input can result in lists of several OPM objects
      lapply(data, convert_dataset)
    else
      convert_dataset(data)
  }

  create_yaml <- function(x, outfile) {
    if (is.list(x)) # would be more elegant if to_yaml() could handle that
      x <- lapply(x, as, "list")
    write(to_yaml(x, json = json), outfile)
  }
  convert_to_yaml <- function(infile, outfile) {
    create_yaml(read_file(infile), outfile)
  }

  create_csv <- function(x, outfile) {
    if (is.list(x))
      x <- do.call(rbind, lapply(x, as.data.frame))
    else
      x <- as.data.frame(x)
    do.call(write.table, c(list(x = x, file = outfile), as.list(table.args)))
  }
  convert_to_csv <- function(infile, outfile) {
    create_csv(read_file(infile), outfile)
  }

  create_plot <- function(x, outfile) {
    do.call(device, c(list(file = outfile), dev.args))
    print(do.call(plot.type, c(list(x = x), plot.args)))
    dev.off()
  }
  create_plot_from_file <- function(infile, outfile) {
    data <- read_file(infile)
    if (is.list(data))
      data <- do.call(c, data)
    create_plot(data, outfile)
  }

  convert_to_single_file <- function(names, outfile.template, out.ext, demo,
      verbose, ..., proc) {
    x <- read_opm(names = names, convert = "grp", ..., demo = demo)
    if (demo) {
      if (verbose)
        message(paste0(x, collapse = "\n"))
      return(invisible(x))
    }
    out.names <- gsub(" ", "-", names(x), fixed = TRUE)
    out.names <- paste(sprintf(outfile.template, out.names), out.ext, sep = ".")
    x <- mclapply(x, convert_dataset, mc.cores = proc)
    mcmapply(create_single_file, x, out.names, mc.cores = proc)
    names(out.names) <- names(x)
    if (verbose)
      message(listing(out.names))
    out.names
  }

  LL(force.aggr, force.disc, gen.iii, device, overwrite)

  # If a metadata filename is given, read it into data frame right now to
  # avoid opening the file each time in the batch_process() loop
  if (length(md.args) && is.character(md.args$md)) {
    tmp <- md.args
    names(tmp)[names(tmp) == "md"] <- "object"
    tmp$replace <- NULL
    md.args$md <- do.call(to_metadata, tmp)
  }

  case(output <- match.arg(output),
    yaml = {
      collect <- FALSE
      io.fun <- convert_to_yaml
      create_single_file <- create_yaml
      json <- FALSE
      in.ext <- "both"
      out.ext <- "yml"
    },
    json = {
      collect <- FALSE
      io.fun <- convert_to_yaml
      create_single_file <- create_yaml
      json <- TRUE
      in.ext <- "both"
      out.ext <- "json"
    },
    csv = {
      collect <- FALSE
      io.fun <- convert_to_csv
      create_single_file <- create_csv
      json <- NULL
      in.ext <- "both"
      out.ext <- "tab"
    },
    levelplot = {
      collect <- FALSE
      io.fun <- create_plot_from_file
      create_single_file <- create_plot
      json <- disc.args <- aggr.args <- NULL
      in.ext <- "both"
      out.ext <- map_values(device, GRAPHICS_FORMAT_MAP)
      plot.type <- level_plot
    },
    xyplot = {
      collect <- FALSE
      io.fun <- create_plot_from_file
      create_single_file <- create_plot
      json <- disc.args <- aggr.args <- NULL
      in.ext <- "both"
      out.ext <- map_values(device, GRAPHICS_FORMAT_MAP)
      plot.type <- xy_plot
    },
    split = {
      collect <- TRUE
      io.fun <- split_files
      in.ext <- "csv"
      fun.args <- list(pattern = '^("Data File",|Data File)', outdir = outdir,
        demo = demo)
    },
    clean = {
      collect <- TRUE
      io.fun <- clean_filenames
      in.ext <- "both"
      fun.args <- list(demo = demo, overwrite = overwrite == "yes")
    }
  )

  if (collect) # the functions have their own 'demo' argument
    invisible(batch_collect(names = names, fun = io.fun, fun.args = fun.args,
      proc = proc, ..., demo = FALSE))
  else if (length(combine.into))
    invisible(convert_to_single_file(names = names, out.ext = out.ext, ...,
      outfile.template = combine.into, demo = demo, verbose = verbose,
      proc = proc))
  else
    batch_process(names = names, out.ext = out.ext, io.fun = io.fun,
      in.ext = in.ext, compressed = TRUE, literally = FALSE, ..., proc = proc,
      overwrite = overwrite, outdir = outdir, verbose = verbose, demo = demo)
}


################################################################################


#' Batch-convert to YAML (deprecated)
#'
#' \strong{Deprecated} function to batch-convert from
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV} (or
#' previous \pkg{opm} \acronym{YAML}) to \pkg{opm} \acronym{YAML}. Use
#' \code{\link{batch_opm}} instead.
#'
#' @inheritParams batch_opm
#' @export
#' @note This \strong{deprecated} function is for batch-converting many files;
#'   for writing a single object to a \acronym{YAML} file (or string), see
#'   \code{\link{to_yaml}}.
#' @return The function invisibly returns a matrix which describes each
#'   attempted file conversion. See \code{\link{batch_process}} for details.
#' @family io-functions
#' @references \url{http://www.yaml.org/}
#' @references \url{http://www.biolog.com/}
#' @seealso utils::read.csv yaml::yaml.load_file
#' @keywords IO
#'
#' @details
#'   This function is \strong{deprecated}; use \code{\link{batch_opm}} instead.
#'
#' @examples
#' # see batch_opm()
#'
batch_opm_to_yaml <- function(names, md.args = NULL, aggr.args = NULL,
    force.aggr = FALSE, disc.args = NULL, force.disc = FALSE,
    gen.iii = opm_opt("gen.iii"), ..., verbose = TRUE, demo = FALSE) {
  message("this function is deprecated; use batch_opm() instead")
  batch_opm(names = names, md.args = md.args, aggr.args = aggr.args,
    force.aggr = force.aggr, disc.args = disc.args, force.disc = force.disc,
    gen.iii = gen.iii, ..., output = "yaml", verbose = verbose, demo = demo)
}


################################################################################
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
#'   the non-separator lines if otherwise.
#'
#'   Conceptually each of the sections into which a file is split comprises a
#'   separator line followed by non-separator lines. That is, separator lines
#'   followed by another separator line are ignored. Non-separator lines not
#'   preceded by a separator line are treated as a section of their own,
#'   however.
#'
#' @param outdir Character scalar determining the output directory. If empty,
#'   or containing empty strings, each file's input directory is used.
#' @param demo Logical scalar. Do not create files, just return the usual list
#'   containing all potentially created files. Note that in contrast to the
#'   \code{demo} arguments of other IO functions, this requires the input files
#'   to be read.
#'
#' @param single Logical scalar. If there is only one group per file, i.e. only
#'   one output file would result from the splitting, create this file anyway?
#'   Such cases could be recognized by empty character vectors as values of the
#'   returned list (see below).
#' @param wildcard Logical scalar. Is \code{pattern} a shell-globbing wildcard
#'   that first needs to be converted to a regular expression?
#' @param invert Logical scalar. Invert pattern matching, i.e. treat all lines
#'   that \strong{not} match \code{pattern} as separators?
#' @param include Logical scalar. Also include the separator lines in the output
#'   files?
#'
#' @param format Character scalar determining the outfile name format. It is
#'   passed to \code{sprintf} and expects three placeholders: \itemize{
#'   \item the basename of the file;
#'   \item the index of the section;
#'   \item the file extension.
#'   }
#'   Getting \code{format} wrong might result in non-unique filenames and thus
#'   probably in overwritten files; accordingly, it should be used with care.
#' @param compressed Logical scalar. Passed to \code{\link{file_pattern}}, but
#'   here only affects the way filenames are split in extensions and basenames.
#'   Should only be set to \code{FALSE} if input files are not compressed (and
#'   have according file extensions).
#'
#' @param ... Optional arguments passed to \code{grepl}, which is used for
#'   matching the separator lines. See also \code{invert} listed above.
#'
#' @export
#' @return List of character vectors, each vector containing the names of the
#'   newly generated files. The names of the list are the input filenames. The
#'   list is returned invisibly.
#'
#' @details This function is useful for splitting
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} multi-plate CSV files
#'   before inputting them with \code{\link{read_opm}}. It is used by
#'   \code{\link{batch_opm}} for this purpose. See also the \sQuote{Examples}.
#'
#' @family io-functions
#' @seealso base::split base::strsplit
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
#' } else {
#'   warning("opm example files not found")
#' }
#' ## One could split new-style CSV as follows (if x is a vector of filenames):
#' # split_files(x, pattern = '^"Data File",')
#' ## note the correct setting of the quotes
#' ## A pattern that covers both old and new-style CSV is:
#' # split_files(x, pattern = '^("Data File",|Data File)')
#' ## This is used by batch_opm() in 'split' mode any by the 'run_opm.R' script
#'
split_files <- function(files, pattern, outdir = "", demo = FALSE,
    single = TRUE, wildcard = FALSE, invert = FALSE, include = TRUE,
    format = opm_opt("file.split.tmpl"), compressed = TRUE, ...) {

  create_outnames <- function(files, compressed, outdir) {
    file.pat <- file_pattern("any", compressed = compressed, literally = FALSE)
    out.base <- sub(file.pat, "", files, perl = TRUE, ignore.case = TRUE)
    out.ext <- substring(files, nchar(out.base) + 2L)
    if (compressed)
      out.ext <- sub("\\.[^.]+$", "", out.ext, perl = TRUE)
    if (length(outdir) && all(nzchar(outdir)))
      out.base <- file.path(outdir, basename(out.base))
    list(base = out.base, ext = out.ext)
  }

  LL(pattern, outdir, demo, single, wildcard, invert, include,
    format, compressed)
  files <- unique(as.character(files))
  out <- create_outnames(files, compressed = compressed, outdir = outdir)
  if (wildcard)
    pattern <- glob_to_regex(pattern)

  invisible(mapply(function(infile, out.base, out.ext) {
    con <- file(description = infile, encoding = opm_opt("file.encoding"))
    data <- readLines(con = con, warn = FALSE)
    close(con)
    data <- sections(x = data, pattern = pattern, invert = invert,
      include = include, ...)
    if ((len <- length(data)) == 0L || (!single && len == 1L))
      return(character())
    outnames <- sprintf(format, out.base, seq_along(data), out.ext)
    if (demo)
      message(listing(structure(.Data = outnames,
        names = seq_along(outnames)), header = infile))
    else
      mapply(write, data, outnames, USE.NAMES = FALSE, SIMPLIFY = FALSE)
    outnames
  }, files, out$base, out$ext, SIMPLIFY = FALSE))
}


################################################################################
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
#' underscores to a single one. Note that directory parts within the file names,
#' if any, are not affected.
#'
#' @param x Character vector or convertible to such. Names of the files to be
#'   modified.
#' @param overwrite Logical scalar. Overwrite pre-existing files, and do not
#'   care for duplicate names created by cleaning the filenames?
#' @param demo Logical scalar. Do not rename files but just return the usual
#'   result indicating the renaming actions that would be attempted? (Note that
#'   this does not indicate whether the renaming would also by successful.)
#' @param empty.tmpl Character scalar. The template to use for filenames that
#'   become empty after cleaning. Should include an integer placeholder to
#'   enable incrementing an index for creating unique filenames. (Empty
#'   filenames should occur rarely anyway.)
#' @export
#' @return Character vector, its names corresponding to the renamed old files,
#'   values corresponding to the novel names, returned invisibly.
#' @details This function might be useful for manageing
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} CSV files, which can
#'   contain a lot of special characters.
#' @family io-functions
#' @keywords utilities
#' @seealso base::file.rename
#' @examples
#'
#' # Check the example files: they should be ok
#' (x <- clean_filenames(opm_files("testdata"), demo = TRUE))
#' stopifnot(length(x) == 0)
#'
#' # Example with temporary files
#' (x <- tempfile(pattern = "cb& ahi+ si--")) # bad file name
#' write("test", x)
#' stopifnot(file.exists(x))
#' (y <- clean_filenames(x)) # file renamed
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
  LL(overwrite, demo, empty.tmpl)
  x <- unique.default(as.character(x))
  if (any(bad <- !nzchar(x))) {
    warning("removing invalid empty file name")
    x <- x[!bad]
  }
  result <- clean_basenames(basename(x))
  result <- ifelse(dirname(x) == ".", result, file.path(dirname(x), result))
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


################################################################################



