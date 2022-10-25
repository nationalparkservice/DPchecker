#' Load Metadata
#'
#' @description load.metadata loads the metadata file from a given path or directory.
#'
#' @details given a path or directory - default is the working directory -  load.metadata looks for files with the ending *_metadata.xml. The function quits and warns the user if no such files are found or if more than one such file is found. If only one metadata file is found, it checked for one of 3 formats: FGDC, ISO, or EML. Currently only EML is supported and the function will warn the user and quit if non-EML metadata is found. The EML metadata file is loaded into R's work space for future use during congruence checking.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#'
#' @return an R-object formatted as EML metadata.
#' @export
#'
#' @examples
#' my_metadata <- load.metadata()
#'
load.metadata <- function(directory = here::here()) {
  # get list of all files ending in metadata.xml
  lf <- list.files(path = directory, pattern = "metadata.xml")

  # if metadata file exists, stop the function and warn the user
  if (length(lf) < 1) {
    message("ERROR: Metadata check failed. No metadata found. Your metadata file name must end in _metadata.xml.\n")
  }
  # if multiple metadata files exists, stop the function and warn the user
  if (length(lf) > 1) {
    message("ERROR: Metadata check failed. The data package format only allows one metadata file per data package. Please remove extraneous metadata files or combine them into a single file.\n")
  }

  # if exactly 1 metadata file exists, determine what format the metadata file is. Accept only EML (for now):
  if (length(lf) == 1) {
    if (sum(grepl("<metstdv>FGDC-STD-001-1998", readr::read_lines(lf))) > 0) {
      metaformat <- "fgdc"
      stop(paste0("\nCongruence checking is not yet supported for ", metaformat, " metadata."))
    } else if (sum(grepl("schemas.isotc211.org/19115", readr::read_lines(lf))) > 0) {
      metaformat <- "ISO19915"
      stop(paste0("\nCongruence checking is not yet supported for ", metaformat, " metadata."))
    } else if (sum(grepl("<eml:eml", readr::read_lines(lf))) > 0) {
      metaformat <- "eml"
      # load metadata
      metadata <- EML::read_eml(lf, from = "xml")
      # return metadata to the the workspace
      cat(paste0("PASSED: Metadata check passed. EML metadata (", crayon::blue$bold(lf), ") found and loaded into R.\n\n"))
      return(metadata)
    }
  }
}


#' Load Data
#'
#' @description load.data inspects the working directory for data files. Loads all existing data files into a tibble.
#'
#' @details loads all data files in a specified directory (default is the working directory) into a tibble for later use in congruence checking. Returns the user to the working directory upon exit. Currently only supports .csv files.
#'
#' @param directory the directory where the data file(s) are found (i.e. your data package). Defaults to the current working directory. On exit, returns to the current working directory.
#'
#' @return a tibble of .csvs
#' @export
#'
#' @examples my_data <- load.data()
load.data <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  lf <- list.files(pattern = ".csv")
  tibble_List <- list()
  for (i in lf) {
    filepath <- file.path(getwd(), i)
    tibble_List[[i]] <- assign(i, readr::read_csv(filepath, show_col_types = FALSE))
  }
  return(tibble_List)
}



#' EML Version Check
#'
#' @description test.metadataVersion determines whether the version of the metadata supplied meets the current criteria for an NPS data package.
#'
#' @details currently only EML is supported. EML must be version >= 2.2.0.
#'
#' @param directory the directory where the data file(s) are found (i.e. your data package). Defaults to the current working directory. On exit, returns to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples
#' test.metadataVersion()
test.metadataVersion <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  mymeta <- load.metadata(directory)
  if (!is.null(mymeta)) {
    vers <- substr(sub(".*https://eml.ecoinformatics.org/eml-", "", mymeta)[1], 1, 5)
    vers <- numeric_version(vers)
    if (vers < "2.2.0") {
      message("ERROR: unsupported EML version: EML must be 2.2.0 or later.\n")
    }
    if (vers >= "2.2.0") {
      message("PASSED: EML version is supported.\n")
    }
  }
}


#' Validate Metadata Schema
#'
#' @description test.validateSchema inspects a metadata object loaded into R and determines whether it is schema-valid.
#'
#' @details currently, only EML is supported. For now this is just a wrapper form EML::eml_validate().
#'
#' @param directory the directory where the data file(s) are found (i.e. your data package). Defaults to the current working directory. On exit, returns to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples
#' test.validateSchema()
test.validateSchema <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  mymeta <- load.metadata(directory)
  if (!is.null(mymeta)) {
    val <- EML::eml_validate(mymeta)
    if (val == TRUE) {
      message("PASSED: Your metada is schema valid.\n")
    }
    if (val != TRUE) {
      message("ERROR: Your is metadata is schema-invalid.\n")
      attribs <- attributes(val)
      print(attribs)
    }
  }
}

#' Footer Check
#'
#' @description test.footer checks the metadata files to determine whether data files contain footer lines or not.
#'
#' @details If footer lines are not present, the data package passes the test. If footer lines are present, the data package fails the test and the user is instructed to remove footer lines prior to data package upload. Currently only EML metadata are supported.
#'
#' @param directory the directory where the data file(s) are found (i.e. your data package). Defaults to the current working directory. On exit, returns to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples
#' test.footer()
test.footer <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  mymeta <- load.metadata(directory)
  if (!is.null(mymeta)) {
    if (is.null(arcticdatautils::eml_get_simple(mymeta, "numFooterLines")) == TRUE) {
      message("Footer Check passed: Metadata indicates data files do not have footers.\n")
    } else {
      message("Footer Check ERROR: metadata indicates that data files include footers. Please remove all footers from data files.\n")
    }
  }
}


#' Header Check
#'
#' @description test.headerNum checks the metadata files to ensure that each data file contains exactly one header row.
#'
#' @details headerCheck examines the numHeaderLines element from EML (currently only EML is supported) metadata to determine how many header rows there are. If there are no header rows or if there is more than one header row, the test fails. The test also fails if there is no information about the number of header rows.
#'
#' @param directory the directory where the data file(s) are found (i.e. your data package). Defaults to the current working directory. On exit, returns to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples
#' test.headerNum()
test.headerNum <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  mymeta <- load.metadata(directory)
  if (!is.null(mymeta)) {
    header <- arcticdatautils::eml_get_simple(mymeta, "numHeaderLines")
    if (is.null(header) == TRUE) {
      stop("Header Check ERROR: metadata does not contain information about number of header rows")
    }
    headerNum <- NULL
    for (i in seq_along(header)) {
      if (header[i] != 1) {
        stop("Header Check ERROR: data files must contain exactly one header row")
      } else {
        headerNum <- append(headerNum, i)
      }
    }
    if (length(headerNum) == length(headerNum)) {
      message("PASSED header Check: Metadata indicates that each data file contains exactly one header row")
    }
  }
}

#' Field Delimiter Check
#'
#' @description test.delimiter checks the metadata file and ensures that each data file has a field delimiter with exactly one characte (e.g. ", ").
#'
#' @details test.delimiter examines the fieldDelimiter element from EML (currently only EML is supported) metadata to determine how many characters there are. If there is no fieldDelimiter element, the test returns an error. If the field delimiter is anything other than exactly one character in length, the test returns an error.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to the current working directory. On exit, returns you to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples
#' test.delimiter()
test.delimiter <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  # load metadata
  mymeta <- load.metadata(directory)

  if (!is.null(mymeta)) {
    delimit <- arcticdatautils::eml_get_simple(mymeta, "fieldDelimiter")
    if (is.null(delimit) == TRUE) {
      stop("Field Delimiter Check ERROR: metadata does not contain information about the field delimiter for data files")
    }
    delimitNum <- NULL
    for (i in seq_along(delimit)) {
      if (length(delimit[i]) != 1) {
        stop("Field Delimiter Check ERROR: the field delimiter must be a single character")
      } else {
        delimitNum <- append(delimitNum, i)
      }
    }
    if (length(delimitNum) == length(delimit)) {
      message("PASSED: field delimiter check passed: Metadata indicates that each data file contains a field delimiter that is a single character")
    }
  }
}

#' Test Metadata for Duplicate Filenames
#'
#' @description test.dupMetaEntries test to see whether there are duplicate filenames listed for the data files in (EML) metadata.
#'
#' @details specifically, test.dupMetaEntries looks at the 'physical' elements of a metadata file, which describe each data file, and asks whether there are duplicates entries under the objectName child element, which is where the file name for each data file is stored. If your files are not in your working directory and you specified their location, reverts back to your working directory on exit.
#'
#' @param directory the directory where the files for your data package reside. Defaults to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples test.dupMetaEntries()
test.dupMetaEntries <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  # load metadata
  mymeta <- load.metadata(directory)

  if (!is.null(mymeta)) {
    # get physical elements (and all children elements)
    attribs <- EML::eml_get(mymeta, "physical")

    # list all file names held in "objectName"
    fn <- unlist(attribs)[grepl("objectName", names(unlist(attribs)), fixed = T)]

    # find duplicate entries:
    dups <- fn[duplicated(fn)]

    # if no duplicates, test passed:
    if (length(dups) == 0) {
      message("PASSED: Each data file name is used exactly once in the metadata file.")
    }
    # if duplicates, test failed:
    if (length(dups > 0)) {
      cat(paste0("ERROR: metadata file name check failed. Some filenames are used more than once in the metadata:\n", crayon::red$bold(dups)))
    }
  }
}

#' Test Data Files for Duplicate Names
#'
#' @description test.dupDataFiles looks at the file names of .csv files within a specified directory and tests whether there are any duplicates.
#'
#' @details not sure why you'd ever need this function. Seems that most file systems won't let you have duplicate file names, but here it is for the sake of completeness. If you specify a directory other than your current working directory, it will return to the current working directory on exit.
#'
#' @param directory path to your data files. Defaults to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples test.dupDataFiles()
test.dupDataFiles <- function(directory = getwd()) {
  # switch directories; on exit return to current directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  # get data file filenames (only .csv supported for now):
  lf <- list.files(pattern = ".csv")

  # find duplicate entries:
  dups <- lf[duplicated(lf)]
  # if no duplicates, test passed:
  if (length(dups) == 0) {
    message("PASSED: Each data file name is used exactly once.")
  }
  # if duplicates, test failed:
  if (length(dups > 0)) {
    print(paste0("The following data filenames are duplicated:\n", dups))
    stop("ERROR: data file name check failed. Some filenames are used more than once.")
  }
}

#' File Name Match
#'
#' @description test.fileNameMatch checks to see whether all data files (.csv) within a specified directory are listed under the objectName (child of physical) element in an EML metadata file in the same directory, and vice versa. Mismatches will result in an error message.
#'
#' @details If a directory other than the current working directory is specified, test.fileNameMatch returns to the current working directory on exit. Note that the metadata file must follow NPS naming conventions, specifically ending in *_metadata.xml. test.fileNameMatch assumes there are the same number of data files in the directory as dataTables in the metadata file.
#'
#' @param directory path to the data files and metadata of a data package. Defaults to the current working directory.
#'
#' @return text
#' @export
#'
#' @examples test.fileNameMatch()
test.fileNameMatch <- function(directory = getwd()) {
  # on exit return to current working directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  # get metadata filenames from "physical" attribute:
  mymeta <- load.metadata(directory)

  if (!is.null(mymeta)) {
    # get physical elements (and all children elements)
    phys <- EML::eml_get(mymeta, "physical")

    # get everything under "objectName"
    fn <- unlist(phys)[grepl("objectName", names(unlist(phys)), fixed = T)]

    # get filenames from .csv data files in directory:
    lf <- list.files(pattern = ".csv")

    # items in metadata but not data file names:
    meta <- setdiff(fn, lf)

    # items in data file names but not in metadata:
    dat <- setdiff(lf, fn)

    if (length(meta) == 0 && length(dat) == 0) {
      message("PASSED: file name congruence check. All data files are listed in metadata and all metdata files names refer to data files.\n")
    }
    if (length(meta) > 0) {
      message("ERROR: metadata lists file names that do not have corresponding data files:")
      print(crayon::red$bold(meta))
    }
    if (length(dat) > 0) {
      message("ERROR: data files exist that are not listed in the metadata:")
      cat(crayon::red$bold(dat))
    }
  }
}

#' Test Number of Fields
#'
#' @description test.filedNum compares the number of attributes each dataTable within the EML metadata to the number of columns for the corresponding .csv. If the numbers are the same, the test passes. If the numbers differ, the test fails.
#'
#' @details One thing to be cautious of: test.fieldNum does not compare the order or names of the columns! For now, that's on you.
#'
#' @param directory path to the data package files. Defaults to the current working directory.
#'
#' @return message
#' @export
#'
#' @examples
#' test.fieldNum()
test.fieldNum <- function(directory = getwd()) {
  # on exit return to current working directory:
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)

  # load metadata
  mymeta <- load.metadata(directory)

  if (!is.null(mymeta)) {
    # get dataTable and all children elements
    dataTab <- EML::eml_get(mymeta, "dataTable")

    # list the filenames associated with each dataTable:
    fn <- unlist(dataTab)[grepl("objectName", names(unlist(dataTab)), fixed = T)]

    # Make a list of each filename ("Meta_<filename>") that contains the names of the attributes associated with each data file.

    # if only one dataTable:
    if (length(suppressWarnings(within(dataTab[1], rm("@context")))) == 0) {
      attribs <- unlist(dataTab)[grepl("attributeName", names(unlist(dataTab)), fixed = T)]
      assign(paste0("Meta_", fn), NULL)
      for (i in seq_along(attribs)) {
        assign(paste0("Meta_", fn), append(get(paste0("Meta_", fn)), attribs[[i]]))
      }
    }

    # if mulitple dataTables:
    if (length(suppressWarnings(within(dataTab[1], rm("@context")))) > 0) {
      newdat <- within(dataTab, rm("@context"))
      for (i in seq_along(fn)) {
        attriblist <- newdat[[i]][[4]][[1]]
        assign(paste0("Meta_", fn[i]), NULL)
        for (j in seq_along(attriblist)) {
          attribname <- newdat[[i]][[4]][[1]][[j]][[1]]
          assign(paste0("Meta_", fn[i]), append(get(paste0("Meta_", fn[i])), attribname))
        }
      }
    }

    lf <- list.files(pattern = ".csv")

    # get first row of each .csv. Assumes there is exactly 1 header row!
    for (i in seq_along(lf)) {
      colnum <- readLines(lf[i], n = 1)
      assign(paste0("Data_", lf[i]), strsplit(colnum, ",")[[1]])
    }

    # Check each CSV. If column number and attribute number are a mismatch, add it to a list.
    mismatch <- NULL
    for (i in seq_along(lf)) {
      if (length(get(paste0("Data_", lf[i]))) != length(get(paste0("Meta_", lf[i])))) {
        mismatch <- append(mismatch, lf[i])
      }
    }

    # if there are any mismatches, print ERROR and specify problematic files:
    if (length(mismatch > 0)) {
      cat("ERROR: field numer mismatch. Some columns in data files are not listed in metadata or some attributes listed in metadata were not found as headers in the data files: \n")
      cat(crayon::red$bold(mismatch), sep = "\n")
    }
    # if there are no mismatches, print passed msg:
    else {
      cat("PASSED: field number match. All columns in data files are listed in metadata and all attributes in metadata are columns in the data files.")
    }
  }
}
