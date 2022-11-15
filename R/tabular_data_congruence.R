#' Load Metadata
#'
#' @description load_metadata loads the metadata file from a given path or directory.
#'
#' @details given a path or directory - default is the working directory -  load_metadata looks for files with the ending *_metadata.xml. The function quits and warns the user if no such files are found or if more than one such file is found. If only one metadata file is found, it checked for one of 3 formats: FGDC, ISO, or EML. Currently only EML is supported and the function will warn the user and quit if non-EML metadata is found. The EML metadata file is loaded into R's work space for future use during congruence checking.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#'
#' @return an R-object formatted as EML metadata.
#' @export
#'
#' @examples
#' my_metadata <- load_metadata()
#'
load_metadata <- function(directory = here::here()) {
  # get list of all files ending in metadata.xml
  lf <- list.files(path = directory, pattern = "metadata.xml")
  metadata_file <- file.path(directory, lf)

  # if exactly 1 metadata file exists, determine what format the metadata file is. Accept only EML (for now):
  if (length(metadata_file) == 1) {
    # Determine metadata format
    metaformat <- dplyr::case_when(any(grepl("<metstdv>FGDC-STD-001-1998", readr::read_lines(metadata_file))) ~ "fgdc",
                                   any(grepl("schemas.isotc211.org/19115", readr::read_lines(metadata_file))) ~ "ISO19915",
                                   any(grepl("<eml:eml", readr::read_lines(metadata_file))) ~ "eml",
                                   TRUE ~ "UNKNOWN")
    # Read metadata
    if (metaformat == "fgdc") {
      stop(paste0("\nCongruence checking is not yet supported for ", metaformat, " metadata."))
    } else if (metaformat == "ISO19915") {
      stop(paste0("\nCongruence checking is not yet supported for ", metaformat, " metadata."))
    } else if (metaformat == "eml") {
      metadata <- EML::read_eml(metadata_file, from = "xml")
      if (is.null(metadata)) {
        stop("Could not load metadata.")
      } else {
        message(paste0("PASSED: Metadata check passed. EML metadata (", crayon::blue$bold(metadata_file), ") found and loaded into R."))
      }

    }
  } else if (length(metadata_file) < 1) { # if no metadata file exists, stop the function and warn the user
    stop("Metadata check failed. No metadata found. Your metadata file name must end in _metadata.xml.\n")
  } else if (length(metadata_file) > 1) {  # if multiple metadata files exist, stop the function and warn the user
    stop("Metadata check failed. The data package format only allows one metadata file per data package. Please remove extraneous metadata files or combine them into a single file.\n")
  }

  return(metadata)
}


#' Load Data
#'
#' @description load_data inspects the working directory for data files. Loads all existing data files into a tibble.
#'
#' @details loads all data files in a specified directory (default is the working directory) into a tibble for later use in congruence checking. Returns the user to the working directory upon exit. Currently only supports .csv files.
#'
#' @param directory the directory where the data file(s) are found (i.e. your data package). Defaults to the current working directory. On exit, returns to the current working directory.
#'
#' @return a tibble of .csvs
#' @export
#'
#' @examples my_data <- load_data()
load_data <- function(directory = here::here()) {
  data_filenames <- list.files(path = directory, pattern = ".csv")
  tibble_list <- sapply(data_filenames,
                        function(data_file) {readr::read_csv(file.path(directory, data_file), show_col_types = FALSE)},
                        USE.NAMES = TRUE,
                        simplify = FALSE)
  return(tibble_list)
}



#' EML Version Check
#'
#' @description test_metadata_version determines whether the version of the metadata supplied meets the current criteria for an NPS data package.
#'
#' @details currently only EML is supported. EML must be version >= 2.2.0.
#'
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_metadata_version()
test_metadata_version <- function(metadata = load_metadata(here::here())) {

  vers <- substr(sub(".*https://eml.ecoinformatics.org/eml-", "", metadata)[1], 1, 5)
  vers <- numeric_version(vers)
  if (vers < "2.2.0") {
    stop(paste("Unsupported EML version: EML must be 2.2.0 or later. Your version is", vers))
  } else {  # vers >= 2.2.0
    message(paste0("PASSED: Your EML version (", vers, ") is supported."))
  }

  return(invisible(metadata))
}


#' Validate Metadata Schema
#'
#' @description test_validate_schema inspects a metadata object loaded into R and determines whether it is schema-valid.
#'
#' @details currently, only EML is supported. For now this is just a wrapper form EML::eml_validate().
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_validate_schema()
test_validate_schema <- function(metadata = load_metadata(here::here())) {

  val <- EML::eml_validate(metadata)
  if (val == TRUE) {
    message("PASSED: Your metadata is schema valid.")
  } else {
    attribs <- attributes(val)
    stop(paste0("Your is metadata is schema-invalid.\n", attribs))
  }

  return(invisible(metadata))
}

#' Footer Check
#'
#' @description test_footer checks the metadata files to determine whether data files contain footer lines or not.
#'
#' @details If footer lines are not present, the data package passes the test. If footer lines are present, the data package fails the test and the user is instructed to remove footer lines prior to data package upload. Currently only EML metadata are supported.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_footer()
test_footer <- function(metadata = load_metadata(here::here())) {

  if (is.null(arcticdatautils::eml_get_simple(metadata, "numFooterLines"))) {
    message("PASSED: Metadata indicates data files do not have footers.\n")
  } else {
    stop("Metadata indicates that data files include footers. Please remove all footers from data files.")
  }

  return(invisible(metadata))
}


#' Header Check
#'
#' @description test_header_num checks the metadata files to ensure that each data file contains exactly one header row.
#'
#' @details test_header_num examines the numHeaderLines element from EML (currently only EML is supported) metadata to determine how many header rows there are. If there are no header rows or if there is more than one header row, the test fails. The test also fails if there is no information about the number of header rows.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_header_num()
test_header_num <- function(metadata = load_metadata(here::here())) {

  header <- arcticdatautils::eml_get_simple(metadata, "numHeaderLines")
  names(header) <- arcticdatautils::eml_get_simple(metadata, "entityName")
  if (is.null(header)) {
    stop("Metadata does not contain information about number of header rows")
  } else if (any(header != "1")) {
    wrong_headers <- header[header != "1"]
    wrong_headers <- paste0("     ", names(wrong_headers), ": ", wrong_headers, collapse = "\n")
    stop(paste0("Metadata indicates that the following data files do not contain either zero or more than one header row:\n", wrong_headers))
  } else {
    message("PASSED header check: Metadata indicates that each data file contains exactly one header row")
  }

  return(invisible(metadata))
}

#' Field Delimiter Check
#'
#' @description test_delimiter checks the metadata file and ensures that each data file has a field delimiter with exactly one character (e.g. ", ").
#'
#' @details test_delimiter examines the fieldDelimiter element from EML (currently only EML is supported) metadata to determine how many characters there are. If there is no fieldDelimiter element, the test returns an error. If the field delimiter is anything other than exactly one character in length, the test returns an error.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_delimiter()
test_delimiter <- function(metadata = load_metadata(here::here())) {

  delimit <- arcticdatautils::eml_get_simple(metadata, "fieldDelimiter")
  names(delimit) <- arcticdatautils::eml_get_simple(metadata, "entityName")
  if (is.null(delimit) == TRUE) {
    stop("Metadata does not contain information about the field delimiter for data files")
  } else if (any(nchar(delimit) != 1)) {
    wrong_delimiters <- delimit[nchar(delimit) != 1]
    wrong_delimiters <- paste0("     ", names(wrong_delimiters), collapse = "\n")
    stop(paste0("Metadata indicates that the following data files do not contain valid delimiters:\n", wrong_delimiters))
  } else {
    message("PASSED: field delimiter check passed: Metadata indicates that each data file contains a field delimiter that is a single character")
  }

  return(invisible(metadata))
}

#' Test Metadata for Duplicate Filenames
#'
#' @description test_dup_meta_entries test to see whether there are duplicate filenames listed for the data files in (EML) metadata.
#'
#' @details specifically, test_dup_meta_entries looks at the 'physical' elements of a metadata file, which describe each data file, and asks whether there are duplicates entries under the objectName child element, which is where the file name for each data file is stored. If your files are not in your working directory and you specified their location, reverts back to your working directory on exit.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples test_dup_meta_entries()
test_dup_meta_entries <- function(metadata = load_metadata(here::here())) {


  # get physical elements (and all children elements)
  attribs <- EML::eml_get(metadata, "physical")

  # list all file names held in "objectName"
  fn <- unlist(attribs)[grepl("objectName", names(unlist(attribs)), fixed = T)]

  # find duplicate entries:
  dups <- fn[duplicated(fn)]

  # if no duplicates, test passed:
  if (length(dups) == 0) {
    message("PASSED: Each data file name is used exactly once in the metadata file.")
  } else if (length(dups > 0)) {  # if duplicates, test failed:
    stop(paste0("Metadata file name check failed. Some filenames are used more than once in the metadata:\n", crayon::red$bold(dups)))
  }


  return(invisible(metadata))
}

#' Test Data Files for Duplicate Names
#'
#' @description test_dup_data_files looks at the file names of .csv files within a specified directory and tests whether there are any duplicates.
#'
#' @details not sure why you'd ever need this function. Seems that most file systems won't let you have duplicate file names, but here it is for the sake of completeness. If you specify a directory other than your current working directory, it will return to the current working directory on exit.
#'
#' @inheritParams load_data
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples test_dup_data_files()
#'
test_dup_data_files <- function(directory = here::here()) {
  # get data file filenames (only .csv supported for now):
  data_files <- list.files(path = directory, pattern = ".csv")

  # find duplicate entries:
  dups <- data_files[duplicated(data_files)]
  # if no duplicates, test passed:
  if (length(dups) == 0) {
    message("PASSED: Each data file name is used exactly once.")
  } else if (length(dups > 0)) {  # if duplicates, test failed:
    dups_msg <- paste0("The following data filenames are duplicated:\n", dups)
    stop(paste0("Data file name check failed. Some filenames are used more than once.\n", dups_msg))
  }
  return(invisible(data_files))
}

#' File Name Match
#'
#' @description test_file_name_match checks to see whether all data files (.csv) within a specified directory are listed under the objectName (child of physical) element in an EML metadata file in the same directory, and vice versa. Mismatches will result in an error message.
#'
#' @details If a directory other than the current working directory is specified, test.fileNameMatch returns to the current working directory on exit. Note that the metadata file must follow NPS naming conventions, specifically ending in *_metadata.xml. test.fileNameMatch assumes there are the same number of data files in the directory as dataTables in the metadata file.
#'
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#' @examples test_file_name_match()
test_file_name_match <- function(directory = here::here(), metadata = load_metadata(directory)) {

  # get physical elements (and all children elements)
  phys <- EML::eml_get(metadata, "physical")

  # get everything under "objectName"
  files_in_metadata <- unlist(phys)[grepl("objectName", names(unlist(phys)), fixed = T)]

  # get filenames from .csv data files in directory:
  files_in_dir <- list.files(path = directory, pattern = ".csv")

  # items in metadata but not data file names:
  meta_only <- setdiff(files_in_metadata, files_in_dir)

  # items in data file names but not in metadata:
  dir_only <- setdiff(files_in_dir, files_in_metadata)

  if (length(meta_only) == 0 && length(dir_only) == 0) {
    message("PASSED: file name congruence check. All data files are listed in metadata and all metadata files names refer to data files.")
  } else if (length(meta_only) > 0 || length(dir_only) > 0) {
    meta_txt <- paste("\n", crayon::red$bold(meta_only), collapse = "")
    dat_txt <- paste("\n", crayon::red$bold(dir_only), collapse = "")
    msg <- paste0(length(meta_only), " files listed in metadata and missing from data folder", meta_txt,
                  "\n", length(dir_only), " files present in data folder and missing from metadata", dat_txt)
    stop(msg)
  }

  return(invisible(metadata))
}

#' Test Matching Data/Metadata Fields
#'
#' @description test_fields_match compares the attributes of each dataTable within the EML metadata to the columns in the corresponding .csv. If the columns have the same names and order, the test passes. If the columns differ, the test fails.
#'
#' @details test_fields_match briefly checks that data files match, but you should really run `test_file_name_match()` before you run this test.
#'
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_fields_match()
test_fields_match <- function(directory = here::here(), metadata = load_metadata(directory)) {

  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get list of attributes for each table in the metadata
  metadata_attrs <- lapply(data_tbl, function(tbl) {arcticdatautils::eml_get_simple(tbl, "attributeName")})
  metadata_attrs$`@context` <- NULL
  names(metadata_attrs) <- arcticdatautils::eml_get_simple(data_tbl, "objectName")

  # Get list of column names for each table in the csv data
  data_files <- list.files(path = directory, pattern = ".csv")
  data_colnames <- sapply(data_files, function(data_file) {names(readr::read_csv(file.path(directory, data_file), n_max = 1, show_col_types = FALSE))}, USE.NAMES = TRUE, simplify = FALSE)

  # Quick check that tables match
  if (!(all(names(data_colnames) %in% names(metadata_attrs)) & all(names(metadata_attrs) %in% names(data_colnames)))) {
    stop("Mismatch in data filenames and files listed in metadata. You must resolve this issue before you can verify that fields match. Call `test_file_name_match()` for more info.")
  }

  # Check each CSV. If column and attributes are a mismatch, describe the issue
  mismatches <- sapply(data_files, function(data_file) {
    meta_cols <- metadata_attrs[[data_file]]
    data_cols <- data_colnames[[data_file]]
    if (length(meta_cols) == length(data_cols) && all(meta_cols == data_cols)) {  # Columns match and are in right order
      return(NULL)
    } else if (all(meta_cols %in% data_cols) && all(data_cols %in% meta_cols)) {  # Columns match and are in wrong order
      return("Metadata column order does not match data column order")
    } else {  # Columns don't match
      missing_from_meta <- paste(data_cols[!(data_cols %in% meta_cols)], collapse = ", ")
      missing_from_data <- paste(meta_cols[!(meta_cols %in% data_cols)], collapse = ", ")
      if (missing_from_meta != "") {
        missing_from_meta <- paste0("Data column(s) ", crayon::red$bold(missing_from_meta), " missing from metadata")
      } else {
        missing_from_meta <- NULL
      }
      if (missing_from_data != "") {
        missing_from_data <- paste0("Metadata column(s) ", crayon::red$bold(missing_from_data), " missing from data")
      } else {
        missing_from_data <- NULL
      }
      return(paste0(paste(c(missing_from_data, missing_from_meta), collapse = ". "), "."))
    }
  }, USE.NAMES = TRUE, simplify = FALSE)

  # Remove tables from list that pass the test, and convert it to a named vector
  mismatches <- purrr::discard(mismatches, is.null) %>%
    unlist()

  # If there are mismatches, throw an error, otherwise, print a message indicating passed test
  if (!is.null(mismatches)) {
    msg <- paste0(names(mismatches), ":  ", mismatches, collapse = "\n")
    msg <- paste("Field mismatch between data and metadata:\n", msg)
    stop(msg)
  } else {
    message("PASSED: fields match. All columns in data files are listed in metadata and all attributes in metadata are columns in the data files.")
  }

  return(invisible(metadata))
}

#' Test Numeric Fields
#'
#' @description test_numeric_fields verifies that all columns listed as numeric in the metadata are free of non-numeric data.
#'
#' @details "NA" and missing data codes documented in the metadata will *not* cause this test to fail. Note that this test assumes that the column types that are in the metadata are the intended types, i.e., if your metadata says a column is text and it should actually be numeric, it will not be caught by this test.
#'
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_numeric_fields()
test_numeric_fields <- function(directory = here::here(), metadata = load_metadata(directory)) {

  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  data_tbl$`@context` <- NULL
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get list of attributes for each table in the metadata
  numeric_attrs <- lapply(data_tbl, function(tbl) {
    attrs <- suppressMessages(EML::get_attributes(tbl$attributeList))
    attrs <- attrs$attributes
    attrs <- dplyr::filter(attrs, domain == "numericDomain")
    return(attrs)
  })
  numeric_attrs$`@context` <- NULL
  names(numeric_attrs) <- arcticdatautils::eml_get_simple(data_tbl, "objectName")

  # Get list of column names for each table in the csv data
  data_files <- list.files(path = directory, pattern = ".csv")
  data_non_numeric <- sapply(data_files, function(data_file) {
    num_col_names <- numeric_attrs[[data_file]]$attributeName
    # If the table doesn't have any numeric columns listed in the metadata, it automatically passes
    if (length(num_col_names) == 0) {
      return(NULL)
    }
    na_strings <- c("", "NA")
    if ("missingValueCode" %in% names(numeric_attrs[[data_file]])) {
      na_strings <- c(na_strings, unique(numeric_attrs[[data_file]]$missingValueCode))
    }
    # Read everything as character, then see if it fails when converting to number
    num_data <- readr::read_csv(file.path(directory, data_file), col_select = dplyr::all_of(num_col_names), na = na_strings, col_types = rep("c", length(num_col_names)), show_col_types = FALSE)
    non_numeric <- sapply(names(num_data), function(col) {
      col_data <- num_data[[col]]
      col_data <- col_data[!is.na(col_data)]
      col_data <- suppressWarnings(as.numeric(col_data))
      if (any(is.na(col_data))) {
        return(col)
      } else {
        return(NULL)
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
    non_numeric <- purrr::discard(non_numeric, is.null)
    if (length(names(non_numeric)) > 0) {
      return(paste(names(non_numeric), collapse = ", "))  # List non-numeric columns
    } else {
      return(NULL)
    }
  },
  USE.NAMES = TRUE, simplify = FALSE)

  # This will be null unless supposedly numeric columns read in as non-numeric
  data_non_numeric <- purrr::discard(data_non_numeric, is.null)
  data_non_numeric <- unlist(data_non_numeric)

  # If numeric cols contain non-numeric values, throw an error, otherwise, print a message indicating passed test
  if (!is.null(data_non_numeric)) {
    msg <- paste0(names(data_non_numeric), ":  ", data_non_numeric, collapse = "\n")
    msg <- paste("Columns indicated as numeric in metadata contain non-numeric values:\n", msg)
    stop(msg)
  } else {
    message("PASSED: Columns indicated as numeric in metadata contain only numeric values and valid missing value codes.")
  }

  return(invisible(metadata))
}

#' Test Date Range
#'
#' @description test_date_range verifies that dates in the dataset are consistent with the date range in the metadata.
#'
#' @details This function checks columns that are identified as date/time in the metadata. It throws a warning if the dates contained in the columns are outside of the temporal coverage specified in the metadata. If the date/time format string specified in the metadata does not match the actual format of the date in the CSV, it will likely fail to parse and throw an error.
#'
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' test_date_range()
test_date_range <- function(directory = here::here(), metadata = load_metadata(directory)) {

  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  data_tbl$`@context` <- NULL
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get begin date from metadata
  meta_begin_date <- readr::parse_datetime(EMLeditor::get.beginDate(metadata), format = "%d %B %Y")
  meta_end_date <- readr::parse_datetime(EMLeditor::get.endDate(metadata), format = "%d %B %Y")
  meta_date_range <- c(begin = meta_begin_date, end = meta_end_date)

  # Check if temporal coverage info is complete. Throw a warning if it's missing entirely and an error if it's only partially complete.
  # The logic being that maybe there's a scenario where temporal coverage isn't relevant to the dataset at all, but if it has date/time info, it has to have both a start and end.
  if (all(is.na(meta_date_range))) {
    warning("Metadata does not contain temporal coverage information.")
    return(metadata)
  } else if (any(is.na(meta_date_range))) {
    missing_date <- names(meta_date_range[is.na(meta_date_range)])
    present_date <- names(meta_date_range[!is.na(meta_date_range)])
    stop(paste("Metadata temporal coverage is missing", missing_date, "date."))
  }

  # Get list of date/time attributes for each table in the metadata
  dttm_attrs <- lapply(data_tbl, function(tbl) {
    attrs <- suppressMessages(EML::get_attributes(tbl$attributeList))
    attrs <- attrs$attributes
    attrs <- dplyr::filter(attrs, domain == "dateTimeDomain")
    return(attrs)
  })
  dttm_attrs$`@context` <- NULL
  names(dttm_attrs) <- arcticdatautils::eml_get_simple(data_tbl, "objectName")

  # For each csv table, check that date/time columns are consistent with temporal coverage in metadata. List out tables and columns that are not in compliance.
  data_files <- list.files(path = directory, pattern = ".csv")
  dataset_out_of_range <- sapply(data_files, function(data_file) {
    dttm_col_names <- dttm_attrs[[data_file]]$attributeName
    # If the table doesn't have any date/time columns listed in the metadata, it automatically passes
    if (length(dttm_col_names) == 0) {
      return(NULL)
    }
    # Get format string for each date/time column and filter out anything that doesn't have a year associated with it
    dttm_formats <- dttm_attrs[[data_file]]$formatString
    dttm_formats <- dttm_formats[grepl("Y", dttm_formats)]
    dttm_col_names <- dttm_col_names[grepl("Y", dttm_formats)]
    # Convert date/time formats to be compatible with R, and put them in a list so we can use do.call(cols)
    dttm_formats_r <- convert_datetime_format(dttm_formats)
    dttm_col_spec <- dttm_formats_r %>%
      as.list() %>%
      lapply(readr::col_datetime)
    names(dttm_col_spec) <- dttm_col_names
    names(dttm_formats_r) <- dttm_col_names
    names(dttm_formats) <- dttm_col_names

    # Read date/time columns, find max/min, and compare with max and min dates in metadata
    na_strings <- c("", "NA")
    if ("missingValueCode" %in% names(dttm_attrs[[data_file]])) {
      na_strings <- c(na_strings, unique(dttm_attrs[[data_file]]$missingValueCode))
    }
    dttm_data <- suppressWarnings(readr::read_csv(file.path(directory, data_file), col_select = dplyr::all_of(dttm_col_names), na = na_strings, col_types = do.call(readr::cols, dttm_col_spec), show_col_types = FALSE))
    tbl_out_of_range <- sapply(names(dttm_data), function(col) {
      col_data <- dttm_data[[col]]
      if (all(is.na(col_data))) {
        return(paste(col, "(failed to parse)"))
      }
      max_date <- max(col_data, na.rm = TRUE)
      min_date <- min(col_data, na.rm = TRUE)
      format_str_r <- dttm_formats_r[col]
      bad_cols <- NULL

      # Compare years first, and only compare months and days if they are included in the format string
      if (lubridate::year(max_date) > lubridate::year(meta_end_date) || lubridate::year(min_date) < lubridate::year(meta_begin_date)) {
        bad_cols <- paste0(col, " [", format(min_date, format_str_r), ", ", format(max_date, format_str_r), "]") # column name and actual date range
      } else if (grepl("%m", format_str_r) && (lubridate::month(max_date) > lubridate::month(meta_end_date) || lubridate::month(min_date) < lubridate::month(meta_begin_date))) {
        bad_cols <- paste0(col, " [", format(min_date, format_str_r), ", ", format(max_date, format_str_r), "]")
      } else if (grepl("%d", format_str_r) && (lubridate::day(max_date) > lubridate::day(meta_end_date) || lubridate::day(min_date) < lubridate::day(meta_begin_date))) {
        bad_cols <- paste0(col, " [", format(min_date, format_str_r), ", ", format(max_date, format_str_r), "]")
      }
      return(bad_cols)
    }, simplify = FALSE, USE.NAMES = TRUE)
    tbl_out_of_range <- purrr::discard(tbl_out_of_range, is.null)
    if (length(names(tbl_out_of_range)) > 0) {
      return(paste(tbl_out_of_range, collapse = ", "))  # List out of range date columns
    } else {
      return(NULL)
    }
  },
  USE.NAMES = TRUE, simplify = FALSE)

  # This will be null unless supposedly numeric columns read in as non-numeric
  dataset_out_of_range <- purrr::discard(dataset_out_of_range, is.null)
  dataset_out_of_range <- unlist(dataset_out_of_range)

  # If numeric cols contain non-numeric values, throw an error, otherwise, print a message indicating passed test
  if (!is.null(dataset_out_of_range)) {
    msg <- paste0("\t", names(dataset_out_of_range), ":  ", dataset_out_of_range, collapse = "\n")
    msg <- paste0("The following date/time columns are out of the range [", meta_begin_date, ", ", meta_end_date, "] specified in the metadata:\n", msg)
    if (grepl("failed to parse", msg)) {
      stop(msg)  # Throw an error if some dates won't parse
    } else {
      warning(msg)  # If dates all parse but are out of range, just throw a warning.
    }

  } else {
    message("PASSED: Columns indicated as date/time in metadata are within the stated temporal coverage range.")
  }

  return(invisible(metadata))
}


#' Convert EML date/time format string to one that R can parse
#'
#' @details This is not a sophisticated function. If the EML format string is not valid, it will happily and without complaint return an R format string that will break your code. You have been warned.
#'
#' @param eml_format_string A character vector of EML date/time format strings. This function understands the following codes: YYYY = four digit year, YY = two digit year, MMM = three letter month abbrev., MM = two digit month, DD = two digit day, hh or HH = 24 hour time, mm = minutes, ss or SS = seconds.
#'
#' @return A character vector of date/time format strings that can be parsed by `readr` or `strptime`.
#' @export
#'
#' @examples
#' convert_datetime_format("MM/DD/YYYY")
#' convert_datetime_format(c("MM/DD/YYYY", "YY-MM-DD"))
convert_datetime_format <- function(eml_format_string) {
  r_format_string <- eml_format_string %>%
    stringr::str_replace_all("YYYY", "%Y") %>%
    stringr::str_replace_all("YY", "%y") %>%
    stringr::str_replace_all("MMM", "%b") %>%
    stringr::str_replace_all("MM", "%m") %>%
    stringr::str_replace_all("DD", "%d") %>%
    stringr::str_replace_all("(hh)|(HH)", "%H") %>%
    stringr::str_replace_all("mm", "%M") %>%
    stringr::str_replace_all("(ss)|(SS)", "%S")

  return(r_format_string)
}
