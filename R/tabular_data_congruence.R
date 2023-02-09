#' Load Metadata
#'
#' @description load_metadata loads the metadata file from a given path or directory.
#'
#' @details given a path or directory - default is the working directory -  load_metadata looks for files with the ending *_metadata.xml. The function quits and warns the user if no such files are found or if more than one such file is found. If only one metadata file is found, it checked for one of 3 formats: FGDC, ISO, or EML. Currently only EML is supported and the function will warn the user and quit if non-EML metadata is found. The EML metadata file is loaded into R's work space for future use during congruence checking.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param inform_success Boolean indicating whether to display a message when metadata is successfully loaded.
#'
#' @return an R-object formatted as EML metadata.
#' @export
#'
#' @examples
#' data_pkg_dir <- DPchecker_example("BICY_veg")
#' my_metadata <- load_metadata(data_pkg_dir)
#'
load_metadata <- function(directory = here::here(), inform_success = FALSE) {
  # get list of all files ending in metadata.xml
  lf <- list.files(path = directory, pattern = "metadata.xml", ignore.case = TRUE)

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
      cli::cli_abort(c("x" = "Congruence checking is not yet supported for {metaformat} metadata."))
    } else if (metaformat == "ISO19915") {
      cli::cli_abort(c("x" = "Congruence checking is not yet supported for {metaformat} metadata."))
    } else if (metaformat == "eml") {
      metadata <- EML::read_eml(metadata_file, from = "xml")
      if (is.null(metadata)) {
        cli::cli_abort(c("x" = "Could not load metadata."))
      } else {
        if (inform_success) {
          cli::cli_inform(c("v" = "Metadata check passed. EML metadata {.file {metadata_file}} found and loaded into R."))
        }
      }
    } else {
      cli::cli_abort(c("x" = "Could not determine metadata format."))
    }
  } else if (length(metadata_file) < 1) { # if no metadata file exists, stop the function and warn the user
    cli::cli_abort(c("x" = "Metadata check failed. No metadata found. Your metadata file name must end in {.file _metadata.xml}."))
  } else if (length(metadata_file) > 1) {  # if multiple metadata files exist, stop the function and warn the user
    cli::cli_abort(c("x" = "Metadata check failed. The data package format only allows one metadata file per data package. Please remove extraneous metadata files or combine them into a single file."))
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
#' @examples
#' data_pkg_dir <- DPchecker_example("BICY_veg")
#' my_data <- load_data(data_pkg_dir)
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
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_metadata_version(meta)
test_metadata_version <- function(metadata = load_metadata(here::here())) {

  # Declaring oldest and newest accepted versions here so that they're easier to update
  oldest_accepted_ver <- "2.2.0"
  newest_accepted_ver <- "2.2.0"

  # vers <- substr(sub(".*https://eml.ecoinformatics.org/eml-", "", metadata)[1], 1, 5)
  vers <- c(EML::eml_get(metadata, "eml")[[1]],
            unlist(strsplit(EML::eml_get(metadata, "schemaLocation")[[1]], "\\s"))[1])
  vers <- vers %>%
    stringr::str_extract("eml-\\d+\\.\\d+(\\.\\d+)?") %>%
    stringr::str_replace("eml-", "")

  tryCatch({ns_ver <- numeric_version(vers[1])
  schema_ver <- numeric_version(vers[2])},
  error = function(err) {
    cli::cli_abort(c("x" = err$message))
  })

  # Check that both namespace and schema versions are within accepted range
  if (ns_ver != schema_ver) {
    cli::cli_warn(c("!" = "There is a mismatch between your namespace version ({.val {ns_ver}}) and your schema version ({.val {schema_ver}})."))
  }
  if (ns_ver < oldest_accepted_ver || schema_ver < oldest_accepted_ver) {
    cli::cli_warn(c("!" = "You are using an old EML version: {oldest_accepted_ver} or later is recommended."))
  } else if (ns_ver > newest_accepted_ver || schema_ver > newest_accepted_ver) {
    cli::cli_abort(c("x" = "You are using an unsupported EML version: versions beyond {newest_accepted_ver} not accepted."))
  } else {
    cli::cli_inform(c("v" = "Your EML version is supported."))
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
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_validate_schema(meta)
test_validate_schema <- function(metadata = load_metadata(here::here())) {

  val <- EML::eml_validate(metadata)
  if (val == TRUE) {
    cli::cli_inform(c("v" = "Your metadata is schema valid."))
  } else {
    attribs <- attributes(val)
    issues <- c()
    if ("errors" %in% names(attribs)) {
      names(attribs$errors) <- rep("x", length(attribs$errors))
      issues <- c(issues, attribs$errors)
    }
    if ("warnings" %in% names(attribs)) {
      names(attribs$warnings) <- rep("!", length(names(attribs$warnings)))
      issues <- c(issues, attribs$warnings)
    }
    cli::cli_abort(c("Your metadata is schema-invalid. See output from {.fn EML::eml_validate} below.", issues))
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
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_footer(meta)
test_footer <- function(metadata = load_metadata(here::here())) {

  if (is.null(arcticdatautils::eml_get_simple(metadata, "numFooterLines"))) {
    cli::cli_inform(c("v" = "Metadata indicates data files do not have footers."))
  } else {
    cli::cli_abort(c("x" = "Metadata indicates that data files include footers. Please remove all footers from data files."))
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
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_header_num(meta)
test_header_num <- function(metadata = load_metadata(here::here())) {

  tbl_metadata <- EML::eml_get(metadata, "dataTable")
  if ("entityName" %in% names(tbl_metadata)) {
    tbl_metadata <- list(tbl_metadata)  # Handle single data table case
  }
  bad_headers <- sapply(tbl_metadata, function(tbl) {
    header_lines <- arcticdatautils::eml_get_simple(tbl, "numHeaderLines")
    return(tibble::tibble(table_name = tbl$entityName,
                          header_lines = ifelse(is.null(header_lines), NA, header_lines)))
  },
  simplify = FALSE)
  bad_headers$`@context` <- NULL
  bad_headers <- do.call(rbind, bad_headers)
  bad_headers <- dplyr::filter(bad_headers, is.na(header_lines) | header_lines != "1")

  if(nrow(bad_headers) == 0) {
    cli::cli_inform(c("v" = "Metadata indicates that each data file contains exactly one header row."))
  } else if (all(is.na(bad_headers$header_lines))) {
    cli::cli_abort(c("x" = "Metadata does not contain information about number of header rows"))
  } else {
    wrong_headers <- paste0(bad_headers$table_name, ": ", bad_headers$header_lines)
    names(wrong_headers) <- rep("*", length(wrong_headers))
    cli::cli_abort(c("x" = "Metadata indicates that the following data files contain either zero or more than one header row:", wrong_headers))
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

#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_delimiter(meta)
test_delimiter <- function(metadata = load_metadata(here::here())) {

  tbl_metadata <- EML::eml_get(metadata, "dataTable")
  if ("entityName" %in% names(tbl_metadata)) {
    tbl_metadata <- list(tbl_metadata)  # Handle single data table case
  }
  bad_delimit <- sapply(tbl_metadata, function(tbl) {
    delimit <- tryCatch(arcticdatautils::eml_get_simple(tbl, "fieldDelimiter"),
                        error = function(e) {
                          if (grepl("not recognized", e$message)) {
                            "[INVALID]"
                          } else {
                            e
                          }
                        })
    return(tibble::tibble(table_name = tbl$entityName,
                          delimiter = ifelse(is.null(delimit), NA, delimit)))
  },
  simplify = FALSE)
  bad_delimit$`@context` <- NULL
  bad_delimit <- do.call(rbind, bad_delimit)

  if (is.null(bad_delimit) || all(is.na(bad_delimit$delimiter))) {
    cli::cli_abort(c("x" = "Metadata does not contain information about the field delimiter for data files"))
  }

  bad_delimit <- dplyr::filter(bad_delimit, is.na(delimiter) | nchar(delimiter) != 1 | delimiter == "[INVALID]")

  if (nrow(bad_delimit) == 0) {
    cli::cli_inform(c("v" = "Metadata indicates that each data file contains a field delimiter that is a single character"))
  }
  else {
    wrong_delimiters <- bad_delimit$table_name
    names(wrong_delimiters) <- rep("*", length(wrong_delimiters))
    cli::cli_abort(c("x" = "Metadata indicates that the following data files do not contain valid delimiters:", wrong_delimiters))
  }

  return(invisible(metadata))
}

#' Test Metadata for Duplicate Filenames
#'
#' @description test_dup_meta_entries test to see whether there are duplicate filenames listed for the data files in (EML) metadata.
#'
#' @details specifically, test_dup_meta_entries looks at the 'physical' elements of a metadata file, which describe each data file, and asks whether there are duplicates entries under the objectName child element, which is where the file name for each data file is stored.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_dup_meta_entries(meta)
test_dup_meta_entries <- function(metadata = load_metadata(here::here())) {


  # get physical elements (and all children elements)
  attribs <- EML::eml_get(metadata, "physical")

  # list all file names held in "objectName"
  fn <- unlist(attribs)[grepl("objectName", names(unlist(attribs)), fixed = T)]

  if (length(fn) == 0) {
    cli::cli_abort(c("x" = "Metadata file name check failed. No file names found in metadata."))
  }

  # find duplicate entries:
  dups <- fn[duplicated(fn)]

  # if no duplicates, test passed:
  if (length(dups) == 0) {
    cli::cli_inform(c("v" = "Each data file name is used exactly once in the metadata file."))
  } else if (length(dups > 0)) {  # if duplicates, test failed:
    names(dups) <- rep("*", length(dups))
    cli::cli_abort(c("x" = "Metadata file name check failed. Some filenames are used more than once in the metadata:", dups))
  }

  return(invisible(metadata))
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
#' @examples
#' dir <- DPchecker_example("BICY_veg")
#' test_file_name_match(dir)
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
    cli::cli_inform(c("v" = "All data files are listed in metadata and all metadata files names refer to data files."))
  } else if (length(meta_only) > 0 || length(dir_only) > 0) {
    msg <- c()
    if (length(meta_only > 0)) {
      names(meta_only) <- rep("*", length(meta_only))
      msg <- c("x" = "{length(meta_only)} file{?s} listed in metadata but missing from data folder", meta_only)
    }
    if (length(dir_only) > 0) {
      names(dir_only) <- rep("*", length(dir_only))
      msg <- c(msg, "x" = "{length(dir_only)} file{?s} present in data folder but missing from metadata", dir_only)
    }
    cli::cli_abort(msg)
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
#' dir <- DPchecker_example("BICY_veg")
#' test_fields_match(dir)
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
    cli::cli_abort(c("x" = "Mismatch in data filenames and files listed in metadata. You must resolve this issue before you can verify that fields match. Call {.fn test_file_name_match} for more info."))
  }

  # Check each CSV. If column and attributes are a mismatch, describe the issue
  mismatches <- sapply(data_files, function(data_file) {
    meta_cols <- metadata_attrs[[data_file]]
    data_cols <- data_colnames[[data_file]]
    if (length(meta_cols) == length(data_cols) && all(meta_cols == data_cols)) {  # Columns match and are in right order
      return(NULL)
    } else if (all(meta_cols %in% data_cols) && all(data_cols %in% meta_cols)) {  # Columns match and are in wrong order
      return(c(" " = paste0("--> {.file ", data_file, "}: Metadata column order does not match data column order")))
    } else {  # Columns don't match
      missing_from_meta <- data_cols[!(data_cols %in% meta_cols)]
      if (length(missing_from_meta) > 0) {
        missing_from_meta <- paste0("----Missing from metadata: ", paste0("{.field ", missing_from_meta, "}", collapse = ", "))
        names(missing_from_meta) <- " "
      }
      missing_from_data <- meta_cols[!(meta_cols %in% data_cols)]
      if (length(missing_from_data) > 0) {
        missing_from_data <- paste0("----Missing from data file: ", paste0("{.field ", missing_from_data, "}", collapse = ", "))
        names(missing_from_data) <- " "
      }
      msg <- c(" " = paste0("--> {.file ", data_file, "}:"), missing_from_meta, missing_from_data)
      return(msg)
    }
  }, USE.NAMES = FALSE, simplify = FALSE)

  # Remove tables from list that pass the test, and convert it to a named vector
  mismatches <- purrr::discard(mismatches, is.null) %>%
    unlist()

  # If there are mismatches, throw an error, otherwise, print a message indicating passed test
  if (!is.null(mismatches)) {
    cli::cli_abort(c("x" = "Column mismatch between data and metadata.", mismatches))
  } else {
    cli::cli_inform(c("v" = "All columns in data match all columns in metadata."))
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
#' dir <- DPchecker_example("BICY_veg")
#' test_numeric_fields(dir)
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
      return(paste0("{.field ", non_numeric, "}", collapse = ", "))  # List non-numeric columns
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
    msg <- paste0("--> {.file ", names(data_non_numeric), "}:  ", data_non_numeric)
    names(msg) <- rep(" ", length(msg))
    msg <- c("x" = "Columns indicated as numeric in metadata contain non-numeric values:", msg)
    cli::cli_abort(msg)
  } else {
    cli::cli_inform(c("v" = "Columns indicated as numeric in metadata contain only numeric values and valid missing value codes."))
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
#' dir <- DPchecker_example("BICY_veg")
#' test_date_range(dir)
test_date_range <- function(directory = here::here(), metadata = load_metadata(directory)) {

  missing_temporal <- is.null(arcticdatautils::eml_get_simple(metadata, "temporalCoverage"))

  # Check if temporal coverage info is complete. Throw a warning if it's missing entirely and an error if it's only partially complete.
  # The logic being that maybe there's a scenario where temporal coverage isn't relevant to the dataset at all, but if it has date/time info, it has to have both a start and end.
  if (missing_temporal) {
    cli::cli_warn(c("!" = "Could not check date range. Metadata does not contain temporal coverage information."))
    return(invisible(metadata))
  }

  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  data_tbl$`@context` <- NULL
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get begin date from metadata
  meta_begin_date <- readr::parse_datetime(EMLeditor::get_begin_date(metadata), format = "%d %B %Y")
  meta_end_date <- readr::parse_datetime(EMLeditor::get_end_date(metadata), format = "%d %B %Y")
  meta_date_range <- c(begin = meta_begin_date, end = meta_end_date)

  if (any(is.na(meta_date_range))) {
    missing_date <- names(meta_date_range[is.na(meta_date_range)])
    present_date <- names(meta_date_range[!is.na(meta_date_range)])
    cli::cli_warn(c("!" = paste("Metadata temporal coverage is missing", missing_date, "date.")))
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
    char_data <- suppressWarnings(readr::read_csv(file.path(directory, data_file), col_select = dplyr::all_of(dttm_col_names), na = na_strings, col_types = rep("c", length(dttm_col_names)), show_col_types = FALSE))

    tbl_out_of_range <- sapply(names(dttm_data), function(col) {
      col_data <- dttm_data[[col]]
      orig_na_count <- sum(is.na(char_data[[col]]))
      if (all(is.na(col_data))) {
        return(paste0("{.field ", col, "} (failed to parse)"))
      } else if (sum(is.na(col_data)) > orig_na_count) {
        return(paste0("{.field ", col, "} (partially failed to parse)"))
      }
      max_date <- max(col_data, na.rm = TRUE)
      min_date <- min(col_data, na.rm = TRUE)
      format_str_r <- dttm_formats_r[col]
      bad_cols <- NULL

      # Compare years first, and only compare months and days if they are included in the format string
      if (lubridate::year(max_date) > lubridate::year(meta_end_date) || lubridate::year(min_date) < lubridate::year(meta_begin_date)) {
        bad_cols <- paste0("{.field ", col, "} [{.val ", format(min_date, format_str_r), "}, {.val ", format(max_date, format_str_r), "}]") # column name and actual date range
      } else if (grepl("%m", format_str_r) && (lubridate::month(max_date) > lubridate::month(meta_end_date) || lubridate::month(min_date) < lubridate::month(meta_begin_date))) {
        bad_cols <- paste0("{.field ", col, "} [{.val ", format(min_date, format_str_r), "}, {.val ", format(max_date, format_str_r), "}]") # column name and actual date range
      } else if (grepl("%d", format_str_r) && (lubridate::day(max_date) > lubridate::day(meta_end_date) || lubridate::day(min_date) < lubridate::day(meta_begin_date))) {
        bad_cols <- paste0("{.field ", col, "} [{.val ", format(min_date, format_str_r), "}, {.val ", format(max_date, format_str_r), "}]") # column name and actual date range
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
    msg <- paste0("--> {.file ", names(dataset_out_of_range), "}:  ", dataset_out_of_range)
    names(msg) <- rep(" ", length(msg))
    err <- paste0("The following date/time columns are out of the range [{.val ", meta_begin_date, "}, {.val ", meta_end_date, "}] specified in the metadata:")
    if (any(grepl("failed to parse", msg))) {
      cli::cli_abort(c("x" = err, msg))  # Throw an error if some dates won't parse
    } else {
      cli::cli_warn(c("!" = err, msg))  # If dates all parse but are out of range, just throw a warning.
    }
  } else {
    cli::cli_inform(c("v" = "Columns indicated as date/time in metadata are within the stated temporal coverage range."))
  }

  return(invisible(metadata))
}

#' Check for Taxonomic Coverage
#' Checks if taxonomic coverage element is present in metadata. Does not perform any validation of taxonomic coverage information.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_taxonomic_cov(meta)
test_taxonomic_cov <- function(metadata = load_metadata(directory)) {

  missing_taxonomic <- is.null(arcticdatautils::eml_get_simple(metadata, "taxonomicCoverage"))

  if (missing_taxonomic) {
    cli::cli_warn(c("!" = "Metadata does not contain taxonomic coverage information."))
  } else {
    cli::cli_inform(c("v" = "Metadata contains taxonomic coverage element."))
  }

  return(invisible(metadata))
}

#' Check for Geographic Coverage
#' Checks if geographic coverage element is present in metadata. Does not perform any validation of geographic coverage information.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_geographic_cov(meta)
test_geographic_cov <- function(metadata = load_metadata(directory)) {

  missing_geographic <- is.null(arcticdatautils::eml_get_simple(metadata, "geographicCoverage"))

  if (missing_geographic) {
    cli::cli_warn(c("!" = "Metadata does not contain geographic coverage information."))
  } else {
    cli::cli_inform(c("v" = "Metadata contains geographic coverage element"))
  }

  return(invisible(metadata))
}

#' Check for DOI
#' Checks if DOI is present in metadata. Does not currently validate DOI.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_geographic_cov(meta)
test_doi <- function(metadata = load_metadata(directory)) {

  doi <- arcticdatautils::eml_get_simple(metadata, "alternateIdentifier")
  missing_doi <- is.null(doi) || !any(grepl("^doi\\:", doi))

  if (missing_doi) {
    cli::cli_warn(c("!" = "Metadata does not contain a digital object identifier."))
  } else {
    cli::cli_inform(c("v" = "Metadata contains a digital object identifier."))
  }

  return(invisible(metadata))
}

#' Check for Publisher
#' Checks if publisher information is present in metadata, with option to require valid NPS publisher information.
#'
#' @inheritParams test_metadata_version
#' @param require_nps If TRUE, throw an error if publisher information is not correct for NPS published data.
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_publisher(meta)
test_publisher <- function(metadata = load_metadata(directory), require_nps = FALSE) {

  pub <- EML::eml_get(metadata, "publisher")
  # Convert to a vector for easier comparison
  if (!is.null(pub)) {
    pub$`@context` <- NULL
    pub <- unlist(pub) %>%
      sort()
  }

  valid_nps_pub <- list(
    organizationName =
      "National Park Service",
    address = list(
      deliveryPoint = "1201 Oakridge Drive, Suite 150",
      city = "Fort Collins",
      administrativeArea = "CO",
      postalCode = "80525",
      country = "USA"
    ),
    onlineUrl = "http://www.nps.gov",
    electronicMailAddress = "irma@nps.gov",
    userId = list(directory = "https://ror.org/", userId = "https://ror.org/044zqqy65")
  ) %>%
    unlist() %>% # Convert to vector for easier comparison
    sort()

  if (is.null(pub)) {
    cli::cli_abort(c("x" = "Metadata does not contain publisher information."))
  } else if (!require_nps) {
    cli::cli_inform(c("v" = "Metadata contains publisher element."))
  } else if (identical(valid_nps_pub, pub)) {
    cli::cli_inform(c("v" = "Metadata contains publisher element and correctly designates NPS as the publisher."))
  } else {
    cli::cli_abort(c("x" = "Metadata contains publisher element but does not correctly designate NPS as the publisher."))
  }

  return(invisible(metadata))
}

#' Test Field Names for Invalid Characters
#'
#' @description test_valid_fieldnames checks for field names in the metadata that contain invalid special characters. Only underscores and alphanumeric characters are permitted, and names must begin with a letter.
#'
#' @details You should run `test_fields_match()` before you run this function, since this function only checks the field names in the metadata.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_valid_fieldnames(meta)
test_valid_fieldnames <- function(metadata = load_metadata(here::here())) {

  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get list of columns for each table in the metadata
  metadata_attrs <- lapply(data_tbl, function(tbl) {arcticdatautils::eml_get_simple(tbl, "attributeName")})
  metadata_attrs$`@context` <- NULL
  names(metadata_attrs) <- arcticdatautils::eml_get_simple(data_tbl, "objectName")

  # Check each table. Throw a warning if they contain special characters
  bad_fieldnames <- sapply(names(metadata_attrs), function(tbl) {
    cols <- metadata_attrs[[tbl]]
    bad_start <- grepl("^[^a-zA-Z]", cols)  # Col names must start with a letter
    special_chars <- grepl("[^a-zA-Z0-9_\\.]", cols)  # No special characters in col names (only alphanumeric and underscores allowed)

    bad_cols <- cols[bad_start | special_chars]

    if (length(bad_cols) == 0) {  # No problems
      return(NULL)
    } else {
      msg <- c(" " = paste0("--> {.file ", tbl, "}: ", paste0("{.field ", bad_cols, "}", collapse = ", ")))
      return(msg)
    }
  }, USE.NAMES = FALSE, simplify = FALSE)

  # Remove tables from list that pass the test, and convert it to a named vector
  bad_fieldnames <- purrr::discard(bad_fieldnames, is.null) %>%
    unlist()

  # If there are mismatches, throw an error, otherwise, print a message indicating passed test
  if (!is.null(bad_fieldnames)) {
    cli::cli_warn(c("!" = "Some field names contain special characters and/or do not begin with a letter:", bad_fieldnames))
  } else {
    cli::cli_inform(c("v" = "Field names begin with a letter and do not contain spaces or special characters."))
  }

  return(invisible(metadata))
}

#' Test File Names for Invalid Characters
#'
#' @description test_valid_filenames checks for file names in the metadata that contain invalid special characters. Only underscores and alphanumeric characters are permitted, and names must begin with a letter.
#'
#' @details You should run `test_file_name_match()` before you run this function, since this function only checks the file names in the metadata.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_valid_filenames(meta)
test_valid_filenames <- function(metadata = load_metadata(here::here())) {

  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get vector of filenames from the metadata
  file_names <- arcticdatautils::eml_get_simple(data_tbl, "objectName")

  # Check each file name. Throw a warning if any contain special characters
  bad_start <- grepl("^[^a-zA-Z]", file_names)  # File names must start with a letter
  special_chars <- grepl("[^a-zA-Z0-9_\\.]", file_names)  # No special characters in file names (only alphanumeric and underscores allowed)

  bad_names <- file_names[bad_start | special_chars]

  # If there are mismatches, throw an error, otherwise, print a message indicating passed test
  if (length(bad_names) > 0) {
    cli::cli_warn(c("!" = paste("Some file names contain special characters and/or do not begin with a letter:", paste0("{.file ", bad_names, "}", collapse = ", "))))
  } else {
    cli::cli_inform(c("v" = "File names begin with a letter and do not contain spaces or special characters."))
  }

  return(invisible(metadata))
}

#' Run all congruence checks
#'
#' @param check_metadata_only Only run checks on the metadata and skip anything involving data files.
#' @param output_filename Optional. If specified, saves results of congruence checks to this file. If omitted, prints results to console. If the file already exists, results will be appended to the existing file.
#' @param output_dir Location in which to save the output file, if using.
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' dir <- DPchecker_example("BICY_veg")
#' run_congruence_checks(dir)
#'
run_congruence_checks <- function(directory = here::here(), metadata = load_metadata(directory), check_metadata_only = FALSE, output_filename, output_dir = here::here()) {

  err_count <- 0
  warn_count <- 0
  total_count <- 10  # Don't forget to update this number when adding more checks!

  if (!missing(output_filename)) {
    output_dir <- normalizePath(output_dir, winslash = .Platform$file.sep, mustWork = TRUE)
    output_path <- file.path(output_dir, output_filename)
    open_mode <- if (file.exists(output_path)) {
      "at"  # if file exists, use append mode
    } else {
      "wt"  # If the file doesn't already exist, use write mode
    }
    file <- file(output_path, open = open_mode)
    sink(file)
    sink(file, type = "message")
    if (open_mode == "at") {
      cli::cli_verbatim("\n\n\n")  # If appending to existing log, add a few newlines to make it more readable
    }
    cli::cli_rule(center = "{Sys.time()}")
    cli::cli_inform("The following checks were run using DPchecker version {packageVersion('DPchecker')}.")
  }

  if (check_metadata_only) {
    cli::cli_h1("Running metadata-only checks (skipping checks against data files)")
  } else {
    cli::cli_h1("Running all congruence checks")
  }

  cli::cli_h2("Reading metadata")
  tryCatch(invisible(metadata),  # load_metadata from the function args actually gets evaluated here
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             try({
               if (grepl("sewright", Sys.getenv("USERNAME"), ignore.case = TRUE)) {
                 rstudioapi::viewer(url = system.file("extdata", "pebkac.jpg", package = "DPchecker", mustWork = TRUE))
               }
             })
             cli::cli_abort(c("x" = "You must correct the above issue before the congruence checks can run."), call = NULL)},
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           })
  cli::cli_h2("Checking metadata compliance")
  tryCatch(test_validate_schema(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_alert_danger("Schema validation failed. Run {.fn test_validate_schema} for details.")
             cli::cli_abort(c("x" = "Metadata schema must validate before the rest of the congruence checks can run."), call = NULL)},
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_alert_warning("Schema validation warnings exist. Run {.fn test_validate_schema} for details.", call = NULL)
           })
  tryCatch(test_dup_meta_entries(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
             cli::cli_abort(c("x" = "You must remove duplicate data table names from metadata before the rest of the congruence checks can run."), call = NULL)},
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           })
  tryCatch(test_metadata_version(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_delimiter(metadata),
           error = function(e) {
             cli::cli_bullets(c(e$message, e$body))
             err_count <<- err_count + 1
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_header_num(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_footer(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_taxonomic_cov(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_geographic_cov(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_doi(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_publisher(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_valid_fieldnames(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })
  tryCatch(test_valid_filenames(metadata),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(e$message, e$body))
           })

  if (!check_metadata_only) {
    cli::cli_h2("Checking that metadata is consistent with data file(s)")
    tryCatch(test_file_name_match(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               cli::cli_abort(c("x" = "Files documented in metadata must match files present in package before the rest of the congruence checks can run."), call = NULL)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(e$message, e$body))
             })
    tryCatch(test_fields_match(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
               cli::cli_abort(c("x" = "Columns documented in metadata must match columns present in data files before the rest of the congruence checks can run."), call = NULL)
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(e$message, e$body))
             })
    tryCatch(test_numeric_fields(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             })
    tryCatch(test_date_range(directory, metadata),
             error = function(e) {
               err_count <<- err_count + 1
               cli::cli_bullets(c(e$message, e$body))
             },
             warning = function(w) {
               warn_count <<- warn_count + 1
               cli::cli_bullets(c(w$message, w$body))
             })
  }

  cli::cli_h2("Summary")
  if (err_count > 0) {
    cli::cli_alert_danger("{err_count} errors to address")
  }
  if (warn_count > 0) {
    cli::cli_alert_warning("{warn_count} warnings to look into")
  }
  if (warn_count + err_count == 0) {
    check_type <- if (check_metadata_only) {
      "metadata"
    } else {
      "congruence"
    }
    cli::cli_alert_success("Success! All {check_type} checks passed.")
  }

  if (!missing(output_filename)) {
    sink(type = "message")
    sink()
    close(file)
    file.show(output_path)  # Opens log file. May want to add option in future to not do this
  }
  return(invisible(c("errors" = err_count, "warnings" = warn_count)))
}

#' Generate path to example data
#'
#' @param dp_name Name of data package.
#'
#' @return Path to example data, if dp_name is specified.
#' @export
#'
#' @examples
#' DPchecker_example()
#' DPchecker_example("BUIS_herps")
DPchecker_example <- function(dp_name = c("BICY_veg", "BUIS_herps")) {
  dp_name <- match.arg(dp_name)
  message("Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.")
  system.file("extdata", dp_name, package = "DPchecker", mustWork = TRUE)
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
#'
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
