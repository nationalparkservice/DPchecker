#' Load Metadata
#'
#' @description `load_metadata()` loads the metadata file from a given path or directory.
#'
#' @details Given a path or directory - default is the working directory -  `load_metadata()` looks for files with the ending *_metadata.xml. The function quits with an error and tells the user if no such files are found or if more than one such file is found. If only one metadata file is found, it is checked for one of 3 formats: FGDC, ISO, or EML. Currently only EML is supported and the function will fail with an error, inform the user, and quit if non-EML metadata is found. The EML metadata file is loaded into R's work space for future use during congruence checking.
#'
#' In the context of National Park Service data packages, this function can be slightly easier to use for loading metadata into R than `EML::read_eml()` because it does require a filename or type be specified.
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
#' @description `load_data()` inspects the working directory for data files. Loads all existing data files into a tibble.
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
#' @description `test_metadata_version()` determines whether the version of the metadata supplied meets the current criteria for an NPS data package.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
#' @description `test_validate_schema()` inspects a metadata object loaded into R and determines whether it is schema-valid. If the test fails, the functio produces an error message.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
#' @description `test_footer()` checks the metadata files to determine whether data files contain footer lines or not.
#'
#' @details If footer lines are not present, the data package passes the test. If footer lines are present, the data package fails the test with an error and the user is instructed to remove footer lines prior to data package upload. Currently only EML metadata are supported.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

  if (is.null(arcticdatautils::eml_get_simple(metadata, "numFooterLines"))) {
    cli::cli_inform(c("v" = "Metadata indicates data files do not have footers."))
  } else {
    cli::cli_abort(c("x" = "Metadata indicates that data files include footers. Please remove all footers from data files."))
  }

  return(invisible(metadata))
}


#' Header Check
#'
#' @description `test_header_num()` checks the metadata files to ensure that each data file contains exactly one header row.
#'
#' @details `test_header_num()` examines the numHeaderLines element from EML (currently only EML is supported) metadata to determine how many header rows there are. If there are no header rows or if there is more than one header row, the test fails with an error. The test also fails with an error if there is no information about the number of header rows.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
#' @description `test_delimiter()` checks the metadata file and ensures that each data file has a field delimiter with exactly one character (e.g. ", ").
#'
#' @details `test_delimiter()` examines the fieldDelimiter element from EML (currently only EML is supported) metadata to determine how many characters there are. If there is no fieldDelimiter element, the test returns an error. If the field delimiter is anything other than exactly one character in length, the test returns an error.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
#' @description `test_dup_meta_entries()` tests to see whether there are duplicate filenames listed for the data files in (EML) metadata.
#'
#' @details specifically, `test_dup_meta_entries()` looks at the 'physical' elements of a metadata file, which describe each data file, and asks whether there are duplicates entries under the objectName child element, which is where the file name for each data file is stored. Duplicate entries will result in the test failing with an error.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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


#' Checks for consistency in data file URLs
#'
#' @description `test_datatable_urls` Checks to make sure that the URLs listed for data files correctly correspond to the DOI in metadata. If the last 7 digits of the URL for all data tables is identical to the last 7 digits of the DOI, the test passes. If there is no DOI, the test fails with a warning. If a data table lacks a URL, the test fails with an error. If any data table URL's last 7 digits are not identical to the DOI's last 7 digits, the test fails with an error.
#'
#' @details suggestions of which functions to use to correct errors/warnings are provided.
#'
#' @inheritParams test_metadata_version
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#'  \dontrun{
#' dir <- DPchecker_example("BICY_veg")
#' test_datatable_urls(dir)
#' }
#'
test_datatable_urls <- function (metadata = load_metadata(directory)) {
  is_eml(metadata)

  #get dataTable urls
  data_tbl <- EML::eml_get(metadata, "dataTable")
  data_tbl <- within(data_tbl, rm("@context"))

  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }
    #check for data table urls and get datastore reference IDs
  err_log <- NULL
  for(i in seq_along(data_tbl)){
    url <- data_tbl[[i]][["physical"]][["distribution"]][["online"]][["url"]]
    if(is.null(url)){
      tbl_name <- data_tbl[[i]][["physical"]][["objectName"]]
      err_log<-append(err_log,
                        paste0("--> {.file ", tbl_name, "} "))
    }
  }
  if(is.null(err_log)){
    cli::cli_inform(c(
      "v" = "Metadata contains URLs for all data tables."))
  } else {
    # really only need to say it once per file/column combo
    msg <- err_log
    err <- paste0("Metadata lacks URL(s) for the following dtata files. Use {.fn EMLeditor::set_data_urls} to add them.")
    cli::cli_abort(c("x" = err, msg))
  }
  return(invisible(metadata))
}

#' Tests for data table URL formatting & correspondance with DOI
#'
#' @description `test_datatable_urls_doi()` passes if all data tables have URLs that are properly formatted (i.e. "https://irma.nps.gov/DataStore/Reference/Profile/xxxxxxx") where "xxxxxx" is identical to the DOI specified in the metadata. Fails with a warning if there is no DOI specified in metadata. If a DOI is specified in metadata, but the data table URL does not properly coincide with the url for the landing page that the doi points to for any one table, the test fails with a warning (and indicates which table failed). If data table urls do not exist, fails with an error and indicates how to add them.
#'
#' @inheritParams test_metadata_version
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#' \dontrun{
#' dir <- DPchecker_example("BICY_veg")
#' test_datatable_urls_doi(dir)
#' }
test_datatable_urls_doi <-  function (metadata = load_metadata(directory)) {
  is_eml(metadata)

  #get dataTable urls
  data_tbl <- EML::eml_get(metadata, "dataTable")
  data_tbl <- within(data_tbl, rm("@context"))

  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # test for DOI presence
  doi <- metadata[["dataset"]][["alternateIdentifier"]]
  #if no DOI:
  if(is.null(doi)){
    cli::cli_warn(c("!" = "Metadata lacks a DOI. Cannot check for data table URL congruence with DOI. Use {.fn EMLeditor::set_doi} or {.fn EMLeditor::set_datastore_doi} to add a DOI."))
  }
  #if yes DOI:

  if(!is.null(doi)){
    #get 7 digit datastore ref
    ds_ref <- stringr::str_sub(doi, -7, -1)
    bad_url <- 0
    for(i in seq_along(data_tbl)){
      url <- data_tbl[[i]][["physical"]][["distribution"]][["online"]][["url"]]
      if(is.null(url)){
        cli::cli_abort(c("x" = "One or more data files lack URLs. Could not test whether URLs are properly formatted or correspond to the corect DOI. Use {.fn EMLeditor::set_data_urls} to add them."))
        return(invisible(metadata))
      }
      prefix <- stringr::str_sub(url, 1, stringr::str_length(url)-7)
      suffix <- stringr::str_sub(url, -7, -1)

      if(!prefix == "https://irma.nps.gov/DataStore/Reference/Profile/" ||
        !suffix == ds_ref){
        bad_url <- bad_url + 1
        tbl_name <- data_tbl[[i]][["physical"]][["objectName"]]
        cli::cli_abort(c(
          "x" = "Metadata url for the data table corresponding to",
          crayon::blue$bold(tbl_name),
          "is incorrectly formatted or does not correspond to the metadata DOI. Use {.fn EMLeditor::set_doi} or {.fn EMLeditor::set_datastore_doi} to edit DOIs or use {.fn EMLeditor::set_data_urls} to edit urls."))
      }
    }
    if(bad_url == 0){
      cli::cli_inform(
        c("v" = "Data table URLs are properly formmatted and correspond to the specified DOI."))
    }
  }
  return(invisible(metadata))
}


#' File Name Match
#'
#' @description `test_file_name_match()` checks to see whether all data files (.csv) within a specified directory are listed under the objectName (child of physical) element in an EML metadata file in the same directory, and vice versa. Mismatches will result in the test failing with an error message.
#'
#' @details If a directory other than the current working directory is specified, `test.file_name_match()` returns to the current working directory on exit. Note that the metadata file must follow NPS naming conventions, specifically ending in *_metadata.xml. `test.file_name_match()` assumes there are the same number of data files in the directory as dataTables in the metadata file.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
#' @description `test_fields_match()` compares the attributes of each dataTable within the EML metadata to the columns in the corresponding .csv. If the columns have the same names and order, the test passes. If the columns differ, the test fails with an error.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
    cli::cli_abort(c("x" = "Mismatch in data file column names and the attributes listed in metadata."))
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
#' @description `test_numeric_fields()` verifies that all columns listed as numeric in the metadata are free of non-numeric data. If non-numeric data are encountered, the test fails with an error.
#'
#' @details "NA" and missing data codes documented in the metadata will *not* cause this test to fail. Note that this test assumes that the column types that are in the metadata are the intended types, i.e., if your metadata says a column is text and it should actually be numeric, it will not be caught by this test. On the other hand, if your metadata indicates a text column is numeric, the function will generate an error.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
    #filter for just numeric attributes:
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
      return(NULL) #no numeric columns; go to next iteration in loop
    }
    #get missing data codes (so missing values to be ignored on data import)
    na_strings <- c("", "NA")
    if ("missingValueCode" %in% names(numeric_attrs[[data_file]])) {
      miss_values <- unique(numeric_attrs[[data_file]]$missingValueCode)
      if(!is.null(miss_values)){
          na_strings <- c(na_strings, miss_values)
      }
    }
    #only keep unique missingValueCodes
    na_strings <- unique(na_strings)
    #get rid of instances when value was actually not there in attribs.
    #Note: this still keeps the missingValueCode "NA".
    na_strings <- na_strings[!is.na(na_strings)]
    # Read everything as character, then see if it fails when converting to number
    num_data <- suppressWarnings(readr::read_csv(file.path(directory, data_file), col_select = dplyr::all_of(num_col_names), na = na_strings, col_types = rep("c", length(num_col_names)), show_col_types = FALSE))
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


#' Test data and metadata data formats match
#'
#' @description `test_dates_parse()` will examine all data columns that are described as containing dates and times. Although it can handle multiple different formats, the ISO-8601 format for dates and times is HIHGLY recommended (ISO is YYYY-MM-DDThh:mm:ss or just YYYY-MM-DD). The function will compare the format provided in the data files to the format indicated in metadata. If there are no dates indicated in the metadata, the test fails with a warning. If there are dates and the formats match, the test passes. If the formats do not match, the test fails with an error. The specific files and columns that failed are indicated in the results.
#'
#' @details `test_dates_parse()` will examine EVERY cell in a column of dates until it hits a date format that does not match the format specified in metadata. For large datasets, this process can take a minute or two. If there is even one typo in your data file, it will cause the function to throw an error. Frequent source of error include viewing dates in Excel, which can be deceptive, typos, and changes in the date format over time or with changing personnel.
#'
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`
#' @export
#'
#' @examples
#' dir <- DPchecker_example("BICY_veg")
#' test_dates_parse(dir)
test_dates_parse <- function(directory = here::here(),
                            metadata = load_metadata(directory)){

  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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

  #log errors. Assume none until one is found.
  error_log <- NULL

  for(i in 1:length(seq_along(data_files))){
    data_file <- data_tbl[[i]][["physical"]][["objectName"]]

    dttm_col_names <- dttm_attrs[[data_file]]$attributeName
    # If the table doesn't have any date/time columns listed in the metadata, it automatically passes
    if (length(dttm_col_names) == 0) {
      next
    }
    # Get format string for each date/time column and filter out anything that doesn't have a year associated with it
    dttm_formats <- dttm_attrs[[data_file]]$formatString
    is_time <- grepl("Y", dttm_formats)
    dttm_formats <- dttm_formats[is_time]
    dttm_col_names <- dttm_col_names[is_time]

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
      na_strings <- c(na_strings,
                      unique(dttm_attrs[[data_file]]$missingValueCode))
    }

    dttm_data <- suppressWarnings(
      readr::read_csv(
        file.path(directory, data_file),
        col_select = dplyr::all_of(dttm_col_names),
        na = na_strings,
        col_types = do.call(readr::cols, dttm_col_spec),
        show_col_types = FALSE))

    #Arooo?
    char_data <- suppressWarnings(
      readr::read_csv(file.path(directory, data_file),
                      col_select = dplyr::all_of(dttm_col_names),
                      na = na_strings,
                      col_types = rep("c", length(dttm_col_names)),
                      show_col_types = FALSE))

    for(j in 1:length(seq_along(dttm_col_names))){

      col_data <- dttm_data[[j]]
      orig_na_count <- sum(is.na(char_data[[j]]))
      if (all(is.na(col_data))) {
        error_log <- append(error_log, paste0("  ", "---> {.file ", data_file, "} {.field ", dttm_col_names[j], "} (failed to parse)"))
      } else if (sum(is.na(col_data)) > orig_na_count) {
        error_log <- append(error_log, paste0("  ---> {.file ", data_file, "} {.field ", dttm_col_names[j], "} (partially failed to parse)"))
      }
    }
  }
  if(is.null(error_log)){
    cli::cli_inform(c("v" = "Metadata and data date formatting is in congruence."))
  }
  else{
    # really only need to say it once per file/column combo
    msg <- error_log
    names(msg) <- rep(" ", length(msg))
    err <- paste0("Metadata/data date format mismatches found. Further temporal coverage tests will fail until this error is resolved:\n")
    cli::cli_abort(c("x" = err, msg))
  }
}


#' Test Date Range
#'
#' @description `test_date_range()` verifies that dates in the dataset are consistent with the date range in the metadata. It is HIGHLY recommended that you provide dates and times in ISO-8601 formatting: YYYY-MM-DDThh:mm:ss (if you don't have time you can us just the YYYY-MM-DD component).
#'
#' @details This function checks columns that are identified as date/time in the metadata. If the metadata lacks a date range, the function fails with a warning. It fails with an error if the dates contained in the columns are outside of the temporal coverage specified in the metadata. If the date/time format string specified in the metadata does not match the actual format of the date in the CSV, it will likely fail to parse and result failing the test with an error. Failure to parse is indicated in the results with the text "(failed to parse)".
#'
#' This test will also inform the user which file and columns are causing the test to fail and how it is failing (i.e. outside of the date range or failed to parse).
#'
#' If the date columns that are causing the test to fail are associated with the QA/QC process and are expected to fall outside the date range specified for the data, these columns can be omitted from the test using skip_cols.
#'
#' @inheritParams load_data
#' @inheritParams test_metadata_version
#' @param skip_cols String. Defaults to NA. One or more columns to omit from the test_date_range function.
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' dir <- DPchecker_example("BICY_veg")
#' test_date_range(dir)
test_date_range <- function(directory = here::here(),
                            metadata = load_metadata(directory),
                            skip_cols = NA){

  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
  firstDate <- arcticdatautils::eml_get_simple(metadata, "beginDate")
  if (is.null(firstDate)) {
    warning("Your metadata lacks a begining date.")
    firstDate <- NA
  } else {
    firstDate <- firstDate %>%
      as.Date() %>%
      format("%d %B %Y")
  }
  meta_begin_date <- readr::parse_datetime(firstDate, format = "%d %B %Y")

  lastDate<- arcticdatautils::eml_get_simple(metadata, "endDate")
  if (is.null(lastDate)) {
    warning("Your metadata lacks an ending date.")
    LastDate <- NA
  } else {
    lastDate <- lastDate %>%
      as.Date() %>%
      format("%d %B %Y")
  }
  meta_end_date <- readr::parse_datetime(lastDate, format = "%d %B %Y")

  meta_date_range <- c(begin = meta_begin_date, end = meta_end_date)

  if (any(is.na(meta_date_range))) {
    missing_date <- names(meta_date_range[is.na(meta_date_range)])
    present_date <- names(meta_date_range[!is.na(meta_date_range)])
    cli::cli_warn(c("!" = paste("Metadata temporal coverage is missing",
                                missing_date, "date.")))
  }

  # Get list of date/time attributes for each table in the metadata
  dttm_attrs <- lapply(data_tbl, function(tbl) {
    attrs <- suppressMessages(EML::get_attributes(tbl$attributeList))
    attrs <- attrs$attributes
    attrs <- dplyr::filter(attrs, domain == "dateTimeDomain")
    return(attrs)
  })
  dttm_attrs$`@context` <- NULL

  #### dropping skip_cols from date range check:
  if(sum(!is.na(skip_cols)) > 0){
    for(i in seq_along(dttm_attrs)){
      dttm_attrs[[i]]<-dplyr::filter(dttm_attrs[[i]],
                                     !attributeName %in% skip_cols)
    }
  }

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
    is_time <- grepl("Y", dttm_formats)
    dttm_formats <- dttm_formats[is_time]
    dttm_col_names <- dttm_col_names[is_time]

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
      na_strings <- c(na_strings,
                      unique(dttm_attrs[[data_file]]$missingValueCode))
    }

    dttm_data <- suppressWarnings(
      readr::read_csv(
        file.path(directory, data_file),
        col_select = dplyr::all_of(dttm_col_names),
        na = na_strings,
        col_types = do.call(readr::cols, dttm_col_spec),
        show_col_types = FALSE))

    #Arooo?
    char_data <- suppressWarnings(
      readr::read_csv(file.path(directory, data_file),
                      col_select = dplyr::all_of(dttm_col_names),
                      na = na_strings,
                      col_types = rep("c", length(dttm_col_names)),
                      show_col_types = FALSE))

    tbl_out_of_range <- sapply(names(dttm_data), function(col) {
      col_data <- dttm_data[[col]]
      orig_na_count <- sum(is.na(char_data[[col]]))
      if (all(is.na(col_data))) {
        return(paste0("{.field ", col, "} (failed to parse)"))
      } else if (sum(is.na(col_data)) > orig_na_count) {
        return(paste0("{.field ", col, "} (partially failed to parse)"))
      }
      #returns bad min/max dates because dates are not parsed as dates yet?
      max_date <- max(col_data, na.rm = TRUE)
      min_date <- min(col_data, na.rm = TRUE)


      #something funky here.... the next line returns NA
      format_str_r <- dttm_formats_r[col]
      bad_cols <- NULL

      #this should strip time from the dates
      max_date <- as.Date(format(max_date, format = dttm_formats_r[col]),
                          format = dttm_formats_r[col])
      min_date <- as.Date(format(min_date, format = dttm_formats_r[col]),
                          format = dttm_formats_r[col])

      #if no month/day in data format, reset to data date month/day to 1:


      # Set metadata day and/or month to 1 if not present in format string so that date comparisons work correctly
      if (!grepl("d", format_str_r)) {
        lubridate::day(meta_end_date) <- 1
        lubridate::day(meta_begin_date) <- 1
        lubridate::day(max_date) <- 1
        lubridate::day(min_date) <- 1
      }
      if (!grepl("m|b", format_str_r)) {
        lubridate::month(meta_end_date) <- 1
        lubridate::month(meta_begin_date) <- 1
        lubridate::month(max_date) <- 1
        lubridate::month(min_date) <- 1
      }
      # Compare min and max dates in data to begin and end dates in metadata
      if (max_date > meta_end_date || min_date < meta_begin_date) {
        bad_cols <- paste0("{.field ",
                           col, "} [{.val ",
                           format(min_date, format_str_r),
                           "}, {.val ",
                           format(max_date, format_str_r),
                           "}]") # column name and actual date range
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
    err <- paste0("The following date/time columns are out of the range [{.val ", meta_begin_date, "}, {.val ", meta_end_date, "}] specified in the metadata. To exclude QA/QC dates, re-run {.fn DPchecker::run_congruence_checks} with skip_cols set to the columns to skip.")
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
#'test
#' @description 'test_taxnomomic_cov()` checks whether taxonomic coverage element is present in metadata. It does not perform any validation of taxonomic coverage information. If taxonomic coverage is present, the test passes. If it is absent, the test fails with a warning.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

  missing_taxonomic <- is.null(arcticdatautils::eml_get_simple(metadata, "taxonomicCoverage"))

  if (missing_taxonomic) {
    cli::cli_warn(c("!" = "Metadata does not contain taxonomic coverage information."))
  } else {
    cli::cli_inform(c("v" = "Metadata contains taxonomic coverage element."))
  }

  return(invisible(metadata))
}

#' Check for Geographic Coverage
#'
#' @description `test_geographic_cov()` checks if geographic coverage element is present in metadata. It does not perform any validation of geographic coverage information. If the geographicCoverage element is present, the test passes. If it is absent, the test fails with a warning.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

  missing_geographic <- is.null(arcticdatautils::eml_get_simple(metadata, "geographicCoverage"))

  if (missing_geographic) {
    cli::cli_warn(c("!" = "Metadata does not contain geographic coverage information."))
  } else {
    cli::cli_inform(c("v" = "Metadata contains geographic coverage element"))
  }

  return(invisible(metadata))
}

#' Check for presence of a Digital Object Identifier
#'
#' @description `test_doi()` checks whether a DOI for the data package is present in metadata. It does not currently validate DOI. If a DOI is present, the test passes. If a DOI is not present, the test fails with a warning.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' meta <- load_metadata(DPchecker_example("BICY_veg"))
#' test_doi(meta)
test_doi <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

  doi <- metadata[["dataset"]][["alternateIdentifier"]]
  missing_doi <- is.null(doi) || !any(grepl("^doi\\:", doi))

  if (missing_doi) {
    cli::cli_warn(c("!" = "Metadata does not contain a digital object identifier. Use {.fn EMLeditor::set_doi} or {.fn EMLeditor::set_datastore_doi} to add a DOI."))
  } else {
    pass_message <- paste0("Metadata contains a digital object identifier, {.val ", doi, "}.")
    cli::cli_inform(c("v" = pass_message))
  }

  return(invisible(metadata))
}

#' Check DOI formatting
#'
#' @description `test_doi_format()` runs some basic formatting checks. If your DOI is absent, the test will fail with an error. If the DOI is not exactly 37 characters AND does not contain "doi: https://doi.org/10.57830/" the test will fail with an error. The test passes if the entry in the alternateIdentifier field is exactly 37 characters long and contains "doi: https://doi.org/10.57830/". Please note that this is a very cursory formatting check; it does not check to make sure the DOI is active (it probably should not be at this stage of data package authoring). It does not check to make sure it is correct or that it correctly corresponds to anything on DataStore or elsewhere within the metadata.
#'
#' @inheritParams test_metadata_version
#'
#' @return Invisibly returns `metadata`.
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- test_doi_format(metadata)
#' }
test_doi_format <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)  # Throw an error if metadata isn't an emld object
  doi <- metadata[["dataset"]][["alternateIdentifier"]]
  missing_doi <- is.null(doi) || !any(grepl("^doi\\:", doi))

  if (missing_doi) {
    cli::cli_warn(c("!" = "Metadata Digital Object Identifier is not properly formatted: DOI missing. Use {.fn EMLeditor::set_doi} or {.fn EMLeditor::set_datastore_doi} to add a DOI."))
  }
  if(nchar(doi) == 37 & grepl("doi: https://doi.org/10.57830/", doi)){
      cli::cli_inform(c("v" = "Metadata Digital Object Identifier appears to be properly formatted."))
  }
  if(nchar(doi) != 37 || !grepl("doi: https://doi.org/10.57830/", doi)){
    cli::cli_abort(c("x" = "Your DOI is improperly formatted. Use {.fn EMLeditor::set_doi} or {.fn EMLeditor::set_datastore_doi} to edit your DOI."))
  }
}

#' Check for Publisher
#' @description `test_publisher()` checks if publisher information is present in metadata, with option to require valid NPS publisher information. If the publisher information is present, the test passes. If the publisher information is absent, the test fails with an error. If require_nps is set to TRUE (defaults to FALSE), the test will also ensure that a valid NPS publisher information is present. In this case, even if the publisher element is present, the test will fail with an error unless the publisher is the NPS (and all the publisher fields exactly match the expected information for NPS data packages).
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
    cli::cli_abort(c("x" = "Metadata does not contain publisher information. Use {.fn EMLeditor::set_publisher} to add publisher information."))
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
#' @description `test_valid_fieldnames()` checks for field names (e.g data column names) in the metadata that contain invalid special characters. Only underscores and alphanumeric characters are permitted, and names must begin with a letter. If invalid column names exist, the test will fail with a warning, otherwise the test passes.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
    cli::cli_warn(c("!" = paste0("Some field names contain special characters and/or do not begin with a letter: ", bad_fieldnames)))
  } else {
    cli::cli_inform(c("v" = "Field names begin with a letter and do not contain spaces or special characters."))
  }

  return(invisible(metadata))
}

#' Test File Names for Invalid Characters
#'
#' @description `test_valid_filenames()` checks for file names in the metadata that contain invalid special characters. Only underscores, hyphens, and alphanumeric characters are permitted, and names must begin with a letter. Currently, invalid filenames will result in the test failing with a warning, otherwise the test passes.
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
  is_eml(metadata)  # Throw an error if metadata isn't an emld object

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
  special_chars <- grepl("[^a-zA-Z0-9_\\.\\-]", file_names)  # No special characters in file names (only alphanumeric and underscores allowed)

  bad_names <- file_names[bad_start | special_chars]

  # If there are mismatches, throw an error, otherwise, print a message indicating passed test
  if (length(bad_names) > 0) {
    cli::cli_warn(c("!" = paste("Some file names contain special characters and/or do not begin with a letter:", paste0("{.file ", bad_names, "}", collapse = ", "))))
  } else {
    cli::cli_inform(c("v" = "File names begin with a letter and do not contain spaces or special characters."))
  }

  return(invisible(metadata))
}

#' Check if metadata is eml object
#'
#' Helper function to validate that an argument belongs to the `emld` class.
#'
#' @inheritParams test_metadata_version
#'
is_eml <- function(metadata) {
  if (!any("emld" %in% class(metadata))) {
    cli::cli_abort(c("x" = "{.arg metadata} must be an EML object."), call = NULL)
  }
}
