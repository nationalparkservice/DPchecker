#' Test publication date presence and ISO-8601 formatting
#'
#' @description tests for presence and ISO-8601 formatting of the publication date. Also tests whether the publication date is within a reasonable bounds; i.e. not a date prior to the beginning of the data package Reference Type (after 2022) and not in a future year.
#'
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata(DPchecker_example("BICY_Veg"))
#' test_pub_date(meta)
#' }
 test_pub_date <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)  # Throw an error if metadata isn't an emld object
  missing_date <-is.null(metadata$dataset$pubDate)
  #error if no publication date found
  z <- NULL #error tracking
  if(missing_date){
    cli::cli_abort(c("x"= "Metadata does not contain publication date."))
    z <- "error"
  }
  #if publication date found, check formatting
  if(!missing_date){
    date <- metadata$dataset$pubDate
    pub_year <- suppressWarnings(as.numeric(substr(date,1,4)))
    curr_year <- as.numeric(substr(Sys.Date(),1,4))
    dash1 <- substr(date,5,5)
    pub_month <-suppressWarnings(as.numeric(substr(date,6,7)))
    dash2 <- substr(date,8,8)
    pub_day <- suppressWarnings(as.numeric(substr(date,9,10)))

      #test for improbable years or non-numerics in year:
    if(is.na(pub_year)){
      cli::cli_abort(c("x" = "Publication year is not in the correct ISO 8601 format (YYYY-MM-DD)."))
      z <- "error"
    }
    if(!is.na(pub_year)){
      if(pub_year <= 2022){
        cli::cli_abort(c("x" = "Publication date, {pub_year}, predates the Data Package Reference Type."))
        z <- "error"
      }
      if(pub_year > curr_year){
        cli::cli_abort(c("x" = "Publication date, {pub_year}, exceeds the current year."))
        z <- "error"
      }
    }
    if(!identical(dash1, "-")){
      cli::cli_abort(c("x" =
        "Publication date separator, {dash1}, is not in the correct ISO 8601 format (YYYY-MM-DD)."))
      z <- "error"
    }
    if(is.na(pub_month)){
      cli::cli_abort(c("x" =
        "Publication month is not in the correct ISO 8601 format (YYY-MM-DD)"))
      z <- "error"
    }
    if(!is.na(pub_month)){
      if(pub_month > 12){
        cli::cli_abort(c("x" = "Publication month, {pub_month}, is >12 and is not in the correct ISO 8601 format (YYYY-MM-DD)."))
        z <- "error"
      }
    }
    if(!identical(dash2, "-")){
      cli::cli_abort(c("x" =
        "Publication date separator, {dash2}, is not in the correct ISO 8601 format (YYYY-MM-DD)."))
      z <- "error"
    }
    if(is.na(pub_day)){
      cli::cli_abort(c("x" =
        "Publication day is not in the correct ISO 8601 format (YYYY-MM-DD)."))
      z <- "error"
    }
    if(!is.na(pub_day)){
      if(pub_day > 31){
        cli::cli_abort(c("x" =
        "Publication day, {pub_day}, is >31 and is not in the correct ISO 8601 format (YYYY-MM-DD)."))
        z <- "error"
      }
    }
    if(is.null(z)) {
      cli::cli_inform(c(
        "v" = "Publication date is present and in the correct ISO 8601 format."))
    }
  }
  return(invisible(metadata))
}

#' Test data package title
#'
#' @description tests EML metadata for presence of a data package title. Warns if the title is > 15 words.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_dp_title()
#' }
test_dp_title <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)  # Throw an error if metadata isn't an emld object
  title <- metadata$dataset$title
  missing_title <- is.null(title)
  #error if no publication title found
  if(missing_title){
    cli::cli_abort(c("x"= "Metadata does not contain a data package title."))
  }
  #if title present, check certain parameters:
  if(!missing_title){
    x <- sapply(strsplit(title, " "), length)
    if(x > 15){
      cli::cli_inform(c(
        "!" = "Data package title is >15 words. Consider a more concise title."))
    }
    if(x < 5){
      cli::cli_inform(c(
        "!" = "Data package title is <5 words. Consider a more informative title."))
    }
    else {
      cli::cli_inform(c(
        "v" = "Data package title is present in metadata."))
    }
  }
  return(invisible(metadata))
}

#' Test for "by or for NPS"
#'
#' @details test for presence of the "by or for NPS" field. Error if data are missing from metadata. Warns if the metadata indicate that the data were not created by or for the NPS (as this is expected to be relatively rare)
#'
#' @inheritParams test_pub_date
#' @return invisibly returns `metadata`
#'
#' @examples
#' \dontrun{
#' test_by_for_nps()
#' }
test_by_for_nps <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  nps <- arcticdatautils::eml_get_simple(metadata, "byOrForNPS")
  missing_nps <- is.null(nps)
  #error if no by or for nps
  if(missing_nps){
    cli::cli_abort(c("x" = "Metadata does not contain information about whether the data package was created by or for the NPS."))
  }
  if(!missing_nps){
    if(nps == "TRUE"){
      cli::cli_inform(c(
        "v" = "Metadata states data was created by or for NPS."))
    }
    else if(nps == "FALSE"){
      cli::cli_warn(c(
        "!" = "Metadata states data was NOT created by or for NPS. Are you sure this is correct?"))
    }
    else if(nps == "NULL"){
      cli::cli_abort(c("x" = 'Metadata indicates "by or for NPS" field set to NULL. This must be fixed.'))
    }
  }
  return(invisible(metadata))
}


#' Tests EML metadata for the publisher name
#'
#' @details error if the publisher name is missing, warning if the publisher name is not "National Park Service". Passes test if publisher name is "National Park Service"
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#'
#' @examples
#' \dontrun{
#' test_publishername()
#' }
test_publisher_name <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  publisher_name <- metadata$dataset$publisher$organizationName
  missing_name <- is.null(publisher_name)
  if(missing_name){
    cli::cli_abort(c("x" = "Metadata does not contain the publisher name."))
  }
  if(!missing_name){
    if(publisher_name == "National Park Service"){
      cli::cli_inform(c(
        "v" = "Metadata indicates the publisher is the National Park Service."))
    }
    else if(publisher_name != "National Park Service"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher is {
                      crayon::bold$red(publisher_name)},
                      not the National Park Service. Are you sure?"))
    }
  }
  return(invisible(metadata))
}


#' Test EML metadata for publisher state - made redundant by test_publisher
#'
#' @description Inspects the publisher address in EML metadata. Returns an error if the administrativeArea (state) element is empty. Returns a warning if it is not "CO". Passes test if it is "CO".
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_publisher_state()
#' }
test_publisher_state <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  pub_state <- metadata$dataset$publisher$address$administrativeArea
  missing_state <- is.null(pub_state)
  if(missing_state){
    cli::cli_abort(c("x"= "Metadata does not contain the publisher state."))
  }
  if(!missing_state){
    if(pub_state == "CO"){
      cli::cli_inform(c("v" = "Metadata indicates the publisher state is CO."))
    }
    else if(pub_state != "CO"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher state, {
                      crayon::bold$red(pub_state)}, is not CO. Are you sure?"))
    }
  }
  return(invisible(metadata))
}

#' Test EML metadata for publisher city - made redundant by test_publisher
#'
#' @description Inspects the publisher address in EML metadata. Returns an error if the city element is empty. Returns a warning if it is not "Fort Collins". Passes test if it is "Fort Collins"
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_publisher_city()
#' }
test_publisher_city <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  pub_city <- metadata$dataset$publisher$address$city
  missing_city <- is.null(pub_city)
  if(missing_city){
    cli::cli_abort(c("x" = "Metadata does not contain the publisher city."))
  }
  if(!missing_city){
    if(pub_city == "Fort Collins"){
      cli::cli_inform(c(
        "v" = "Metadata indicates the publisher city is Fort Collins."))
    }
    if(pub_city != "Fort Collins"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher city, {crayon::bold$red(pub_city)}, is not Fort Collins."))
    }
  }
  return(invisible(metadata))
}


#' Test EML abstract
#'
#' @description Inspects EML for presence of a data package abstract. Fails if absent. If present, warns if the abstract is <20 words, >250 words, or contains a subset of common characters that indicate improper formatting. Otherwise passes.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_dp_abstract()
#' }
test_dp_abstract <- function(metadata = load_metadata(directory)){
  is_eml(metadata)
  abstract <- metadata$dataset$abstract
  missing_abstract <- is.null(abstract)
  if(missing_abstract){
    cli::cli_abort(c("x" = "Metadata does not contain an abstract for the data package."))
  }
  if(!missing_abstract){
    #get rid of <para> tags, empty paragraphs, etc
    abstract <- unlist(abstract)
    abs<-NULL
    for(i in seq_along(abstract)){
      if(nchar(abstract[i])>0){
        abs <- append(abs, abstract[[i]])
      }
    }
    x <- sapply(strsplit(abs, "" ), length)
    x <- sum(x) #sums up all words across potential multiple paragraphs
    z <- NULL #tracking for warnings
    if(x < 20){
      cli::cli_warn(c(
        "!" = "The data package abstract is less than 20 words. Are you sure this is informative enough?"))
      z <- "warn"
    }
    if(x > 250){
      cli::cli_warn(c(
        "!" = "The data package abstract is longer than 250 words. Consider revising for a more concise abstract."))
      z <- "warn"
    }
    if(sum(stringr::str_detect(abs, "&amp;#13;"))>0){
      cli::cli_warn(c(
        "!" = "The data package abstract contains non-standard characters: {
                      crayon::bold$red('&amp;#13;')}. "))
      z <- "warn"
    }
    if(sum(grepl("[\r?\n|\r]", abs))>0){
      cli::cli_warn(c("!" = "The data package abstract contains non-standard end of line characters such as \\r, \\n, or \\n\\r."))
      z <- "warn"
    }
    if(sum(grepl("   ", abs))>0){
      cli::cli_warn(c(
        "!" = "The data package abstract contains extra (more than two in a row) spaces."))
      z <- "warn"
    }
    if(length(seq_along(abs))>3){
      cli::cli_warn(c(
        "!" = "The data package abstract contains more than three paragraphs. Consider revising. Use {crayon::green("
      ))
    }
    #if no other warnings generated:
    if(is.null(z)){
      cli::cli_inform(c(
        "v" = "The Metadata contains a well formatted abstract for the data package."))
    }
  }
  return(invisible(metadata))
}

#' Test presence of file descriptions
#'
#' @description Tests for the presence of file descriptions (\<entityDescription\>) fields. It fails if any one or all of the \<entityDescription\> fields are empty in a dataTable. It fails if any two or more file descriptions are identical. Otherwise the test passes. The test generates a warning for each file description that is longer than 10 words or shorter than three words.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_file_descript()
#' }
test_file_descript <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  data_tbl$`@context` <- NULL
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }
  metadata_file_desc <- NULL
  for(i in seq_along(data_tbl)){
    metadata_file_desc <- append(metadata_file_desc,
                                 data_tbl[[i]][["entityDescription"]])
  }
  #if no file descriptions at all:
  if(is.null(metadata_file_desc)){
    cli::cli_abort(c("x" = "Metadata does not contain data file descriptions (<entityDescription>)."))
  }
  if(!is.null(metadata_file_desc)){
    #if missing individual file descriptions:
    z <- NULL #warn counter
    for(i in seq_along(metadata_file_desc)){
      if(is.null(metadata_file_desc[i])){
        cli::cli_abort(c("x" = "Metadata lacks a file description for file number {i}."))
        z <- "error"
      }
    }
    #if all dataTables have file descriptions (note: does not check against the
    #actual number of .csvs in the data package, for that see:
    #`test_file_name_match()`)
    if(is.null(z)){
      #find duplicates:
      dups<-metadata_file_desc[duplicated(metadata_file_desc)]
      if(length(dups) > 0){
        cli::cli_abort(c("x" = "Metadata file description check failed. Some descriptions are used multiple times to describe different files."))
      }
      else if(length(dups) == 0){
        cli::cli_inform(c("v" = "All dataTables listed in metadata have a unique file description."))
      }
      for(i in seq_along(metadata_file_desc)){
        x <- stringr::str_count(metadata_file_desc[i], '\\w+')
        if(x < 3){
          cli::cli_warn(c(
            "!" = "Data file {i} description is less than 3 words long. Consider a more informative descrption."))
        }
        if(x > 10){
          cli::cli_warn(c("!" = "Data file {i} description is greater than 10 words. Consider a more concise description."))
        }
      }
    }
  }
}


#' Test for CUI dissemination code
#'
#' @description Examines EML metadata for the presence of a Controlled Unclassified Information (CUI) dissemination code. The function returns an error if the code does not exist or does not match a list of valid codes. A present, valid code results in a pass.  however, if the code is not "PUBLIC" the user is warned.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_cui_dissemination()
#' }
test_cui_dissemination <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  valid_codes <- c("PUBLIC", "NOCON", "DL ONLY", "FEDCON", "FED ONLY")
  # get all additionalMetadata elelements and all children elements
  diss_code <- arcticdatautils::eml_get_simple(metadata, "CUI")
  if(is.null(diss_code)){
    cli::cli_abort(c("x" = "Metadata does not contain a CUI dissemination code."))
  }
  if(!is.null(diss_code)){
    if(!diss_code %in% valid_codes){
      cli::cli_abort(c("x" = "The CUI dissemination code {diss_code} is not a valid code."))
    }
    if(diss_code %in% valid_codes){
      cli::cli_inform(c("v" = "Metadata contains the valid CUI dissemination code {diss_code}."))
    }
    if(diss_code != "PUBLIC"){
      cli::cli_warn(c("!" = "Metadata CUI dissemination code {crayon::red$bold(diss_code)} indicates the data package is NOT public. Are you sure?"))
    }
  }
  return(invisible(metadata))
}

#' Test for presence of a license name
#'
#' @description Examines the licenseName element of EML metadata. If there is no license name, the test fails. If the license name does not match a list of valid license names, the test fails. If the metadata contain a valid license name, but the license name and CUI dissemination code do not agree, the test fails. Otherwise, the test passes.  Additionally, if the license name is not "Public Domain" or "CC0 1.0 Universal", the function throws a warning that the data package is not public.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_license()
#' }
test_license <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  license_list <- c("Public Domain", "CC0 1.0 Universal", "No License/Controlled Unclassified Information")

  license <- metadata$dataset$licensed$licenseName
  if(is.null(license)){
    cli::cli_abort(c("x" = "Metadata does not contain a license name."))
  }
  if(!is.null(license)){
    if(!license %in% license_list){
      cli::cli_abort(c(
        "x" = "The metadata does not contain a valid license name."))
    }
    if(license %in% license_list){
      diss_code <- arcticdatautils::eml_get_simple(metadata, "CUI")
      if(diss_code == "PUBLIC" & license == "No License/Controlled Unclassified Information"){
        cli::cli_abort(c(
          "x" = "Metadata license and CUI dissemination code do not agree."))
      }
      else if(diss_code != "PUBLIC" & license != "No Licnese/Controlled Unclassified Information"){
        cli::cli_abort(c(
          "x" = "Metadata license and CUI dissemination code do not agree."))
      }
      else {
        cli::cli_inform(c("v" = "Metadata contains a valid license name."))
      }
      if(license == "NO License/Controlled Unlcassified Information"){
      cli::cli_warn(c("!" = "Metadata license name indicates that the data package is NOT public. Are you sure?"))
      }
    }
  }
  return(invisible(metadata))
}

#' Test for presence of Intellectual Rights
#'
#' @description Test for the presence of text within the intellectualRights element in EML formatted metadata. If text if present, the test passes. Otherwise, the test fails. `test_int_rights()` makes no attempt to parse the text or test whether it properly coincides with the CUI dissemination codes or licenseName in the metadata. This is a simple presence/absence test.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_int_rights()
#' }
test_int_rights <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  int_rights <- metadata$dataset$intellectualRights
  if(is.null(int_rights)){
    cli::cli_abort(c("x" = "Metadata lacks an Intellectual Rights statemet."))
  }
  else {
    cli::cli_inform(c(
      "v" = "Metadata contains an Intellectual Rights statement."))
  }
  return(invisible(metadata))
}

#' Test metadata for attribute definitions
#'
#' @description Extracts all attributeNames and attributeDefinitions from EML metadata. It tests to make sure there are the same number of attributeNames and attributeDefinitions. If true, the test passes. If not, it produces an error. Note bene: this test ONLY looks at the metadata, it does NOT look at the data files. Passing this test does not mean that all data columns have attributes in the metadata associated with them. To test that, see `test_fields_match()`.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_attribute_defs()
#' }
test_attribute_defs <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get list of columns for each table in the metadata
  metadata_attrs <- lapply(data_tbl,
                           function(tbl)
                             {arcticdatautils::eml_get_simple(tbl,
                                                              "attributeName")})
  metadata_attrs$`@context` <- NULL
  #get attribute definitions:
  attr_defs <- lapply(data_tbl,
                      function (tbl)
                        {list(arcticdatautils::eml_get_simple(tbl,
                                                      "attributeDefinition"))})
  attr_defs$`@context` <- NULL
  #unlist:
  metadata_attrs <- unlist(metadata_attrs)
  attr_defs <- unlist(attr_defs)
  #comparisons:
  if(identical(seq_along(metadata_attrs), seq_along(attr_defs))){
    cli::cli_inform(c(
      "v" = "All attributes listed in metadata have attribute definitions."))
  }
  if(!identical(seq_along(metadata_attrs), seq_along(attr_defs))){
    cli::cli_abort(c(
      "x" = "Some metadata attributes are missing definitions (or vice versa)."))
  }
  return(invisible(metadata))
}

#' Tests for attribute storage type
#'
#' @description `test_storage_type()` checks to see if there are the same number of attributes (attributeName) and storageTypes in the metadata. Equal numbers of elements will pass; unequal will generate an error. `test_storage_type()` does NOT attempt to verify if the number of storageType elements matches the number of columns in the data package data files (for that functionality, use `test_fields_match()`). `test_storage_type()` does verify that the storageType is valid; i.e. is a member of an accepted list of possible storage types. Currently: string, float, date, factor, or characters. Validity for this test is based solely on observed ezEML/EAL output (in theory any string in storageType is schema-valid). `test_storage_type()` does NOT attempt to verify that the value in storageType logically matches the type data in the corresponding column.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_storage_type()
#' }
test_storage_type <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }

  # Get list of columns for each table in the metadata
  metadata_attrs <- lapply(data_tbl,
                           function(tbl)
                           {arcticdatautils::eml_get_simple(tbl,
                                                            "attributeName")})
  metadata_attrs$`@context` <- NULL
  #get attribute storage types:
  attr_storage_type <- lapply(data_tbl,
                      function (tbl)
                      {list(arcticdatautils::eml_get_simple(tbl,
                                                            "storageType"))})
  attr_storage_type$`@context` <- NULL
  #unlist:
  metadata_attrs <- unlist(metadata_attrs)
  attr_storage_type <- unlist(attr_storage_type)
  #comparisons:
  if(identical(seq_along(metadata_attrs), seq_along(attr_storage_type))){
    cli::cli_inform(c("v" = "All attributes listed in metadata have storage types associated with them."))
  }
  else {
    cli::cli_abort(c("x" = "Some metadata attributes are missing definitions (or vice versa)."))
  }
  attr_storage_list <- c("string", "float", "date", "factor", "character")
  if(sum(!attr_storage_type %in% attr_storage_list) > 0){
    cli::cli_warn(c("!" = "Some attribute storage types are not accepted values."))
  }
  else{
    cli::cli_inform(c("v" = "All attribute storage types are valid values."))
  }
  return(invisible(metadata))
}
