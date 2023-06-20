#' Test publication date presence and ISO-8601 formatting
#'
#' @description `test_pub_date()` tests for presence and ISO-8601 formatting of the publication date. Also tests whether the publication date is within a reasonable bounds; i.e. not a date prior to the beginning of the data package Reference Type (after 2022) and not in a future year. If the publication date is missing or improperly formatted, the test fails with an error. If the publication date is outside a reasonable range, the test fails with a warning. Otherwiset the test passes.
#'
#' @param metadata The metadata object returned by `load_metadata`. If parameter is not provided, it defaults to calling `load_metadata` in the current project directory.#'
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
        cli::cli_warn(c("!" = "Publication date, {pub_year}, predates the Data Package Reference Type."))
        z <- "error"
      }
      if(pub_year > curr_year){
        cli::cli_warn(c("!" = "Publication date, {pub_year}, exceeds the current year."))
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
#' @description `test_dp_title()` tests EML metadata for presence of a data package title. The test fails with an error if the title is absent. The test fails with a warning if the title is > 20 or < 5 words. Otherwise, the test passes.
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
    cli::cli_abort(c("x"= "Metadata does not contain a data package title. Use {.fn EMLeditor::set_title} to add one."))
  }
  #if title present, check certain parameters:
  if(!missing_title){
    x <- sapply(strsplit(title, " "), length)
    if(x > 20){
      cli::cli_inform(c(
        "!" = "Data package title is >20 words. Consider a more concise title. Use {.fn EMLeditor::set_title} to revise."))
    }
    if(x < 5){
      cli::cli_inform(c(
        "!" = "Data package title is <5 words. Consider a more informative title. Use {.fn EMLeditor::set_title} to revise."))
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
#' @details `test_by_for_nps()` test for presence of the "by or for NPS" field. The test fails with an error if the information is missing from metadata. The test fails with a warning if the metadata indicate that the data were not created by or for the NPS (as this is expected to be relatively rare). Otherwise the test passes.
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
    cli::cli_abort(c("x" = "Metadata does not contain information about whether the data package was created by or for the NPS.  Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if(!missing_nps){
    if(nps == "TRUE"){
      cli::cli_inform(c(
        "v" = "Metadata states data was created by or for NPS."))
    }
    else if(nps == "FALSE"){
      cli::cli_warn(c(
        "!" = "Metadata states data was NOT created by or for NPS. Are you sure this is correct?  Use {.fn EMLeditor::set_publisher} to revise."))
    }
    else if(nps == "NULL"){
      cli::cli_abort(c("x" = 'Metadata indicates "by or for NPS" field set to NULL.  Use {.fn EMLeditor::set_publisher} to revise..'))
    }
  }
  return(invisible(metadata))
}


#' Tests EML metadata for the publisher name
#'
#' @details `test_publisher_name()` test for the presence of the data package publisher name. It fails with an error if the publisher name is missing, and fails with a warning if the publisher name is not "National Park Service" (as this is expected to be rare). Passes test if publisher name is "National Park Service"
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#'
#' @examples
#' \dontrun{
#' test_publisher_name()
#' }
test_publisher_name <- function(metadata = load_metadata(directory)) {
  is_eml(metadata)
  publisher_name <- metadata$dataset$publisher$organizationName
  missing_name <- is.null(publisher_name)
  if(missing_name){
    cli::cli_abort(c("x" = "Metadata does not contain the publisher name. Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if(!missing_name){
    if(publisher_name == "National Park Service"){
      cli::cli_inform(c(
        "v" = "Metadata indicates the publisher is the National Park Service."))
    }
    else if(publisher_name != "National Park Service"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher is {
                      crayon::bold$red(publisher_name)},
                      not the National Park Service. Are you sure?
                      Use {.fn EMLeditor::set_publisher} to revise."))
    }
  }
  return(invisible(metadata))
}


#' Test EML metadata for publisher state
#'
#' @description `test_publisher_state()` inspects the publisher address in EML metadata. The test faisl with an error if the administrativeArea (state) element is empty. The test fails with a warning if it is not "CO" (again, because this is expected to be exceedingly rare). The test passes if it is "CO".
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
    cli::cli_abort(c("x"= "Metadata does not contain the publisher state. Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if(!missing_state){
    if(pub_state == "CO"){
      cli::cli_inform(c("v" = "Metadata indicates the publisher state is CO."))
    }
    else if(pub_state != "CO"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher state, {
                      crayon::bold$red(pub_state)}, is not CO. Are you sure?
                       Use {.fn EMLeditor::set_publisher} to revise."))
    }
  }
  return(invisible(metadata))
}

#' Test EML metadata for publisher city
#'
#' @description `test_publisher_city()` inspects the publisher address in EML metadata. It fails with an an error if the city element is empty. It fails with a warning if it is not "Fort Collins". The test passes if it is "Fort Collins"
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
    cli::cli_abort(c("x" = "Metadata does not contain the publisher city.
                     Use {.fn EMLeditor::set_publisher} to revise."))
  }
  if(!missing_city){
    if(pub_city == "Fort Collins"){
      cli::cli_inform(c(
        "v" = "Metadata indicates the publisher city is Fort Collins."))
    }
    if(pub_city != "Fort Collins"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher city,
                      {crayon::bold$red(pub_city)}, is not Fort Collins.
                       Use {.fn EMLeditor::set_publisher} to revise."))
    }
  }
  return(invisible(metadata))
}


#' Test EML abstract
#'
#' @description `test_dp_abstract()` inspects EML for presence of a data package abstract. The test Fails with an error if the abstract is absent. If the abstract is present, the test fails with a warning if the abstract is <20 words, >250 words, or contains a subset of common characters that indicate improper formatting. Otherwise the test passes.
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
    cli::cli_abort(c("x" = "Metadata does not contain an abstract for the data package.
                     Use {.fn EMLeditor::set_abstract} to add one."))
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
    x <- sapply(stringr::str_count(abs, '\\w+'), sum)
    #x <- sapply(strsplit(abs, "" ), length)
    x <- sum(x) #sums up all words across potential multiple paragraphs
    z <- NULL #tracking for warnings
    if(x < 20){
      cli::cli_warn(c(
        "!" = "The data package abstract is less than 20 words.
        Are you sure this is informative enough?
        Use {.fn EMLeditor::set_abstract} to revise."))
      z <- "warn"
    }
    if(x > 250){
      cli::cli_warn(c(
        "!" = "The data package abstract is longer than 250 words.
        Consider using {.fn EMLeditor::set_abstract} and
        revising for a more concise abstract."))
      z <- "warn"
    }
    if(sum(stringr::str_detect(abs, "&amp;#13;"))>0){
      cli::cli_warn(c(
        "!" = "The data package abstract contains non-standard characters: {
                      crayon::bold$red('&amp;#13;')}.
        Use {.fn EMLeditor::set_abstract} to revise."))
      z <- "warn"
    }
    if(sum(grepl("[\r?\n|\r]", abs))>0){
      cli::cli_warn(c("!" = "The data package abstract contains non-standard end of line characters such as \\r, \\n, or \\n\\r.
                      Use {.fn EMLeditor::set_abstract} to revise."))
      z <- "warn"
    }
    if(sum(grepl("   ", abs))>0){
      cli::cli_warn(c(
        "!" = "The data package abstract contains extra (more than two in a row) spaces.
        Use {.fn EMLeditor::set_abstract} to revise."))
      z <- "warn"
    }
    if(length(seq_along(abs))>3){
      cli::cli_warn(c(
        "!" = "The data package abstract contains more than three paragraphs.
        Consider using {.fn EMLeditor::set_abstract} to revise."
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
#' @description `test_file_descript()` tests for the presence of file descriptions (entityDescription) fields. It fails with an error if any one or all of the entityDescription fields are empty in a dataTable. It fails with an error if any two or more file descriptions are identical. The test fails with a warning if for each file description that is longer than 15 words or shorter than three words. Otherwise the test passes.
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
        if(x > 15){
          cli::cli_warn(c("!" = "Data file {i} description is greater than 15 words. Consider a more concise description."))
        }
      }
    }
  }
}


#' Test for CUI dissemination code
#'
#' @description `test_cui_dissemination()` examines EML metadata for the presence of a Controlled Unclassified Information (CUI) dissemination code. The function fails with an an error if the code does not exist or does not match a list of valid codes. If the valid code is not "PUBLIC" the test will produce a warning. A valid code results in a pass.
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
    cli::cli_abort(c("x" = "Metadata does not contain a CUI dissemination code. Use {.fn EMLeditor::set_cui}."))
  }
  if(!is.null(diss_code)){
    if(!diss_code %in% valid_codes){
      cli::cli_abort(c("x" = "The CUI dissemination code {diss_code} is not a valid code. Use {.fn EMLeditor::set_cui}."))
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
#' @description `test_license()` examines the licenseName element of EML metadata. If there is no license name, the test fails with ab error. If the license name does not match a list of valid license names, the test fails. If the metadata contain a valid license name, but the license name and CUI dissemination code do not agree, the test fails with an error. Otherwise, the test passes.  Additionally, if the license name is not "Public Domain" or "CC0 1.0 Universal", the function will produce a warning that the data package is not public.
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
    cli::cli_abort(c("x" = "Metadata does not contain a license name. Use {.fn EMLeditor::set_int_rights} to add a license name."))
  }
  if(!is.null(license)){
    if(!license %in% license_list){
      cli::cli_abort(c(
        "x" = "The metadata does not contain a valid license name. Use {.fn EMLeditor::set_int_rights} to add a valid license name."))
    }
    if(license %in% license_list){
      diss_code <- arcticdatautils::eml_get_simple(metadata, "CUI")
      if(diss_code == "PUBLIC" & license == "No License/Controlled Unclassified Information"){
        cli::cli_abort(c(
          "x" = "Metadata license and CUI dissemination code do not agree. Use {.fn EMLeditor::set_int_rights} or {.fn EMLeditor::set_cui}."))
      }
      else if(diss_code != "PUBLIC" & license != "No License/Controlled Unclassified Information"){
        cli::cli_abort(c(
          "x" = "Metadata license and CUI dissemination code do not agree. Use {.fn EMLeditor::set_int_rights} or {.fn EMLeditor::set_cui}."))
      }
      else {
        cli::cli_inform(c("v" = "Metadata contains a valid license name."))
      }
      if(license == "No License/Controlled Unclassified Information"){
      cli::cli_warn(c("!" = "Metadata license name indicates that the data package is NOT public. Are you sure?"))
      }
    }
  }
  return(invisible(metadata))
}

#' Test for presence of Intellectual Rights
#'
#' @description `test_int_rights()` tests for the presence of text within the intellectualRights element in EML formatted metadata. If text if present, the test passes. Otherwise, the test fails. `test_int_rights()` makes no attempt to parse the text or test whether it properly coincides with the CUI dissemination codes or licenseName in the metadata. This is a simple presence/absence test.
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
    cli::cli_abort(c("x" = "Metadata lacks an Intellectual Rights statemet. Use {.fn EMLeditor::set_int_rights}."))
  }
  else {
    cli::cli_inform(c(
      "v" = "Metadata contains an Intellectual Rights statement."))
  }
  return(invisible(metadata))
}

#' Test metadata for attribute definitions
#'
#' @description `test_attribute_defs()` extracts all attributeNames and attributeDefinitions from EML metadata. It tests to make sure there are the same number of attributeNames and attributeDefinitions. If true, the test passes. If not, it fails with an error. This test ONLY looks at the metadata, it does NOT look at the data files. Passing this test does not mean that all data columns have attributes in the metadata associated with them. To test that, see `test_fields_match()`.
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
#' @description `test_storage_type()` checks to see if there are the same number of attributes (attributeName) and storageTypes in the metadata. Equal numbers of elements will pass; unequal numbers will fail the test with an error. `test_storage_type()` does NOT attempt to verify if the number of storageType elements matches the number of columns in the data package data files (for that functionality, use `test_fields_match()`).
#'
#' `test_storage_type()` verifies that the storageType is valid; i.e. is a member of an accepted list of possible storage types. Currently these are: string, float, date, factor, or characters. Validity for this test is based solely on observed ezEML/EAL output (in theory any string in storageType is schema-valid). Invalid storageTypes will result in a warning.
#'
#' `test_storage_type()` does NOT attempt to verify that the value in storageType logically matches the type data in the corresponding column.
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


#' Test creators for presence of an ORCiD
#'
#' @description `test_orcid_exists()` will inspect the metadata and test each creator listed as an individual person (individualName) but not creators that are organizations for the presence of an ORCiD. If an ORCiD is found for all individual creators, the test passes. If any individual creator lacks an ORCiD, the test fails with a warning and the users is pointed towards `EMLeditor::set_creator_orcids()` to add ORCiDs if they so choose.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_orcid_exists()
#' }
test_orcid_exists <- function(metadata = load_metadata(directory)){
  is_eml(metadata)
  #get creators
  creator <- metadata[["dataset"]][["creator"]]

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if(sum(names_list %in% names(creator)) > 0){
    creator <- list(creator)
  }

  # extract orcids and surNames
  surName <- NULL
  existing_orcid <- NULL
  for(i in seq_along(creator)){
    if("individualName" %in% names(creator[[i]])){
      #check for orcid directory id:
      last_name <- creator[[i]][["individualName"]][["surName"]]
      surName <- append(surName, last_name)
      orcid<-creator[[i]][["userId"]][["userId"]]
      if(is.null(orcid)){
        cli::cli_warn(c("!" = "Creator {last_name} lacks an ORCiD. To add an ORCiD, use {.fn EMLeditor::set_creator_orcids}.\n"))
      }
      else {
        existing_orcid <- append(existing_orcid, orcid)
      }
    }
  }
  if(identical(seq_along(surName), seq_along(existing_orcid))){
    cli::cli_inform(c("v" = "All individual creators have associated ORCiDs."))
  }
  return(invisible(metadata))
}



#' Test for ORCiD formatting (and presence)
#'
#' @description `test_orcid_format()` inspects metadata and looks for ORCiDs for each individual creator (not organizations listed as creators). If all individuals have correctly formatted ORCiDs (i.e a 19-character string such as xxxx-xxxx-xxxx-xxxx), the test passes. If any individual has in improperly formatted ORCiD, the test fails with an error. If there are no improperly formatted ORCiDs but one or more ORCiDs are missing, the test fails with a warning. Note that if there are imporperly formatted ORCiDs, the test will not inspect for presence/absence of individual ORCiDs. For a full accounting of which (if any) ORCiDs are missing (but no formatting check), use `test_orcid_exists`.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_orcid_format()
#' }
test_orcid_format <- function(metadata = load_metadata(directory)){
  is_eml(metadata)
  creator <- metadata[["dataset"]][["creator"]]

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if(sum(names_list %in% names(creator)) > 0){
    creator <- list(creator)
  }

  # extract orcids and surNames
  surName <- NULL
  existing_orcid <- NULL
  bad_orcids <- NULL
  for(i in seq_along(creator)){
    if("individualName" %in% names(creator[[i]])){
      #check for orcid directory id:
      last_name <- creator[[i]][["individualName"]][["surName"]]
      orcid<-creator[[i]][["userId"]][["userId"]]
      if(!is.null(orcid)){
        if(nchar(orcid) != 19){
          bad_orcids <- append(bad_orcids, last_name)
        }
        existing_orcid <- append(existing_orcid, orcid)
        surName <- append(surName, last_name)
      }
    }
  }
  if(is.null(bad_orcids) & !is.null(existing_orcid)){
    cli::cli_inform(c("v" = "All Creator ORCiDs are properly formatted.\n"))
  }
  else{
    if(!is.null(bad_orcids)){
      cli::cli_abort(c("x" = "{?ORCiD/ORCiDs} for {?creator/creators} {bad_orcids} {?is/are} improperly formatted. To reformat as xxxx-xxxx-xxxx-xxxx (do NOT include the http prefix), use {.fn EMLeditor::set_creator_orcids}.\n"))
    }
    #else {
    #  cli::cli_warn(c("!" = "{?ORCiD/ORCiDs} for {?Creator/Creators} {surName} {?is/are} imporperly formatted: {?ORCiD/ORiDs} missing. To add {?an ORCiD/ORCiDs}, use {.fn EMLeditor::set_creator_orcids}.\n"))
    #}
  }
  return(invisible(metadata))
}

#' Test whether supplied Creator ORCiDs resolve to a valid ORCiD profile
#'
#' @description `test_orcid_resolves()` will only examine ORCiDs that are supplied for individual Creators (not organizations). If the ORCiD supplied can be used to construct a URL that leads to a valid ORCiD profile, the test passes. If the ORCiD supplied cannot be used to construct a URL that resolves to a valid ORCiD profile - either because the ORCiD itself does not exist or because the ORCiD was supplied in an incorrect format, the test fails with an error. This test does not examine Creators that do not have associated ORCiDs; if no ORCiDs are provided that test does not return a pass or a fail.
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_orcid_resolves()
#' }
test_orcid_resolves <- function(metadata = load_metadata(directory)){
  is_eml(metadata)
  creator <- metadata[["dataset"]][["creator"]]
  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if(sum(names_list %in% names(creator)) > 0){
    creator <- list(creator)
  }
  # extract orcids and surNames
  surName <- NULL
  existing_orcid <- NULL
  for(i in seq_along(creator)){
    if("individualName" %in% names(creator[[i]])){
      #check for orcid directory id:
      orcid<-creator[[i]][["userId"]][["userId"]]
      #if there is an orcid, record the orcid and surName associated with it
      if(!is.null(orcid)){
        last_name <- creator[[i]][["individualName"]][["surName"]]
        surName <- append(surName, last_name)
        existing_orcid <- append(existing_orcid, orcid)
      }
    }
  }
  #if there are any orcids, record orcids that return a 404 or other web page error:
  if(!is.null(existing_orcid)){
    bad_orcid <- NULL
    for(i in seq_along(surName)){
      orcid_url <- paste0("https://orcid.org/", existing_orcid[i])
      if(httr::http_error(orcid_url)){
        bad_orcid <- append(bad_orcid, surName[i])
      }
    }

    if(is.null(bad_orcid)){
      cli::cli_inform(c("v" = "All Creator ORCiDs resolved to a valid ORCiD profile.\n"))
    }
    else {
      cli::cli_abort(c("x" = "{?Creator/Creators} {bad_orcid} {?had an/had} {?ORCiD/ORCiDs} that did not resolve to a valid ORCiD profile. Use {.fn EMLeditor::set_creator_orcids} to edit ORCiDs.\n"))
    }
  }
  return(invisible(metadata))
}


#' Tests whether metadata creator matches the ORCiD profile
#'
#' @description `test_orcid_match()` will only evaluate Creators that are individuals (not organizations). If an ORCiD has been supplied, the function will attempt to access the indicated ORCiD profile and test whether the last name indicated on the ORCiD profile matches the surName indicated in Metadata. If all surNames match the ORCiD profiles, the test passes. If any surName does not match the indicated ORCID profile, the test fails with an error.
#'
#' @details Potential reasons for failing this test having entered the wrong ORCiD into metadata, having improperly formatted the ORCiD in metadata (it should be listed as xxxx-xxxx-xxxx-xxxx - see `test_orcid_format()`), having set your ORCiD profile to "private" (in which case the function can't access the name associated with the profile) or differences between the ORCiD profile name and the name in metadata (such as maiden vs. married name, transposing given and surnames, or variation in surName spelling).
#'
#' @inheritParams test_pub_date
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_orcid_match()
#' }
test_orcid_match <- function(metadata = load_metadata(directory)){
  is_eml(metadata)
  creator <- metadata[["dataset"]][["creator"]]
  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if(sum(names_list %in% names(creator)) > 0){
    creator <- list(creator)
  }
  # extract orcids and surNames
  surName <- NULL
  existing_orcid <- NULL
  for(i in seq_along(creator)){
    if("individualName" %in% names(creator[[i]])){
      #check for orcid directory id:
      orcid<-creator[[i]][["userId"]][["userId"]]
      #if there is an orcid, record the orcid and surName associated with it
      if(!is.null(orcid)){
        surName <- append(surName,
                          creator[[i]][["individualName"]][["surName"]])
        existing_orcid <- append(existing_orcid, orcid)
      }
    }
  }

  #if there are any orcids, record orcids bad orcids:
  if(!is.null(existing_orcid)){
    bad_orcid <- NULL
    wrong_person <- NULL
    for(i in seq_along(surName)){
      orcid_url <- paste0("https://orcid.org/", existing_orcid[i])
      #api request to ORCID:
      test_req<-httr::GET(orcid_url)
      # if the status is good, check that the names match:
      status <- test_req$status_code
      if(status == 200){
        #munge api result:
        test_json <- httr::content(test_req, "text")
        test_rjson <- jsonlite::fromJSON(test_json)
        #pull last name from api result; if set to private == NULL
        last_name<-test_rjson$person$name$`family-name`$value

        #check whether surName in metadata matches last name on ORCiD profile:
        if(!identical(surName[i], last_name)){
          wrong_person <- append(wrong_person, surName[i])
        }
      }
      #if the status is bad, assume the profile is somehow bad/doesn't match
      if(status != 200){
        wrong_person <- append(wrong_person, surName[i])
      }
    }
  }
  if(exists("wrong_person")){
    if(is.null(wrong_person)){
      cli::cli_inform(c("v" = "All Creator ORCiDs resolve to an ORCiD profile that matches the Creator last name.\n"))
    }
    else{
      cli::cli_abort(c("x" = "{?Creator/Creators} {wrong_person} {?has/have} {?an ORCiD/ORCiD} {?profile/profiles} that {?does/do} not match the {?surName/surNames} in metadata. Use {.fn EMLeditor::set_creator_orcids} to edit ORCiDs in metadata, or go to {.url https://orcid.org} to make your ORCiD profile publicly visible.\n"))
    }
  }
  return(invisible(metadata))
}
