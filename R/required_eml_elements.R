#' Test publication date presence and ISO-8601 formatting
#'
#' @description tests for presence and ISO-8601 formatting of the publication date. Also tests whether the publication date is within a reasonable bounds; i.e. not a date prior to the beginning of the data package Reference Type (after 2022) and not in a future year.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata(DPchecker_example("BICY_Veg"))
#' test_pub_date(meta)
#' }
test_pub_date <- function(directory = here::here(),
                          metadata = load_metadata(directory)) {
  is_eml(metadata)  # Throw an error if metadata isn't an emld object
  missing_date <-is.null(metadata$dataset$pubDate)
  #error if no publication date found
  if(missing_date){
    cli::cli_alert_danger("Metadata does not contain publication date")
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
      cli::cli_alert_danger(c(
        "Publication year is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    if(!is.na(pub_year)){
      if(pub_year <= 2022){
      cli::cli_alert_danger(c("Publication date, ",
                              crayon::red$bold(pub_year),
                              ", predates the Data Package Reference Type"))
      }
      if(pub_year > curr_year){
      cli::cli_alert_danger(c("Publication date, ",
                              crayon::red$bold(pub_year),
                              ", exceeds the current year"))
      }
    }
    if(!identical(dash1, "-")){
      cli::cli_alert_danger(c(
        "Publication date sepearator, \"-\",",
        " is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    if(is.na(pub_month)){
      cli::cli_alert_danger(c(
        "Publication month is not in the correct ISO 8601 format (YYY-MM-DD"))
    }
    if(!is.na(pub_monty)){
      if(pub_month > 12){
        cli::cli_alert_danger(c(
          "Publication month, ", crayon::red$bold(pub_month), ",
          is >12 and is not in the correct ISO 8601 format (YYYY-MM-DD)"))
      }
    }
    if(!identical(dash2, "-")){
      cli::cli_alert_danger(c(
        "Publication date sepearator, \"-\",",
        " is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    if(is.na(pub_day)){
      cli::cli_alert_danger(c(
        "Publication day is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    if(!is.na(pub_day)){
      if(pub_day > 31){
      cli::cli_alert_danger(c(
        "Publication day, ", crayon::red$bold(pub_day),
        " is >31 and is not in the correct ISO 8601 format (YYYY-MM-DD)"))
      }
    }
    else {
      cli::cli_inform(c(
        "v" = "Publication date is present and in the correct ISO 8601 format"))
    }
  }
  return(invisible(metadata))
}

#' Test data package title
#'
#' @description tests EML metadata for presence of a data package title. Warns if the title is > 10 words.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_dp_title()
#' }
test_dp_title <- function(directory = here::here(),
                          metadata = load_metadata(directory)) {
  is_eml(metadata)  # Throw an error if metadata isn't an emld object
  title <- metadata$dataset$title
  missing_title <- is.null(title)
  #error if no publication title found
  if(missing_title){
    cli::cli_alert_danger("Metadata does not contain a data package title")
  }
  #if title present, check certain parameters:
  if(!missing_date){
    x <- sapply(strsplit(title, " "), length)
    if(x > 15){
      cli::cli_warn(c(
        "!" = "Data package title is >15 words. Consider a more concise title"))
    }
    if(x < 5){
      cli::cli_warn(c(
        "!" = "Data package title is <5 words.",
        " Consider a more informative title"))
    }
    else {
      cli::cli_inform(c(
        "v" = "Data package title is present in metadata"))
    }
  }
  return(invisible(metadata))
}

#' Test for "by or for NPS"
#'
#' @details test for presence of the "by or for NPS" field. Error if data are missing from metadata. Warns if the metadata indicate that the data were not created by or for the NPS (as this is expected to be relatively rare)
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#'
#' @examples
#' \dontrun{
#' test_by_for_nps()
#' }
test_by_for_nps <- function(directory = here::here(),
                            metadata = load_metadata(directory)) {
  is_eml(metadata)
  nps <- metadata$additionalMetadata$metadata$agencyOriginated$byOrForNPS
  missing_nps <- is.null(nps)
  #error if no by or for nps
  if(missing_nps){
    cli::cli_alert_danger(
      "Metadata does not contain information about whether the data package was created by or for the NPS")
  }
  if(!missing_nps){
    if(nps=="TRUE"){
      cli::cli_inform(c(
        "v" = "Metadata states data was created by or for NPS"))
    }
    else if(nps=="FALSE"){
      cli::cli_warn(c(
        "!" = "Metadata states data was NOT created by or for NPS"))
    }
    else if(nps=="NULL"){
      cli::cli_alert_danger(
        'Metadata indicates "by or for NPS" field set to NULL')
    }
  }
  return(invisible(metadata))
}


#' Tests EML metadata for the publisher name
#'
#' @details error if the publisher name is missing, warning if the publisher name is not "National Park Service". Passes test if publisher name is "National Park Service"
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#'
#' @examples
#' \dontrun{
#' test_publishername()
#' }
test_publisher_name <- function(directory = here::here(),
                                metadata = load_metadata(directory)) {
  is_eml(metadata)
  publisher_name <- metadata$dataset$publisher$organizationName
  missing_name <- is.null(publisher_name)
  if(missing_name){
    cli::cli_alert_danger("Metadata does not contain the publisher name")
  }
  if(!missing_name){
    if(publisher_name == "National Park Service"){
      cli::cli_inform(c(
        "v" = "Metata indicates the publisher is the National Park Service"))
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
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_publisher_state()
#' }
test_publisher_state <- function(directory = here::here(),
                                 metadata = load_metadata(directory)) {
  is_eml(metadata)
  pub_state <- metadata$dataset$publisher$address$administrativeArea
  missing_state <- is.null(pub_state)
  if(missing_state){
    cli::cli_alert_danger("Metadata does not contain the publisher state")
  }
  if(!missing_state){
    if(pub_state == "CO"){
      cli::cli_inform(c("v" = "Metadata indicates the publisher state is CO"))
    }
    else if(pub_state != "CO"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher state, {
                      crayon::bold$red(pub_state)}, is not CO"))
    }
  }
  return(invisible(metadata))
}

#' Test EML metadata for publisher city - made redundant by test_publisher
#'
#' @description Inspects the publisher address in EML metadata. Returns an error if the city element is empty. Returns a warning if it is not "Fort Collins". Passes test if it is "Fort Collins"
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_publisher_city()
#' }
test_publisher_city <- function(directory = here::here(),
                                metadata = load_metadata(directory)) {
  is_eml(metadata)
  pub_city <- metadata$dataset$publisher$address$city
  missing_city <- is.null(pub_city)
  if(missing_state){
    cli::cli_alert_danger("Metadata does not contain the publisher city")
  }
  if(!missing_state){
    if(pub_city == "Fort Collins"){
      cli::cli_inform(c(
        "v" = "Metadata indicates the publisher city is Fort Collins"))
    }
    if(pub_city != "Fort Collins"){
      cli::cli_warn(c("!" = "Metadata indicates the publisher city, {
                      crayon::bold$red(pub_city)}, is not Fort Collins"))
    }
  }
  return(invisible(metadata))
}


#' Test EML abstract
#'
#' @description inspects EML for presence of a data package abstract. FAils if abseent. If present, warns if the abstract is <20 words, >250 words, or contains a subset of common characters that indicate improper formatting.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_dp_abstract()
#' }
test_dp_abstract <- function(directory = here::here(),
                          metadata = load_metadata(directory)) {
  is_eml(metadata)
  abstract <- metadata$dataset$abstract
  missing_abstract <- is.null(abstract)
  if(missing_abstract){
    cli::cli_alert_danger(
      "Metadata does not contain an abstract for the data package")
  }
  if(!missing_abstract){
    x <- sapply(strsplit(title, " "), length)
    if(x < 20){
      cli::cli_warn(c(
        "!" = "The data package abstract is less than",
        "20 words. Are you sure this is informative enough?"))
    }
    if(x > 250){
      cli::cli_warn(c(
        "!" = "The data package abstract is longer than 250",
        " words. Consider revising for a more concise abstract"))
    }
    if(stringr::str_detect(abstract, "&amp;#13;")){
      cli::cli_warn(c(
        "!" = "The data package abstract contains non-standard characters: {
                      crayon::bold$red('&amp;#13;')}"))
    }
    if(grepl("[\r?\n|\r]", abstract)){
      cli::cli_warn(c(
        "!" = "The data package abstract contains non-standard end of line",
        " characters such as \\r, \\n, or \\n\\r"))
    }
    if(grepl("   ", abstract)){
      cli::cli_warn(c(
        "!" = "The data package abstract contains extra",
        " (more than two in a row) spaces"))
    }
    else{
      cli::cli_inform(c(
        "v" = "The Metadata contains a well formatted abstract",
        " for the data package"))
    }
  }
  return(invisible(metadata))
}

#' Test presence of file descriptions
#'
#' @description `test_file_descriptions()` tests for the presence of file descriptions (<entityDescription>) fields. It tests that there are no duplications. It warns if descriptions are longer than 10 words and shorter than three words.
#'
#' @param directory the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.
#' @param metadata The metadata object returned by `load_metadata`. If parameter not provided, defaults to calling `load_metadata` in current project directory.
#'
#' @return invisibly returns `metadata`
#' @export
#'
#' @examples
#' \dontrun{
#' test_file_descriptions()
#' }
test_file_descriptions <- function(directory = here::here(),
                                   metadata = load_metadata(directory)) {
  is_eml(metadata)
  # get dataTable and all children elements
  data_tbl <- EML::eml_get(metadata, "dataTable")
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }
  metadata_file_desc <- lapply(data_tbl, function(tbl)
    {arcticdatautils::eml_get_simple(tbl, "entityDescription")})
  if(is.null(metadata_file_desc)){
    cli::cli_alert_danger("Metadata does not contain data file descriptions (<entityDescription>")
  }
  if(!is.null(metadata_file_desc)){
    metadata_file_desc$`@context` <- NULL
    file_desc<-unlist(metadata_file_desc)


  }
}
