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
#' meta <- load_metadata(DPchecker_example("BICY_Veg"))
#' test_pub_date(meta)
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
    pub_year <- as.numeric(substr(date,1,4))
    curr_year <- as.numeric(substr(Sys.Date(),1,4))
    dash1 <- substr(date,5,5)
    pub_month <-as.numeric(substr(date,6,7))
    dash2 <- substr(date,8,8)
    pub_day <- as.numeric(substr(date,9,10))
      #test for improbable years or non-numerics in year:
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
    if(is.na(pub_year)){
      cli::cli_alert_danger(c(
        "Publication date is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    else if(!identical(dash1, "-")){
      cli::cli_alert_danger(c(
        "Publication date is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    else if(pub_month > 12 | is.na(pub_month)){
      cli::cli_alert_danger(c(
        "Publication date is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    else if(!identical(dash2, "-")){
      cli::cli_alert_danger(c(
        "Publication date is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    else if(pub_day > 31 | (is.na(pub_day))){
      cli::cli_alert_danger(c(
        "Publication date is not in the correct ISO 8601 format (YYYY-MM-DD)"))
    }
    else {
      cli::cli_inform(c(
        "v" = "Publication date is present and in the correct ISO 8601 format"))
    }
  }
}





