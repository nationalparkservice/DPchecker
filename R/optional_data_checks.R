#' Check data files for PII (emails)
#'
#' @description `test_pii_data_emails()` is a tool to help identify emails in the data files (.csv) that may constitute Personally Identifiable Information (PII). This tool is not guaranteed to find all emails, nor can it definitely tell you whether an email constitutes PII or not. `test_pii_data_emails()` reads in each .csv file from the specified directory. It uses regular expressions to extract all emails (in truth, it's hard to test the regex against all possible emails so there is a chance it will miss one here or there). If there are any emails that end in anything other than .gov, the function fails with a warning and lists the offending files and the emails they contain. If there are no emails in the data files or the only emails in the data files end in .gov, these are assumed to be public emails and the function passes without listing any emails.
#'
#'
#' @param directory String. The directory where the data package resides. Defaults to the current working directory.
#'
#' @return String
#' @export
#'
#' @examples
#' \dontrun{
#' test_pii_data_emails()
#' }
test_pii_data_emails <- function(directory = here::here()) {

  #get the full file names and paths for all files in directory
  files <- list.files(directory, full.names = TRUE)

  emails_list <- NULL #list of lists containing offending emails.
  #populate emails_list with pii emails:
  for(file in files){
    data_emails <- NULL #holds  emails
    email_files <- NULL #holds names of files that contain emails
    personal_emails <- NULL # holds offending emails
    if(grepl(".csv", file, ignore.case = TRUE)) {
      #read data line by line, concatenate lines (emails split across a lines)
      data_lines <- paste(readLines(file), collapse = " ")
      #for each csv file, extract all emails and add them to file_emails
      data_emails <- suppressWarnings(
        regmatches(data_lines,gregexpr("([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))", data_lines)))
      data_emails <- unlist(data_emails, recursive = FALSE)

      #if a file contains emails, check for offending emails:
      if(length(seq_along(data_emails)) > 0){
        for(i in seq_along(data_emails)){
          #filter out .govs
          if(!stringr::str_detect(data_emails[i], ".gov")){
            personal_emails <- append(personal_emails, data_emails[i])
          }
        }
      }
      if(!is.null(personal_emails)){
        pii_emails <- list(personal_emails)
        names(pii_emails) <- basename(file)
        emails_list<-append(emails_list, pii_emails)
      }
    }
  }
  #if there are offending emails, fail with warning:
  if(!is.null(emails_list)){
    msg <- paste0("--> {.file ", names(emails_list), "}: ", unlist(emails_list))
    names(msg) <- rep(" ", length(msg))
    err <- paste0("The following data files contain emails with potential PII:")

    cli::cli_warn(c("!" = err, msg))
  #if no pii emails (non .gov), pass test:
  } else {
    cli::cli_inform(
      c("v" = "Data files do not appear to contain any personal emails."))
  }
  #return(invisible(metadata))
}
