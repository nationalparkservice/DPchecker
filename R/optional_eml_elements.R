#' Examines the additionalInfo elment of EML metadata
#'
#' @description `test_notes()` extracts the additionalInfo components of EML metadata. These elements will be used to populate the "Notes" section on the DataStore landing page. If the Notes section is blank, the test fails with a warning. If the notes section contains non-standard characters (such as &amp;#13;) or more than two consecuitive spaces, the test fails with a warning. Otherwise the test passes.  For all warnings, the user is advised to use `EMLeditor::set_additional_info()` to fix the additionalInfo section.
#'
#' @param metadata The metadata object returned by `load_metadata`. If parameter is not provided, it defaults to calling `load_metadata` in the current project directory.
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#' \dontrun{
#' test_notes()
#' }
test_notes <- function(metadata = load_metadata(directory)){
  is_eml(metadata)
  notes <- metadata$dataset$additionalInfo
  missing_notes <- is.null(notes)
  if(missing_notes){
    cli::cli_warn(c("!" = "Metadata does not contain additionalInfo (notes).
                     Use {.fn EMLeditor::set_additional_info} to add notes."))
  }
  if(!missing_notes){
    #get rid of <para> tags, empty paragraphs, etc
    notes <- unlist(notes)
    info<-NULL
    for(i in seq_along(notes)){
      if(nchar(notes[i])>0){
        info <- append(info, notes[[i]])
      }
    }
    if(sum(grepl("[\r|&amp;#13;]", info))>0){
      cli::cli_warn(c("!" = "The metadata additionalInfo (\"Notes\" on DataStore) contains non-standard characters such as \\r or &amp;#13;.
                      Use {.fn EMLeditor::set_additional_info} to revise."))
      z <- "warn"
    }
    if(sum(grepl("   ", info))>0){
      cli::cli_warn(c(
        "!" = "The data package additionalInfo (\"Notes\" on DataStore) contains extra (more than two in a row) spaces.
        Use {.fn EMLeditor::set_additional_info} to revise."))
      z <- "warn"
    }
    #if no other warnings generated:
    if(is.null(z)){
      cli::cli_inform(c(
        "v" = "The Metadata contains a well formatted additionalInfo (\"Notes\" on DataStore) section."))
    }
  }
  return(invisible(metadata))
}


#' Examines the Methods section of EML
#'
#' @description `test_methods()` first extracts the methods from an EML object. If methods are not present, the test fails with an error. If the methods are present, the tests asks 1) Is the section longer than 20 words? If not, the test fails with a warning.  2) Does the methods section contain unconventional characters such as &amp;#13;? If so, the test fails with a warning. 2) Does the methods section contain additional spaces (more than two consecuitive spaces)? If so, the test fails with a warning. If all of the tests pass, the test as whole passes. For any error or warning, users are advised to use `EMLeditor::set_methods()` to correct the problem.
#'
#' @param metadata
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#' \dontrun{
#' test_methods()
#' }
test_methods <- function(metadata = load_metadata(directory)){
  is_eml(metadata)
  method <- metadata$dataset$methods
  missing_methods <- is.null(method)
  if(missing_methods){
    cli::cli_abort(c("x" = "Metadata does not contain a methods. Use {.fn EMLeditor::set_methods} to add methods."))
  }
  if(!missing_methods){
    #get rid of <para> tags, empty paragraphs, etc
    method <- unlist(method)
    new_methods<-NULL
    for(i in seq_along(method)){
      if(nchar(method[i])>0){
        new_methods <- append(new_methods, method[[i]])
      }
    }
    x <- sapply(stringr::str_count(new_methods, '\\w+'), sum)
    #x <- sapply(strsplit(abs, "" ), length)
    x <- sum(x) #sums up all words across potential multiple paragraphs
    z <- NULL #tracking for warnings
    if(x < 20){
      cli::cli_warn(c(
        "!" = "The methods section is is less than 20 words.
        Are you sure this is informative enough?
        Use {.fn EMLeditor::set_methods} to revise."))
      z <- "warn"
    }
    if(sum(grepl("[\r|&amp;#13;]", new_methods))>0){
      cli::cli_warn(c("!" = "The metadata methods contains non-standard characters such as \\r or &amp;#13;. Use {.fn EMLeditor::set_methods} to revise."))
      z <- "warn"
    }
    if(sum(grepl("   ", new_methods))>0){
      cli::cli_warn(c(
        "!" = "The metadata methods contains extra (more than two in a row) spaces.
        Use {.fn EMLeditor::set_methods} to revise."))
      z <- "warn"
    }
    #if no other warnings generated:
    if(is.null(z)){
      cli::cli_inform(c(
        "v" = "The Metadata contains a well formatted methods section."))
    }
  }
  return(invisible(metadata))
}
