

#' Check metadata for PII (emails)
#'
#' @description `test_pii_meta_emails()` is a tool to help identify emails in metadata that may constitute Personally Identifiable Information (PII). This tool is not guaranteed to find all emails, nor can it definitely tell you whether an email constitutes PII or not. `test_pii_meta_emails()` reads in a *_metadata.xml file from the specified directory. It uses regular expressions to extract all emails (in truth, it's hard to test the regex against all possible emails so there is a chance it will miss one here or there). If there are no emails in the metadata, the function fails with a warning (there probably should be an email contact somewhere in the metadata). If there are any emails that end in anything other than .gov, the function fails with a warning and lists the offending emails. If the only emails in metadata end in .gov, these are assumed to be public emails and the function passes without listing out the emails.
#'
#'
#' @param directory String. The directory where the metadata file resides. Defaults to the current working directory.
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#' \dontrun{
#' test_pii_meta_emails()
#' }
test_pii_meta_emails <- function(directory = here::here()) {

  #get the full file names and paths for all files in directory
  files <- list.files(directory, full.names = TRUE)
  metadata <- NULL
  #Read in all metadata files as a single line
  for(file in files){
    if(grepl("metadata.xml", file)) {
      metadata <- append(metadata,
                         paste(suppressWarnings(readLines(file)),
                               collapse = " "))
    }
  }
  #extract emails from metadata lines:
  meta_emails <- suppressWarnings(
    regmatches(metadata,
               gregexpr("([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))", metadata)))
  if(!is.null(meta_emails)){
    meta_emails <- unlist(meta_emails, recursive = FALSE)
    personal_emails <- NULL
    #uggggggly email filtering:
    for(i in seq_along(meta_emails)){
      #filter out .govs
      if(!stringr::str_detect(meta_emails[i], ".gov")){
        personal_emails <- append(personal_emails, meta_emails[i])
      }
    }
    if(is.null(personal_emails)){
      cli::cli_inform(c("v" = "Metadata does not appear to contain any personal emails."))
    } else {
      cli::cli_warn(c("!" = "Metadata contains the following personal emails: {.email {personal_emails}}. Consider removing potential Personal Identifiable Information (PII)."))
    }
  }
  else{
    cli::cli_warn(c("!" = "Metadata does not appear to contain any emails. Are you sure this is correct?"))
  }
  return(invisible(metadata))
}


#' Examines the additionalInfo elment of EML metadata
#'
#' @description `test_notes()` extracts the additionalInfo components of EML metadata. These elements will be used to populate the "Notes" section on the DataStore landing page. If the Notes section is blank, the test fails with a warning. If the notes section contains non-standard characters (such as &amp;#13;) or more than two consecutive spaces, the test fails with a warning. Otherwise the test passes.  For all warnings, the user is advised to use `EMLeditor::set_additional_info()` to fix the additionalInfo section.
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
    z<-NULL
    if(sum(grepl("\r|&amp;#13;", info))>0){
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
#' @param metadata The metadata object returned by `load_metadata`. If parameter is not provided, it defaults to calling `load_metadata` in the current project directory.
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
    if(sum(grepl("\r|&amp;#13;", new_methods))>0){
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
#' @description `test_orcid_format()` inspects metadata and looks for ORCiDs for each individual creator (not organizations listed as creators). If all individuals have correctly formatted ORCiDs (i.e a 37-character string such as "https://orcid.org/xxxx-xxxx-xxxx-xxxx"), the test passes. This is a simple test that just looks at string length, not content. If any individual has in improperly formatted ORCiD, the test fails with an error. If there are no improperly formatted ORCiDs but one or more ORCiDs are missing, the test fails with a warning. Note that if there are improperly formatted ORCiDs, the test will not inspect for presence/absence of individual ORCiDs. For a full accounting of which (if any) ORCiDs are missing (but no formatting check), use `test_orcid_exists`.
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
        if(nchar(orcid) != 37){
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
      cli::cli_abort(c("x" = "{?ORCiD/ORCiDs} for {?creator/creators} {bad_orcids} {?is/are} improperly formatted. To reformat as https://orcid.org/xxxx-xxxx-xxxx-xxxx, use {.fn EMLeditor::set_creator_orcids}.\n"))
    }
    #else {
    #  cli::cli_warn(c("!" = "{?ORCiD/ORCiDs} for {?Creator/Creators} {surName} {?is/are} imporperly formatted: {?ORCiD/ORiDs} missing. To add {?an ORCiD/ORCiDs}, use {.fn EMLeditor::set_creator_orcids}.\n"))
    #}
  }
  return(invisible(metadata))
}

#' Test whether supplied Creator ORCiDs resolve to a valid ORCiD profile
#'
#' @description `test_orcid_resolves()` will only examine ORCiDs that are supplied for individual Creators (not organizations). If the ORCiD supplied consists of a URL that leads to a valid ORCiD profile, the test passes. If the ORCiD supplied is not a URL that resolves to a valid ORCiD profile - either because the ORCiD itself does not exist or because the ORCiD was supplied in an incorrect format, the test fails with an error. This test does not examine Creators that do not have associated ORCiDs; if no ORCiDs are provided that test does not return a pass or a fail.
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
      orcid_url <- existing_orcid[i]
      #test URL is valid:
      tryCatch({test_url <- httr::http_error(orcid_url)},
               error = function(e){}
      )
      #if http_error generates an error:
      if(!exists("test_url")){
        #if it isn't a valid url at all (e.g. just xxxx-xxxx-xxxx-xxxx)
        bad_orcid <- append(bad_orcid, surName[i])
      }
      else{
        if(test_url == TRUE){
          #valid URL, but results in 400 or above error:
          bad_orcid <- append(bad_orcid, surName[i])
        }
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
#' @details Potential reasons for failing this test include having entered the wrong ORCiD into metadata, having improperly formatted the ORCiD in metadata (it should be listed as https://orcid.org/xxxx-xxxx-xxxx-xxxx - see `test_orcid_format()`), having set your ORCiD profile to "private" (in which case the function can't access the name associated with the profile) or differences between the ORCiD profile name and the name in metadata (such as maiden vs. married name, transposing given and surnames, or variation in surName spelling).
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

  #if there are any orcids, record orcids & bad orcids:
  if(!is.null(existing_orcid)){
    bad_orcid <- NULL
    wrong_person <- NULL
    for(i in seq_along(surName)){
      orcid_url <- existing_orcid[i]
      is_it_na <- stringr::str_sub(orcid_url, start = -2)
      if(is_it_na == "NA") {
        next
      }
      #api request to ORCID:

      tryCatch({test_req <- httr::GET(orcid_url)},
               error = function(e){}
      )
      if(exists("test_req")){
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
      else{
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

#' Test for public GPS point coordinates
#'
#' @description `test_public_points()` will look for GPS point coordinates in metadata, if the data package is not public will warn users that they are potentially publishing confidential unclassified information (CUI). Specifically, if no GPS points are identified, the function passes. If the CUI dissemination code is set to "PUBLIC" and GPS points are identified, the function passes.  If the CUI dissemination code is not set to "PUBLIC" and GPS points are identified, the function will fail with a warning. If no CUI dissemination code is detected, the function fails with an error.
#'
#' @details The contents of metadata are public even if the data itself is restricted. This means that if GPS coordinates (a common type of data that must be redacted or fuzzed before it can be made public) exist in metadata, these GPS coordinates will be publicly available. The function will warn people of that potentiality. The function will only flag GPS points (not bounding boxes or polygons or other shapes). The function only checks for GPS points in the geographicCoverage element.
#'
#' @inheritParams test_pub_date
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#' \dontrun{
#' test_public_points()
#' }
test_public_points <- function(metadata = load_metadata(directory)){
  #check whether metadata is a properly formated EML document
  is_eml(metadata)
  #get geographic coverage element from metadata:
  geo_cov <- metadata[["dataset"]][["coverage"]][["geographicCoverage"]]
  #if htere is no geo_coverage, tell the user there's no potential CUI GPS points:
  missing_geo<-is.null(geo_cov)
  if(missing_geo){
    cli::cli_inform(c("v" = "No potentially confidential GPS points found in metadata"))
    return(invisible(metadata))
  }

  #drop `@context` item from geo_cov
  geo_cov$`@context` <- NULL

  # If there's only geographic coverage element, geo_cov ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("geographicDescription" %in% names(geo_cov)) {
    geo_cov <- list(geo_cov)
  }

  #place to store GPS points
  point_detect <- NULL

  #check whether points are detected (as opposed to lines or boxes)
  for(i in 1:length(seq_along(geo_cov))){
    lat_point <-
      geo_cov[[i]][["boundingCoordinates"]][["westBoundingCoordinate"]]== geo_cov[[i]][["boundingCoordinates"]][["eastBoundingCoordinate"]]
    long_point <-
      geo_cov[[i]][["boundingCoordinates"]][["southBoundingCoordinate"]] == geo_cov[[i]][["boundingCoordinates"]][["northBoundingCoordinate"]]

    #if a point is found, add it to the point_detect list:
    if(lat_point == TRUE & long_point == TRUE){
      point_detect <- append(point_detect, geo_cov[[i]][["geographicDescription"]])
    }
  }

  #if no GPS points in metadata, pass the test and inform user:
  if(is.null(point_detect)){
    cli::cli_inform(c("v" = "No GPS points detected in metadata"))
  } else {
    # if GPS coordinates are detected:
    # get the CUI designation
    cui <- arcticdatautils::eml_get_simple(metadata, "CUI")
    if(is.null(cui)){
      #if no CUI designation, fail the test and require designating CUI:
      cli::cli_abort(c("x" = "No CUI designation found. Unable to determine whether GPS points potentially contain CUI. Please use {.fn EMLeditor::set_cui} to designate a CUI dissemination category."))
    }
    #if there is a CUI designation....
    if(!is.null(cui)){
      #if CUI is set to public, pass the test and tell the user GPS coordinates will be public
      if(cui == "PUBLIC"){
        cli::cli_inform(c("v" = "CUI is set to PUBLIC and all GPS coordinates will be publicly available."))
      }
      else {
        # if CUI is not public, warn the user that GPS coordinates will be public.
        cli::cli_warn(c("!" = "CUI is not set to PUBLIC. GPS coordinates detected in metadata will be publicly available. Are you sure?"))
      }
    }
  }
  return(invisible(metadata))
}


#' Test for a DataStore project
#'
#' @inheritParams test_pub_date
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#'  \dontrun{
#' test_project()
#' }
test_project <- function (metadata = load_metadata(directory)) {
  #check whether EML is schema valid
  is_eml(metadata)
  #get project element
  proj <- metadata[["dataset"]][["project"]]

  #if there is no project information:
  missing_proj <- is.null(proj)
  if (missing_proj) {
    msg <- paste0("No project associated with the metadata. ",
                  "To add a DataStore project, use ",
                  "{.fun EMLeditor::set_project}.")
    cli::cli_warn(c("!" = msg))
    return(invisible(metadata))
  }

  #drop `@context` item from proj
  proj$`@context` <- NULL

  # If there's only project coverage element, proj ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  relist <- ("title" %in% names(proj) | "references" %in% names(proj))
  if (relist) {
    proj <- list(proj)
  }

  #if there are projects, but no DataStore project:
  proj_test <- unlist(proj)
  DS_proj <- "id" %in% names(proj_test)
  if (!DS_proj) {
    msg <- paste0("No DataStore project associated with the metadata. ",
                  "To add a DataStore project, use ",
                  "{.fun EMLeditor::set_project}.")
    cli::cli_warn(c("!" = msg))
    return(invisible(metadata))
  }

  msg <- "The metadata contains at least one DataStore Project reference."
  cli::cli_inform(c("v" = msg))
  return(invisible(metadata))
}

#' Test for content unit links
#'
#' @inheritParams test_pub_date
#'
#' @return invisible(metadata)
#' @export
#'
#' @examples
#'  \dontrun{
#' test_content_units()
#' }
test_content_units <- function(metadata = load_metadata(directory)) {
  #check whether EML is schema valid
  is_eml(metadata)

  #get geographic coverage from metadata
  geo_cov <- metadata[["dataset"]][["coverage"]][["geographicCoverage"]]

  #drop `@context` item from geo_cov
  geo_cov$`@context` <- NULL

  # If there's only geographic coverage element, geo_cov ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("geographicDescription" %in% names(geo_cov)) {
    geo_cov <- list(geo_cov)
  }

  #test for existence of geography; if absent warn and suggest solution
  if (is.null(geo_cov)) {
    msg1 <- "Metadata does not contain park content unit links."
    msg <- "to add content unit links."
    cli::cli_warn(
      c("!" = "{msg1}, Use {.fn EMLeditor::set_content_units} {msg}"))
      }

  #if geography is present:
  else {
    units <- FALSE
    for (i in 1:length(seq_along(geo_cov))) {
      #test for content unit links:
      if (grepl("NPS Content Unit Link:",
                geo_cov[[i]]$geographicDescription)) {
        units <- TRUE
      }
      # exit the for-loop after the first instance of a content unit:
      if (units == TRUE) {
        break
      }
    }
    # if content units are present, pass the test
    if (units) {
      cli::cli_inform(c("v" = "Metadata contains NPS content unit links."))
    }
    #if content units are not present, fail with a warning and suggest solution
    if (!units) {
      msg1 <- "Metadata does not contain park content unit links."
      msg <- "to add content unit links."
      cli::cli_warn(
        c("!" = "{msg1}, Use {.fn EMLeditor::set_content_units} {msg}"))

    }
  }

  return(invisible(metadata))
}
