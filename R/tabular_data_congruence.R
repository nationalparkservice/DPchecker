#' Load Metadata
#' 
#' @description loads the metadata file from a given path or directory.
#' 
#' @details given a path or directory (default is the working directory) loadMetadata looks for files with the ending *_metadata.xml. The function quits and warns the user if no such files are found or if more than one such file is found. If only one \*_metadata.xml file is found, it checked for one of 3 formats: FGDC, ISO, or EML. Currently only EML is supported and the function will warn the user and quit if non-EML metadata is found. The EML metadata file is loaded into R's work space for future use during congruence checking.
#'
#' @param directory the directory where the metadata file is found (i.e. your data package). Defaults to the current working directory. On exit, returns you to the current working directory.
#'
#' @return an R-object formatted as EML metadata.
#' @export
#'
#' @examples 
#' my_metadata<-loadMetadata()
#' 
loadMetadata <- function(directory=getwd()){
  #catch and store current working directory
  orig_wd <- getwd()
  #reset the working directory to the original directory when function is exited
  on.exit(setwd(orig_wd))
  #switch to directory where metadata should be
  setwd(directory) 
  #get list of all files ending in metadata.xml
  lf<-list.files(pattern="metadata.xml")
  
  #if metadata file exists, stop the function and warn the user
  if(length(lf) < 1){
    stop("No metadata found. Your metadata file name must end in _metadata.xml")
  }
  #if multiple metadata files exists, stop the function and warn the user
  if(length(lf) > 1){
    stop("The data package format only allows one metadata file per data package. Please remove extraneous metadata files or combine them into a single file.")
  }
  
  #if exactly 1 metadata file exists, determine what format the metadata file is. Accept only EML (for now):
  if(length(lf)==1){
    if(sum(grepl("<metstdv>FGDC-STD-001-1998", readLines(lf)))>0){
      metaformat<-"fgdc"
      stop(paste0("\nCongruence checking is not yet supported for ", metaformat, " metadata."))
    }
    else if(sum(grepl("schemas.isotc211.org/19115", readLines(lf)))>0){
      metaformat<-"ISO19915"
      stop(paste0("\nCongruence checking is not yet supported for ", metaformat, " metadata."))
    }
    else if(sum(grepl("<eml:eml", readLines(lf)))>0){
      metaformat<-"eml"
      #writeLines(paste0("\nYou are working with ", metaformat, " metadata"))
      #load metadata
      metadata<-EML::read_eml(lf, from="xml")
    }
  }
    #return metadata to the the workspace
    return(metadata)
}


#' Load Data
#'
#' @description loadData inspects the working directory for data files. Loads all existing data files into a tibble. 
#' 
#' @details loads all data files in a specified directory (default is the working directory) into a tibble for later use in congruence checking. Returns the user to the working directory upon exit. Currently only supports .csv files. 
#' @param directory the directory where the data file(s) are found (i.e. your data package). Defaults to the current working directory. On exit, returns to the current working directory.
#'
#' @return a tibble of .csvs
#' @export
#'
#' @examples my_data<-loadData()
loadData<-function(directory=getwd()){
  orig_wd<-getwd()
  on.exit(setwd(orig_wd))
  setwd(directory)
  lf<-list.files(pattern=".csv")
  names<-gsub(pattern="\\.csv$", "", lf)
  lf<-list.st<-list()

  for(i in lf){
    filepath<-file.path(getwd(),i)
    tibble_List[[i]]<-assign(i, readr::read_csv(filepath, show_col_types=FALSE))
  }
  return(tibble_List)
}



#' EML Version Check
#' 
#' @description metadataVersion determines whether the version of the metadata supplied meets the current criteria for an NPS data package.
#' 
#' @details currently only EML is supported. EML must be version >= 2.2.0. 
#'
#' @param emlObject an R object containing EML metadata from a EML formatted .xml file loaded using either loadMetatdata() or EML::read_eml(<filename>, from="xml")
#'
#' @return message
#' @export
#'
#' @examples
#' metadataVersion(my_metadata)
metadataVersion<-function(emlObject){
  vers<-substr(sub(".*https://eml.ecoinformatics.org/eml-", "", emlObject), 1, 5)[1]
  vers<-numeric_version(vers)
  if(vers<"2.2.1"){
    message("EML version: Error - must be 2.2.0 or later")
  }
  if(vers>="2.2.0"){
    message("EML version: Valid")
  }
}


#' Validate Metadata Schema
#' 
#' @description validateSchema inspects a metadata object loaded into R and determines whether it is schema-valid.
#' 
#' @details currently, only EML is supported. For now this is just a wrapper form EML::eml_validate().
#'
#' @param emlObject 
#'
#' @return
#' @export
#'
#' @examples
validateSchema<-function(emlObject){
  EML::eml_validate(emlObject)
}




    
    
    
  