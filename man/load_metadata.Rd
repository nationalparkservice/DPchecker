% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabular_data_congruence.R
\name{load_metadata}
\alias{load_metadata}
\title{Load Metadata}
\usage{
load_metadata(directory = here::here(), inform_success = FALSE)
}
\arguments{
\item{directory}{the directory where the metadata file is found - i.e. your data package. Defaults to your current project directory.}

\item{inform_success}{Boolean indicating whether to display a message when metadata is successfully loaded.}
}
\value{
an R-object formatted as EML metadata.
}
\description{
\code{load_metadata()} loads the metadata file from a given path or directory.
}
\details{
Given a path or directory - default is the working directory -  \code{load_metadata()} looks for files with the ending *_metadata.xml. The function quits with an error and tells the user if no such files are found or if more than one such file is found. If only one metadata file is found, it is checked for one of 3 formats: FGDC, ISO, or EML. Currently only EML is supported and the function will fail with an error, inform the user, and quit if non-EML metadata is found. The EML metadata file is loaded into R's work space for future use during congruence checking.

In the context of National Park Service data packages, this function can be slightly easier to use for loading metadata into R than \code{EML::read_eml()} because it does require a filename or type be specified.
}
\examples{
data_pkg_dir <- DPchecker_example("BICY_veg")
my_metadata <- load_metadata(data_pkg_dir)

}
