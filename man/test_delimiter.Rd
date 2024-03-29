% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabular_data_congruence.R
\name{test_delimiter}
\alias{test_delimiter}
\title{Field Delimiter Check}
\usage{
test_delimiter(metadata = load_metadata(here::here()))
}
\arguments{
\item{metadata}{The metadata object returned by \code{load_metadata}. If parameter not provided, defaults to calling \code{load_metadata} in current project directory.}
}
\value{
Invisibly returns \code{metadata}.
}
\description{
\code{test_delimiter()} checks the metadata file and ensures that each data file has a field delimiter with exactly one character (e.g. ", ").
}
\details{
\code{test_delimiter()} examines the fieldDelimiter element from EML (currently only EML is supported) metadata to determine how many characters there are. If there is no fieldDelimiter element, the test returns an error. If the field delimiter is anything other than exactly one character in length, the test returns an error.
}
\examples{
meta <- load_metadata(DPchecker_example("BICY_veg"))
test_delimiter(meta)
}
