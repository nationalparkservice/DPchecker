% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/required_eml_elements.R
\name{test_publisher_state}
\alias{test_publisher_state}
\title{Test EML metadata for publisher state}
\usage{
test_publisher_state(metadata = load_metadata(directory))
}
\arguments{
\item{metadata}{The metadata object returned by \code{load_metadata}. If parameter is not provided, it defaults to calling \code{load_metadata} in the current project directory.}
}
\value{
invisibly returns \code{metadata}
}
\description{
\code{test_publisher_state()} inspects the publisher address in EML metadata. The test faisl with an error if the administrativeArea (state) element is empty. The test fails with a warning if it is not "CO" (again, because this is expected to be exceedingly rare). The test passes if it is "CO".
}
\examples{
\dontrun{
test_publisher_state()
}
}
