% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_checks.R
\name{DPchecker_example}
\alias{DPchecker_example}
\title{Generate path to example data}
\usage{
DPchecker_example(dp_name = c("BICY_veg", "BUIS_herps"))
}
\arguments{
\item{dp_name}{Name of data package.}
}
\value{
Path to example data, if dp_name is specified.
}
\description{
Generate path to example data
}
\examples{
DPchecker_example()
DPchecker_example("BUIS_herps")
}
