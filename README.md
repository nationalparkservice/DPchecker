# DPCchecker
DPCchecker (Data Package Congruence checker) is a package with a series of functions for NPS data package authors and reviewers to check for internal consistency among data/meta data and with the data package standards.


Currently, only EML metadata and .csv data files are supported. All data files and the single metadata file (named *_metadata.xml) must be in the same directory.

Currently, each function runs an individual test and imports data or metadata as necessary. At some point these functions will be inserted in to a wrapper that will run all the tests and import data/metadata only once.

# Installation
You can install the development version of DPchecker from Github with:

#install.packages("devtools")
devtools::install_github("nationalparkservice/DPChecker")

#examples
Check whether your metadata are schema-valid:

If you set your working directory to the directory with your data package data files and metadata file:

test.metadataVersion()

Alternatively, you can supply the path to the data package data files and metadata without changing your working directory:

test.metadataVersion("C:/user/documents/mydatapackage")
