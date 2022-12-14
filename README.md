

# DPchecker

#### v0.0.0.9000

<!-- badges: start -->
  [![R-CMD-check](https://github.com/nationalparkservice/DPchecker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nationalparkservice/DPchecker/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/nationalparkservice/DPchecker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nationalparkservice/DPchecker/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

DPchecker (Data Package checker) is a package with a series of functions for NPS data package authors and reviewers to check for internal consistency among data/meta data and with the data package standards.

Currently, *only EML metadata and .csv data files* are supported. All data files and the single metadata file (named *_metadata.xml) must be in the same directory.

Each function runs an individual test and imports data or metadata as necessary. At some point these functions will be inserted in to a wrapper that will run all the tests and import data/metadata only once.

## Installation
You can install the development version of DPchecker from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nationalparkservice/DPchecker")
```

## Examples:
Check whether your metadata are schema-valid:

``` r
library(DPchecker)

# If you set your working directory to the directory with your data package data files (including metadata):

test.validateSchema()

# Alternatively, you can supply the path to the data package data files (including metadata) without changing your working directory:

test.validateSchema("C:/user/documents/mydatapackage")
```
