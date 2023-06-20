

# DPchecker

#### v0.3.1

DPchecker (Data Package checker) is a package with a series of functions for NPS data package authors and reviewers to check for internal consistency among data/meta data and with the data package standards.

Currently, *only EML metadata and .csv data files* are supported. It is recommended that you store all data files and the single metadata file (filename must end in "metadata.xml") in the same directory.

Each function runs an single check and imports data or metadata as necessary. If you prefer to run all checks at once, you can use `run_congruence_checks()`.

## Installation
You can install the development version of DPchecker from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nationalparkservice/DPchecker")
```

## Examples:
Run all checks at once:

``` r
library(DPchecker)

# Get the directory where example data is stored (alternately, replace this with the path to your data folder)

dir <- DPchecker_example("BICY_veg")  # Use this to test things out with the included example data
# dir <- "C:/Users/yourusername/Documents/my_data_package"  # The path to your data package should look something like this

# Run all checks and summarize results

run_congruence_checks(dir)

# Alternately, if your data package is stored in the root of your R project folder, you don't need to pass any arguments
run_congruence_checks()
```

Verify that file names and column names in the metadata match the data:

``` r
library(DPchecker)

dir <- DPchecker_example("BICY_veg")  # Use this to test things out with the included example data
# dir <- "C:/Users/yourusername/Documents/my_data_package"  # The path to your data package should look something like this

test_file_name_match(dir)
test_fields_match(dir)
```
