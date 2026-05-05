# Test Date Range

`test_date_range()` verifies that dates in the dataset are consistent
with the date range in the metadata. It is HIGHLY recommended that you
provide dates and times in ISO-8601 formatting: YYYY-MM-DDThh:mm:ss (if
you don't have time you can us just the YYYY-MM-DD component).

## Usage

``` r
test_date_range(
  directory = here::here(),
  metadata = load_metadata(directory),
  skip_cols = NA
)
```

## Arguments

- directory:

  the directory where the data file(s) are found (i.e. your data
  package). Defaults to the current working directory. On exit, returns
  to the current working directory.

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

- skip_cols:

  String. Defaults to NA. One or more columns to omit from the
  test_date_range function.

## Value

Invisibly returns `metadata`.

## Details

This function checks columns that are identified as date/time in the
metadata. If the metadata lacks a date range, the function fails with a
warning. It fails with an error if the dates contained in the columns
are outside of the temporal coverage specified in the metadata. If the
date/time format string specified in the metadata does not match the
actual format of the date in the CSV, it will likely fail to parse and
result failing the test with an error. Failure to parse is indicated in
the results with the text "(failed to parse)".

This test will also inform the user which file and columns are causing
the test to fail and how it is failing (i.e. outside of the date range
or failed to parse).

If the date columns that are causing the test to fail are associated
with the QA/QC process and are expected to fall outside the date range
specified for the data, these columns can be omitted from the test using
skip_cols.

## Examples

``` r
dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_date_range(dir)
#> ✔ Columns indicated as date/time in metadata are within the stated temporal
#>   coverage range.
```
