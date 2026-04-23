# Checks for consistency in data file URLs

`test_datatable_urls` Checks to make sure that the URLs listed for data
files correctly correspond to the DOI in metadata. If the last 7 digits
of the URL for all data tables is identical to the last 7 digits of the
DOI, the test passes. If there is no DOI, the test fails with a warning.
If a data table lacks a URL, the test fails with an error. If any data
table URL's last 7 digits are not identical to the DOI's last 7 digits,
the test fails with an error.

## Usage

``` r
test_datatable_urls(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

invisible(metadata)

## Details

suggestions of which functions to use to correct errors/warnings are
provided.

## Examples

``` r
if (FALSE) { # \dontrun{
dir <- DPchecker_example("BICY_veg")
test_datatable_urls(dir)
} # }
```
