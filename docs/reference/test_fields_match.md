# Test Matching Data/Metadata Fields

`test_fields_match()` compares the attributes of each dataTable within
the EML metadata to the columns in the corresponding .csv. If the
columns have the same names and order, the test passes. If the columns
differ, the test fails with an error.

## Usage

``` r
test_fields_match(
  directory = here::here(),
  metadata = load_metadata(directory)
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

## Value

Invisibly returns `metadata`.

## Details

test_fields_match briefly checks that data files match, but you should
really run [`test_file_name_match()`](test_file_name_match.md) before
you run this test.

## Examples

``` r
dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_fields_match(dir)
#> ✔ All columns in data match all columns in metadata.
```
