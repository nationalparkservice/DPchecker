# Test Numeric Fields

`test_numeric_fields()` verifies that all columns listed as numeric in
the metadata are free of non-numeric data. If non-numeric data are
encountered, the test fails with an error.

## Usage

``` r
test_numeric_fields(
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

"NA" and missing data codes documented in the metadata will *not* cause
this test to fail. Note that this test assumes that the column types
that are in the metadata are the intended types, i.e., if your metadata
says a column is text and it should actually be numeric, it will not be
caught by this test. On the other hand, if your metadata indicates a
text column is numeric, the function will generate an error.

## Examples

``` r
dir <- DPchecker_example("BICY_veg")
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_numeric_fields(dir)
#> ✔ Columns indicated as numeric in metadata contain only numeric values and
#>   valid missing value codes.
```
