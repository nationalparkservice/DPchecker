# Footer Check

`test_footer()` checks the metadata files to determine whether data
files contain footer lines or not.

## Usage

``` r
test_footer(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

If footer lines are not present, the data package passes the test. If
footer lines are present, the data package fails the test with an error
and the user is instructed to remove footer lines prior to data package
upload. Currently only EML metadata are supported.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_footer(meta)
#> ✔ Metadata indicates data files do not have footers.
```
