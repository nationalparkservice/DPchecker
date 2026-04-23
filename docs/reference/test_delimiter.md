# Field Delimiter Check

`test_delimiter()` checks the metadata file and ensures that each data
file has a field delimiter with exactly one character (e.g. ", ").

## Usage

``` r
test_delimiter(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

`test_delimiter()` examines the fieldDelimiter element from EML
(currently only EML is supported) metadata to determine how many
characters there are. If there is no fieldDelimiter element, the test
returns an error. If the field delimiter is anything other than exactly
one character in length, the test returns an error.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_delimiter(meta)
#> ✔ Metadata indicates that each data file contains a field delimiter that is a
#>   single character
```
