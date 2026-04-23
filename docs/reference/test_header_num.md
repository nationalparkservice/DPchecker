# Header Check

`test_header_num()` checks the metadata files to ensure that each data
file contains exactly one header row.

## Usage

``` r
test_header_num(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

`test_header_num()` examines the numHeaderLines element from EML
(currently only EML is supported) metadata to determine how many header
rows there are. If there are no header rows or if there is more than one
header row, the test fails with an error. The test also fails with an
error if there is no information about the number of header rows.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_header_num(meta)
#> ✔ Metadata indicates that each data file contains exactly one header row.
```
