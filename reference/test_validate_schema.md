# Validate Metadata Schema

`test_validate_schema()` inspects a metadata object loaded into R and
determines whether it is schema-valid. If the test fails, the functio
produces an error message.

## Usage

``` r
test_validate_schema(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

currently, only EML is supported. For now this is just a wrapper form
EML::eml_validate().

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_validate_schema(meta)
#> ✔ Your metadata is schema valid.
```
