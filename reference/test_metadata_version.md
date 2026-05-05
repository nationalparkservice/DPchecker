# EML Version Check

`test_metadata_version()` determines whether the version of the metadata
supplied meets the current criteria for an NPS data package.

## Usage

``` r
test_metadata_version(metadata = load_metadata(here::here()))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Details

currently only EML is supported. EML must be version \>= 2.2.0.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_metadata_version(meta)
#> ✔ Your EML version is supported.
```
