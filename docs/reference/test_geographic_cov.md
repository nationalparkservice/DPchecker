# Check for Geographic Coverage

`test_geographic_cov()` checks if geographic coverage element is present
in metadata. It does not perform any validation of geographic coverage
information. If the geographicCoverage element is present, the test
passes. If it is absent, the test fails with a warning.

## Usage

``` r
test_geographic_cov(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

## Value

Invisibly returns `metadata`.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_geographic_cov(meta)
#> ✔ Metadata contains geographic coverage element
```
