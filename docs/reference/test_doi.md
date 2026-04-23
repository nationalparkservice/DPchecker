# Check for presence of a Digital Object Identifier

`test_doi()` checks whether a DOI for the data package is present in
metadata. It does not currently validate DOI. If a DOI is present, the
test passes. If a DOI is not present, the test fails with a warning.

## Usage

``` r
test_doi(metadata = load_metadata(directory))
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
test_doi(meta)
#> ✔ Metadata contains a digital object identifier, "doi:
#>   https://doi.org/10.57830/2295086".
```
