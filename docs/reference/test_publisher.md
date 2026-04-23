# Check for Publisher

`test_publisher()` checks if publisher information is present in
metadata, with option to require valid NPS publisher information. If the
publisher information is present, the test passes. If the publisher
information is absent, the test fails with an error. If require_nps is
set to TRUE (defaults to FALSE), the test will also ensure that a valid
NPS publisher information is present. In this case, even if the
publisher element is present, the test will fail with an error unless
the publisher is the NPS (and all the publisher fields exactly match the
expected information for NPS data packages).

## Usage

``` r
test_publisher(metadata = load_metadata(directory), require_nps = FALSE)
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter not
  provided, defaults to calling `load_metadata` in current project
  directory.

- require_nps:

  If TRUE, throw an error if publisher information is not correct for
  NPS published data.

## Value

Invisibly returns `metadata`.

## Examples

``` r
meta <- load_metadata(DPchecker_example("BICY_veg"))
#> Data are provided for example use only. Do not assume that they are complete, accurate, or up to date.
test_publisher(meta)
#> ✔ Metadata contains publisher element.
```
